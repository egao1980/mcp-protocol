(in-package #:mcp-protocol/tests)

(defun %echo-server ()
  (let ((server (make-instance 'mcp-protocol:mcp-server
                               :name "test-server" :version "0.1.0"
                               :instructions "dual-era fixture")))
    (mcp-protocol:register-tool
     server
     (mcp-protocol:make-mcp-tool
      "echo" :description "echo msg"
      :input-schema (mcp-protocol:json-object "type" "object")
      :handler (lambda (args)
                 (mcp-protocol:tool-result
                  (list (mcp-protocol:make-text-content
                         (or (mcp-protocol:param args "msg") "")))))))
    (mcp-protocol:register-resource
     server
     (mcp-protocol:make-mcp-resource
      "memo://hi" :name "hi" :handler (lambda (res)
                                        (declare (ignore res))
                                        "hello")))
    (mcp-protocol:register-prompt
     server
     (mcp-protocol:make-mcp-prompt "greet" :description "say hi"))
    server))

(defun %wired (&key (era :unknown))
  (let* ((server (%echo-server))
         (transport (rpc-backend-inprocess:make-inprocess-rpc-transport))
         (client (make-instance 'mcp-protocol:mcp-client
                                :transport transport
                                :era era
                                :name "test-client"
                                :version "0.1.0")))
    (mcp-protocol:serve-mcp server :transport transport)
    (values client server)))

(deftest classes-exist
  (ok (find-class 'mcp-protocol:mcp-server))
  (ok (find-class 'mcp-protocol:mcp-client))
  (ok (find-class 'mcp-protocol:mcp-backend))
  (ok (equal "2026-07-28" mcp-protocol:+mcp-protocol-version+))
  (ok (equal "2025-11-25" mcp-protocol:+mcp-legacy-protocol-version+))
  (ok (equal -32022 mcp-protocol:+mcp-error-unsupported-protocol-version+)))

(deftest local-catalog
  (let ((server (%echo-server)))
    (ok (= 1 (length (mcp-protocol:list-tools server))))
    (let ((result (mcp-protocol:call-tool server "echo"
                                          (mcp-protocol:json-object "msg" "hi"))))
      (ok (hash-table-p result))
      (ok (null (gethash "isError" result))))
    (ok (search "hello"
                (gethash "text"
                         (elt (gethash "contents"
                                       (mcp-protocol:read-resource server "memo://hi"))
                              0))))))

(deftest modern-discover-and-tools
  (multiple-value-bind (client server)
      (%wired)
    (declare (ignore server))
    (let ((disc (mcp-protocol:mcp-initialize client)))
      (ok (eq :modern (mcp-protocol:mcp-client-era client)))
      (ok (equal "complete" (gethash "resultType" disc)))
      (ok (find "2026-07-28" (coerce (gethash "supportedVersions" disc) 'list)
                :test #'string=))
      (ok (find "2025-11-25" (coerce (gethash "supportedVersions" disc) 'list)
                :test #'string=))
      (ok (equal "test-server"
                 (gethash "name"
                          (gethash "io.modelcontextprotocol/serverInfo"
                                   (gethash "_meta" disc))))))
    (let ((tools (mcp-protocol:list-tools client)))
      (ok (= 1 (length tools)))
      (ok (equal "echo" (mcp-protocol:mcp-tool-name (first tools)))))
    (let ((call (mcp-protocol:call-tool client "echo"
                                        (mcp-protocol:json-object "msg" "pong"))))
      (ok (equal "complete" (gethash "resultType" call)))
      (ok (equal "pong"
                 (gethash "text" (elt (gethash "content" call) 0)))))))

(deftest legacy-initialize-handshake
  (multiple-value-bind (client server)
      (%wired :era :legacy)
    (declare (ignore server))
    (let ((init (mcp-protocol:mcp-initialize client)))
      (ok (eq :legacy (mcp-protocol:mcp-client-era client)))
      (ok (equal "2025-11-25" (gethash "protocolVersion" init)))
      (ok (null (gethash "resultType" init)))
      (ok (equal "test-server"
                 (gethash "name" (gethash "serverInfo" init)))))
    (let ((tools (mcp-protocol:list-tools client)))
      (ok (= 1 (length tools)))
      (ok (equal "echo" (mcp-protocol:mcp-tool-name (first tools)))))
    (let ((call (mcp-protocol:call-tool client "echo"
                                        (mcp-protocol:json-object "msg" "legacy"))))
      (ok (null (gethash "resultType" call)))
      (ok (equal "legacy"
                 (gethash "text" (elt (gethash "content" call) 0)))))))

(deftest discover-fallback-invalid-params
  "FastMCP 3 rejects server/discover with -32602, not -32601."
  (let* ((server (%echo-server))
         (transport (rpc-backend-inprocess:make-inprocess-rpc-transport))
         (client (make-instance 'mcp-protocol:mcp-client
                                :transport transport
                                :era :unknown
                                :name "test-client"
                                :version "0.1.0")))
    (rpc-protocol:rpc-serve
     (lambda (method params)
       (if (string= method "server/discover")
           (error 'rpc-protocol:rpc-invalid-params
                  :message "Invalid request parameters")
           (handler-case
               (mcp-protocol:dispatch-mcp-method server method params)
             (mcp-protocol:mcp-error (c)
               (error 'rpc-protocol:rpc-error
                      :message (mcp-protocol:mcp-error-message c)
                      :code (mcp-protocol:mcp-error-code c)
                      :data (mcp-protocol:mcp-error-data c))))))
     :transport transport)
    (let ((init (mcp-protocol:mcp-initialize client)))
      (ok (eq :legacy (mcp-protocol:mcp-client-era client)))
      (ok (equal "2025-11-25" (gethash "protocolVersion" init))))))

(deftest discover-fallback-to-legacy
  (let* ((server (%echo-server))
         (transport (rpc-backend-inprocess:make-inprocess-rpc-transport))
         (client (make-instance 'mcp-protocol:mcp-client
                                :transport transport
                                :era :unknown
                                :name "test-client"
                                :version "0.1.0")))
    (rpc-protocol:rpc-serve
     (lambda (method params)
       (if (string= method "server/discover")
           (error 'rpc-protocol:rpc-method-not-found)
           (handler-case
               (mcp-protocol:dispatch-mcp-method server method params)
             (mcp-protocol:mcp-error (c)
               (error 'rpc-protocol:rpc-error
                      :message (mcp-protocol:mcp-error-message c)
                      :code (mcp-protocol:mcp-error-code c)
                      :data (mcp-protocol:mcp-error-data c))))))
     :transport transport)
    (let ((init (mcp-protocol:mcp-initialize client)))
      (ok (eq :legacy (mcp-protocol:mcp-client-era client)))
      (ok (equal "2025-11-25" (gethash "protocolVersion" init))))))

(defun %modern-meta ()
  (mcp-protocol:json-object
   "io.modelcontextprotocol/protocolVersion" "2026-07-28"
   "io.modelcontextprotocol/clientCapabilities" (mcp-protocol:json-object)))

(deftest modern-list-is-cacheable
  "SEP-2549: tools/list (and other list/read results) need ttlMs + cacheScope."
  (multiple-value-bind (client server)
      (%wired)
    (declare (ignore client))
    (let ((raw (mcp-protocol:dispatch-mcp-method
                server "tools/list"
                (mcp-protocol:json-object "_meta" (%modern-meta)))))
      (ok (equal "complete" (gethash "resultType" raw)))
      (ok (eql mcp-protocol:+mcp-default-ttl-ms+ (gethash "ttlMs" raw)))
      (ok (equal "public" (gethash "cacheScope" raw))))))

(deftest unsupported-version-32022
  (multiple-value-bind (client server)
      (%wired :era :modern)
    (declare (ignore server))
    (setf (mcp-protocol:mcp-client-protocol-version client) "1999-01-01")
    (handler-case
        (progn
          (mcp-protocol:mcp-discover client)
          (fail "expected mcp-error"))
      (mcp-protocol:mcp-error (c)
        (ok (eql mcp-protocol:+mcp-error-unsupported-protocol-version+
                 (mcp-protocol:mcp-error-code c)))
        (ok (find "2026-07-28"
                  (coerce (mcp-protocol:param (mcp-protocol:mcp-error-data c) "supported")
                          'list)
                  :test #'string=))
        (ok (equal "1999-01-01"
                   (mcp-protocol:param (mcp-protocol:mcp-error-data c) "requested")))))))

(deftest spec-classes-and-gfs
  (ok (find-class 'mcp-protocol:mcp-resource-template))
  (ok (find-class 'mcp-protocol:mcp-sampling-request))
  (ok (find-class 'mcp-protocol:mcp-elicit-request))
  (ok (find-class 'mcp-protocol:mcp-root))
  (ok (find-class 'mcp-protocol:mcp-completion-ref))
  (ok (find-class 'mcp-protocol:mcp-log-message))
  (ok (find-class 'mcp-protocol:mcp-progress))
  (ok (find-class 'mcp-protocol:mcp-subscription))
  (ok (fboundp 'mcp-protocol:create-message))
  (ok (fboundp 'mcp-protocol:elicit))
  (ok (fboundp 'mcp-protocol:list-roots))
  (ok (fboundp 'mcp-protocol:complete))
  (ok (fboundp 'mcp-protocol:listen-subscriptions))
  (ok (fboundp 'mcp-protocol:request-sampling)))

(deftest modern-meta-required
  (multiple-value-bind (client server)
      (%wired)
    (declare (ignore client))
    (handler-case
        (progn
          (mcp-protocol:dispatch-mcp-method
           server "tools/list"
           (mcp-protocol:json-object
            "_meta" (mcp-protocol:json-object
                     "io.modelcontextprotocol/protocolVersion" "2026-07-28")))
          (fail "expected missing clientCapabilities"))
      (mcp-protocol:mcp-error (c)
        (ok (eql rpc-protocol:+invalid-params+ (mcp-protocol:mcp-error-code c)))))))

(deftest unknown-tool-is-invalid-params
  (multiple-value-bind (client server)
      (%wired)
    (declare (ignore client))
    (handler-case
        (progn
          (mcp-protocol:dispatch-mcp-method
           server "tools/call"
           (mcp-protocol:json-object
            "name" "nope"
            "_meta" (%modern-meta)))
          (fail "expected unknown tool"))
      (mcp-protocol:mcp-error (c)
        (ok (eql rpc-protocol:+invalid-params+ (mcp-protocol:mcp-error-code c))))
      (rpc-protocol:rpc-error (c)
        (ok (eql rpc-protocol:+invalid-params+ (rpc-protocol:rpc-error-code c)))))))

(deftest input-schema-validation
  (let ((server (%echo-server)))
    (mcp-protocol:register-tool
     server
     (mcp-protocol:make-mcp-tool
      "need-msg"
      :input-schema (mcp-protocol:json-object
                     "type" "object"
                     "required" (vector "msg")
                     "properties"
                     (mcp-protocol:json-object
                      "msg" (mcp-protocol:json-object "type" "string")))
      :handler (lambda (args)
                 (mcp-protocol:tool-result
                  (list (mcp-protocol:make-text-content
                         (mcp-protocol:param args "msg")))))))
    (ok (hash-table-p
         (mcp-protocol:call-tool server "need-msg"
                                 (mcp-protocol:json-object "msg" "ok"))))
    (ok (signals (mcp-protocol:call-tool server "need-msg"
                                         (mcp-protocol:json-object))
                 'mcp-protocol:mcp-error))))

(deftest templates-complete-listen-log
  (multiple-value-bind (client server)
      (%wired)
    (mcp-protocol:register-resource-template
     server
     (mcp-protocol:make-mcp-resource-template
      "memo://{id}" :name "memo"
      :complete (lambda (name value)
                  (declare (ignore name))
                  (list (concatenate 'string value "1")))))
    (mcp-protocol:register-prompt
     server
     (mcp-protocol:make-mcp-prompt
      "pick" :complete (lambda (name value)
                         (declare (ignore name value))
                         '("alpha" "beta"))))
    (let ((tmpls (mcp-protocol:list-resource-templates client)))
      (ok (= 1 (length tmpls)))
      (ok (equal "memo://{id}" (mcp-protocol:mcp-resource-template-uri (first tmpls)))))
    (let ((comp (mcp-protocol:complete
                 client
                 (mcp-protocol:json-object "type" "ref/prompt" "name" "pick")
                 (mcp-protocol:json-object "name" "x" "value" ""))))
      (ok (equal "alpha"
                 (elt (gethash "values" (gethash "completion" comp)) 0))))
    (let ((sub (mcp-protocol:listen-subscriptions
                client (mcp-protocol:json-object "toolsListChanged" t))))
      (ok (eq t (gethash "toolsListChanged" (gethash "notifications" sub)))))
    (ok (hash-table-p (mcp-protocol:set-log-level client "debug")))))

(deftest client-feature-handlers
  (let ((client (make-instance 'mcp-protocol:mcp-client
                               :roots (list (mcp-protocol:make-mcp-root
                                             "file:///tmp" :name "tmp"))
                               :sampling-handler
                               (lambda (params)
                                 (declare (ignore params))
                                 (mcp-protocol:json-object "role" "assistant"
                                                           "model" "test"
                                                           "content" (mcp-protocol:make-text-content "hi")))
                               :elicitation-handler
                               (lambda (params)
                                 (declare (ignore params))
                                 (mcp-protocol:json-object "action" "accept"
                                                           "content" (mcp-protocol:json-object))))))
    (ok (equal "file:///tmp"
               (gethash "uri" (elt (gethash "roots" (mcp-protocol:list-roots client)) 0))))
    (ok (equal "assistant" (gethash "role" (mcp-protocol:create-message client
                                                                        (mcp-protocol:json-object)))))
    (ok (equal "accept" (gethash "action" (mcp-protocol:elicit client
                                                               (mcp-protocol:json-object)))))))

(deftest mrtr-input-required
  (let ((server (%echo-server)))
    (mcp-protocol:register-tool
     server
     (mcp-protocol:make-mcp-tool
      "need-sample"
      :input-schema (mcp-protocol:json-object "type" "object")
      :handler (lambda (args)
                 (declare (ignore args))
                 (mcp-protocol:request-sampling (mcp-protocol:json-object)))))
    (let ((raw (mcp-protocol:dispatch-mcp-method
                server "tools/call"
                (mcp-protocol:json-object "name" "need-sample"
                                          "_meta" (%modern-meta)))))
      (ok (equal "input_required" (gethash "resultType" raw)))
      (ok (gethash "inputRequests" raw)))))

(deftest pagination-next-cursor
  (let ((server (make-instance 'mcp-protocol:mcp-server :name "page" :version "0"))
        (mcp-protocol:*mcp-page-size* 2))
    (loop for i from 0 below 3
          do (mcp-protocol:register-tool
              server
              (mcp-protocol:make-mcp-tool
               (format nil "t~d" i)
               :input-schema (mcp-protocol:json-object "type" "object")
               :handler (lambda (args) (declare (ignore args)) "x"))))
    (let ((page1 (mcp-protocol:dispatch-mcp-method
                  server "tools/list"
                  (mcp-protocol:json-object "_meta" (%modern-meta)))))
      (ok (eql 2 (length (gethash "tools" page1))))
      (ok (equal "2" (gethash "nextCursor" page1)))
      (let ((page2 (mcp-protocol:dispatch-mcp-method
                    server "tools/list"
                    (mcp-protocol:json-object
                     "cursor" (gethash "nextCursor" page1)
                     "_meta" (%modern-meta)))))
        (ok (eql 1 (length (gethash "tools" page2))))
        (ok (null (gethash "nextCursor" page2)))))))

(deftest provide-input-restart
  (let ((got (handler-bind ((mcp-protocol:mcp-input-required
                             (lambda (c)
                               (mcp-protocol:invoke-provide-input
                                (mcp-protocol:json-object "role" "assistant")
                                c))))
               (mcp-protocol:request-sampling (mcp-protocol:json-object)))))
    (ok (hash-table-p got))
    (ok (equal "assistant" (gethash "role" got)))))

(deftest unhandled-input-required-still-maps
  (let ((server (%echo-server)))
    (mcp-protocol:register-tool
     server
     (mcp-protocol:make-mcp-tool
      "need-sample-2"
      :input-schema (mcp-protocol:json-object "type" "object")
      :handler (lambda (args)
                 (declare (ignore args))
                 (mcp-protocol:request-sampling (mcp-protocol:json-object)))))
    (let ((raw (mcp-protocol:dispatch-mcp-method
                server "tools/call"
                (mcp-protocol:json-object "name" "need-sample-2"
                                          "_meta" (%modern-meta)))))
      (ok (equal "input_required" (gethash "resultType" raw))))))

(deftest unknown-tool-typed
  (ok (signals (mcp-protocol:call-tool (%echo-server) "nope"
                                       (mcp-protocol:json-object))
               'mcp-protocol:mcp-unknown-tool)))

(deftest unknown-tool-use-value
  (let* ((server (%echo-server))
         (echo (first (mcp-protocol:list-tools server)))
         (result (handler-bind ((mcp-protocol:mcp-unknown-tool
                                 (lambda (c)
                                   (mcp-protocol:invoke-use-value echo c))))
                   (mcp-protocol:call-tool server "nope"
                                           (mcp-protocol:json-object "msg" "via")))))
    (ok (hash-table-p result))
    (ok (null (gethash "isError" result)))))

(deftest unknown-tool-skip
  (let ((result (handler-bind ((mcp-protocol:mcp-unknown-tool
                                (lambda (c)
                                  (mcp-protocol:invoke-skip c))))
                  (mcp-protocol:call-tool (%echo-server) "nope"
                                          (mcp-protocol:json-object)))))
    (ok (eq t (gethash "isError" result)))))

(deftest call-tool-use-value-result
  (let ((got (handler-bind ((mcp-protocol:mcp-unknown-tool
                             (lambda (c)
                               (mcp-protocol:invoke-use-value
                                (mcp-protocol:tool-result
                                 (list (mcp-protocol:make-text-content "supplied")))
                                c))))
               (mcp-protocol:call-tool (%echo-server) "nope"
                                       (mcp-protocol:json-object)))))
    (ok (hash-table-p got))
    (ok (equal "supplied"
               (gethash "text" (elt (gethash "content" got) 0))))))
