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
      (ok (equal :false (gethash "isError" result))))
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
