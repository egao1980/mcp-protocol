(in-package #:mcp-protocol)

;;; Dual-era MCP. JSON-RPC is rpc-protocol — do not invent a second codec.
;;;
;;; Modern (2026-07-28+): stateless. Per-request _meta, server/discover,
;;; resultType. No initialize / Mcp-Session-Id.
;;; Legacy (2025-11-25): initialize + notifications/initialized.

(defun %ensure-backend (&optional (backend *mcp-backend*))
  (or backend
      (restart-case
          (error 'mcp-missing-backend
                 :message "*mcp-backend* is nil — load an mcp-backend-*")
        (use-value (value)
          :report "Use a supplied MCP-BACKEND"
          value))))

(defun %client-transport (client)
  (or (mcp-client-transport client)
      rpc-protocol:*rpc-transport*
      (error 'mcp-error :message "mcp-client has no RPC transport")))

(defun modern-protocol-version-p (ver)
  (and (stringp ver) (string>= ver +mcp-protocol-version+)))

(defun %client-meta (client)
  (json-object "io.modelcontextprotocol/protocolVersion"
               (mcp-client-protocol-version client)
               "io.modelcontextprotocol/clientInfo"
               (json-object "name" (mcp-peer-name client)
                            "version" (mcp-peer-version client))
               "io.modelcontextprotocol/clientCapabilities"
               (or (mcp-client-client-capabilities client) (json-object))))

(defun %ensure-params (params)
  (cond
    ((hash-table-p params) params)
    ((null params) (json-object))
    (t (json-object))))

(defun %with-client-meta (client params)
  (let ((params (%ensure-params params)))
    (setf (gethash "_meta" params) (%client-meta client))
    params))

(defun %modern-wire-p (client method)
  (or (eq (mcp-client-era client) :modern)
      (string= method "server/discover")))

(defun %rpc-call (client method params)
  (let ((params (if (%modern-wire-p client method)
                    (%with-client-meta client params)
                    (%ensure-params params))))
    (handler-case
        (rpc-protocol:rpc-call method params :transport (%client-transport client))
      (rpc-protocol:rpc-error (c)
        (error 'mcp-error
               :message (rpc-protocol:rpc-error-message c)
               :code (rpc-protocol:rpc-error-code c)
               :data (rpc-protocol:rpc-error-data c))))))

(defun %rpc-notify (client method params)
  (handler-case
      (rpc-protocol:rpc-notify method (%ensure-params params)
                               :transport (%client-transport client))
    (rpc-protocol:rpc-error (c)
      (error 'mcp-error
             :message (rpc-protocol:rpc-error-message c)
             :code (rpc-protocol:rpc-error-code c)
             :data (rpc-protocol:rpc-error-data c)))))

(defun make-text-content (text)
  (json-object "type" "text" "text" (if (stringp text) text (princ-to-string text))))

(defun tool-result (content &key is-error)
  (json-object "content" (if (listp content) (coerce content 'vector) content)
               "isError" (if is-error t :omit)))

(defun %coerce-tool-result (value)
  (cond
    ((hash-table-p value) value)
    ((stringp value) (tool-result (list (make-text-content value))))
    ((and (listp value) (every #'hash-table-p value))
     (tool-result value))
    (t (tool-result (list (make-text-content value))))))

(defun %tool-json (tool)
  (json-object "name" (mcp-tool-name tool)
               "title" (or (mcp-tool-title tool) :omit)
               "description" (mcp-tool-description tool)
               "inputSchema" (or (mcp-tool-input-schema tool)
                                 (json-object "type" "object"))
               "outputSchema" (or (mcp-tool-output-schema tool) :omit)))

(defun %resource-json (res)
  (json-object "uri" (mcp-resource-uri res)
               "name" (or (mcp-resource-name res) (mcp-resource-uri res))
               "description" (mcp-resource-description res)
               "mimeType" (mcp-resource-mime-type res)))

(defun %prompt-json (prompt)
  (json-object "name" (mcp-prompt-name prompt)
               "description" (mcp-prompt-description prompt)
               "arguments" (or (mcp-prompt-arguments prompt) #())))

(defun %parse-tool (obj)
  (make-mcp-tool (param obj "name")
                 :description (param obj "description")
                 :input-schema (param obj "inputSchema")))

(defun %parse-resource (obj)
  (make-mcp-resource (param obj "uri")
                     :name (param obj "name")
                     :description (param obj "description")
                     :mime-type (param obj "mimeType")))

(defun %parse-prompt (obj)
  (make-mcp-prompt (param obj "name")
                   :description (param obj "description")
                   :arguments (param obj "arguments")))

(defun %server-info (server)
  (json-object "name" (mcp-peer-name server)
               "version" (mcp-peer-version server)
               "title" (or (mcp-peer-title server) :omit)))

(defun %server-capabilities (server)
  (declare (ignore server))
  (json-object "tools" (json-object "listChanged" t)
               "resources" (json-object "listChanged" t "subscribe" t)
               "prompts" (json-object "listChanged" t)
               "completions" (json-object)
               "logging" (json-object)))

(defun %unsupported-version (requested)
  (error 'mcp-error
         :message "Unsupported protocol version"
         :code +mcp-error-unsupported-protocol-version+
         :data (json-object "supported" (coerce *supported-protocol-versions* 'vector)
                            "requested" requested)))

(defun %request-version (params)
  (param (param params "_meta") "io.modelcontextprotocol/protocolVersion"))

(defun %check-version (params)
  (let ((ver (%request-version params)))
    (when (and ver (not (member ver *supported-protocol-versions* :test #'string=)))
      (%unsupported-version ver))))

(defun %check-modern-meta (method params)
  "Modern requests MUST carry protocolVersion + clientCapabilities."
  (when (%request-modern-p method params)
    (let ((meta (param params "_meta")))
      (unless (and meta (stringp (param meta "io.modelcontextprotocol/protocolVersion")))
        (error 'mcp-error
               :message "missing _meta.io.modelcontextprotocol/protocolVersion"
               :code rpc-protocol:+invalid-params+))
      (unless (param meta "io.modelcontextprotocol/clientCapabilities")
        (error 'mcp-error
               :message "missing _meta.io.modelcontextprotocol/clientCapabilities"
               :code rpc-protocol:+invalid-params+))
      (let ((lvl (param meta "io.modelcontextprotocol/logLevel")))
        (when (and lvl (not (member lvl *mcp-log-levels* :test #'string=)))
          (error 'mcp-error
                 :message (format nil "invalid log level ~s" lvl)
                 :code rpc-protocol:+invalid-params+))))))

(defun %request-modern-p (method params)
  (let ((ver (%request-version params)))
    (cond
      ((string= method "server/discover") t)
      ((or (string= method "initialize")
           (string= method "notifications/initialized"))
       nil)
      (ver (modern-protocol-version-p ver))
      (t nil))))

(defun %maybe-complete (obj modern-p &optional extra-meta)
  (when (and modern-p (hash-table-p obj))
    (unless (gethash "resultType" obj)
      (setf (gethash "resultType" obj) "complete"))
    (when extra-meta
      (setf (gethash "_meta" obj) extra-meta)))
  obj)

(defun %server-result-meta (server)
  (json-object "io.modelcontextprotocol/serverInfo" (%server-info server)))

(defun %paginate (items cursor &key (limit *mcp-page-size*))
  (let* ((all (coerce items 'list))
         (start (if (and cursor (stringp cursor) (plusp (length cursor)))
                    (or (parse-integer cursor :junk-allowed t) 0)
                    0))
         (start (min (max 0 start) (length all)))
         (rest (nthcdr start all))
         (page (subseq rest 0 (min limit (length rest))))
         (next (when (> (length rest) limit)
                 (princ-to-string (+ start limit)))))
    (values page next)))

(defun %with-cache (obj &key (ttl-ms +mcp-default-ttl-ms+) (scope "public"))
  "SEP-2549 CacheableResult: ttlMs + cacheScope on list/read results."
  (when (hash-table-p obj)
    (unless (gethash "ttlMs" obj)
      (setf (gethash "ttlMs" obj) ttl-ms))
    (unless (gethash "cacheScope" obj)
      (setf (gethash "cacheScope" obj) scope)))
  obj)

;;; --- registry -------------------------------------------------------------

(defgeneric register-tool (server tool &key)
  (:method ((server mcp-server) (tool mcp-tool) &key)
    (setf (gethash (mcp-tool-name tool) (mcp-server-tools server)) tool)
    tool))

(defgeneric register-resource (server resource &key)
  (:method ((server mcp-server) (resource mcp-resource) &key)
    (setf (gethash (mcp-resource-uri resource) (mcp-server-resources server)) resource)
    resource))

(defgeneric register-prompt (server prompt &key)
  (:method ((server mcp-server) (prompt mcp-prompt) &key)
    (setf (gethash (mcp-prompt-name prompt) (mcp-server-prompts server)) prompt)
    prompt))

;;; --- discover / initialize / ping / cancel --------------------------------

(defgeneric mcp-discover (peer &key protocol-version capabilities client-info))

(defmethod mcp-discover ((server mcp-server) &key protocol-version capabilities client-info)
  (declare (ignore capabilities client-info))
  (let ((ver (or protocol-version +mcp-protocol-version+)))
    (unless (member ver *supported-protocol-versions* :test #'string=)
      (%unsupported-version ver))
    (setf (mcp-server-protocol-version server) ver)
    (%maybe-complete
     (%with-cache
      (json-object "supportedVersions" (coerce *supported-protocol-versions* 'vector)
                   "capabilities" (%server-capabilities server)
                   "instructions" (or (mcp-server-instructions server) :omit)))
     t
     (%server-result-meta server))))

(defmethod mcp-discover ((client mcp-client) &key protocol-version capabilities client-info)
  (when protocol-version
    (setf (mcp-client-protocol-version client) protocol-version))
  (when client-info
    (setf (mcp-peer-name client) (or (param client-info "name") (mcp-peer-name client))
          (mcp-peer-version client) (or (param client-info "version")
                                        (mcp-peer-version client))))
  (when capabilities
    (setf (mcp-client-client-capabilities client) capabilities))
  (setf (mcp-client-era client) :modern)
  (let ((result (%rpc-call client "server/discover" (json-object))))
    (setf (mcp-client-server-info client)
          (or (param (param result "_meta") "io.modelcontextprotocol/serverInfo")
              (param result "serverInfo"))
          (mcp-client-server-capabilities client) (param result "capabilities")
          (mcp-client-instructions client) (param result "instructions"))
    result))

(defgeneric mcp-initialize (peer &key protocol-version capabilities client-info server-info))

(defmethod mcp-initialize ((server mcp-server) &key protocol-version capabilities
                                                 client-info server-info)
  (declare (ignore capabilities client-info))
  (let ((ver (if (and protocol-version
                      (member protocol-version *supported-protocol-versions* :test #'string=))
                 protocol-version
                 +mcp-legacy-protocol-version+)))
    (setf (mcp-server-protocol-version server) ver)
    (json-object "protocolVersion" ver
                 "capabilities" (%server-capabilities server)
                 "serverInfo" (or server-info (%server-info server))
                 "instructions" (or (mcp-server-instructions server) :omit))))

(defun %legacy-initialize (client &key protocol-version capabilities client-info)
  (let* ((ver (or protocol-version +mcp-legacy-protocol-version+))
         (info (or client-info
                   (json-object "name" (mcp-peer-name client)
                                "version" (mcp-peer-version client))))
         (caps (or capabilities (json-object)))
         (result (%rpc-call client "initialize"
                            (json-object "protocolVersion" ver
                                         "capabilities" caps
                                         "clientInfo" info))))
    (%rpc-notify client "notifications/initialized" (json-object))
    (setf (mcp-client-era client) :legacy
          (mcp-client-protocol-version client) (or (param result "protocolVersion") ver)
          (mcp-client-server-info client) (param result "serverInfo")
          (mcp-client-server-capabilities client) (param result "capabilities")
          (mcp-client-instructions client) (param result "instructions"))
    result))

(defun %supported-from-error (err)
  (let ((raw (param (mcp-error-data err) "supported")))
    (cond
      ((null raw) nil)
      ((vectorp raw) (coerce raw 'list))
      ((listp raw) raw)
      (t (list raw)))))

(defmethod mcp-initialize ((client mcp-client) &key protocol-version capabilities
                                                 client-info server-info)
  (declare (ignore server-info))
  (ecase (mcp-client-era client)
    (:legacy
     (%legacy-initialize client :protocol-version (or protocol-version
                                                      +mcp-legacy-protocol-version+)
                         :capabilities capabilities :client-info client-info))
    (:modern
     (mcp-discover client :protocol-version (or protocol-version +mcp-protocol-version+)
                   :capabilities capabilities :client-info client-info))
    (:unknown
     (handler-case
         (mcp-discover client :protocol-version (or protocol-version +mcp-protocol-version+)
                       :capabilities capabilities :client-info client-info)
       (mcp-error (c)
         (when (eql (mcp-error-code c) +mcp-error-unsupported-protocol-version+)
           (let ((retry (find +mcp-protocol-version+ (%supported-from-error c)
                              :test #'string=)))
             (when (and retry (not (equal retry (or protocol-version
                                                    +mcp-protocol-version+))))
               (return-from mcp-initialize
                 (mcp-discover client :protocol-version retry
                               :capabilities capabilities
                               :client-info client-info)))))
         ;; Any other discover failure (FastMCP 3 uses -32602, not -32601) → initialize.
         (setf (mcp-client-era client) :legacy
               (mcp-client-protocol-version client) +mcp-legacy-protocol-version+)
         (%legacy-initialize client
                             :protocol-version +mcp-legacy-protocol-version+
                             :capabilities capabilities
                             :client-info client-info))))))

(defgeneric mcp-ping (peer &key)
  (:method ((server mcp-server) &key)
    (json-object))
  (:method ((client mcp-client) &key)
    (%rpc-call client "ping" (json-object))))

(defgeneric mcp-cancel (peer &key request-id reason)
  (:method ((server mcp-server) &key request-id reason)
    (declare (ignore request-id reason))
    t)
  (:method ((client mcp-client) &key request-id reason)
    (%rpc-notify client "notifications/cancelled"
                 (json-object "requestId" request-id "reason" reason))
    t))

;;; --- tools / resources / prompts ------------------------------------------

(defgeneric list-tools (peer &key cursor))
(defgeneric call-tool (peer name arguments &key))
(defgeneric list-resources (peer &key cursor))
(defgeneric read-resource (peer uri &key))
(defgeneric list-prompts (peer &key))
(defgeneric get-prompt (peer name &key arguments))

(defmethod list-tools ((server mcp-server) &key cursor)
  (declare (ignore cursor))
  (loop for tool being the hash-values of (mcp-server-tools server)
        collect tool))

(defmethod call-tool ((server mcp-server) name arguments &key)
  (let ((tool (gethash name (mcp-server-tools server))))
    (unless tool
      (error 'mcp-unknown-tool :name name
                              :message (format nil "unknown tool ~s" name)
                              :code rpc-protocol:+invalid-params+))
    (validate-tool-arguments tool arguments)
    (let ((fn (mcp-tool-handler tool)))
      (unless fn
        (error 'mcp-error :message (format nil "tool ~s has no handler" name)
                          :code rpc-protocol:+invalid-params+))
      (handler-case
          (%coerce-tool-result (funcall fn arguments))
        (mcp-input-required (c) (signal c))
        (mcp-error (e) (error e))
        (error (e)
          (tool-result (list (make-text-content (format nil "~a" e)))
                       :is-error t))))))

(defmethod list-resources ((server mcp-server) &key cursor)
  (declare (ignore cursor))
  (loop for res being the hash-values of (mcp-server-resources server)
        collect res))

(defmethod read-resource ((server mcp-server) uri &key)
  (let ((res (gethash uri (mcp-server-resources server))))
    (unless res
      (error 'mcp-error :message (format nil "unknown resource ~s" uri)
                        :code rpc-protocol:+invalid-params+))
    (let* ((fn (mcp-resource-handler res))
           (body (if fn (funcall fn res) "")))
      (json-object "contents"
                   (vector (json-object "uri" uri
                                        "mimeType" (mcp-resource-mime-type res)
                                        "text" (if (stringp body)
                                                   body
                                                   (princ-to-string body))))))))

(defmethod list-prompts ((server mcp-server) &key)
  (loop for p being the hash-values of (mcp-server-prompts server)
        collect p))

(defmethod get-prompt ((server mcp-server) name &key arguments)
  (let ((prompt (gethash name (mcp-server-prompts server))))
    (unless prompt
      (error 'mcp-error :message (format nil "unknown prompt ~s" name)
                        :code rpc-protocol:+invalid-params+))
    (let ((fn (mcp-prompt-handler prompt)))
      (if fn
          (funcall fn arguments)
          (json-object "messages"
                       (vector (json-object
                                "role" "user"
                                "content" (make-text-content
                                           (or (mcp-prompt-description prompt)
                                               name)))))))))

(defmethod list-tools ((client mcp-client) &key cursor)
  (let ((result (%rpc-call client "tools/list"
                           (json-object "cursor" (or cursor :omit)))))
    (%vec-map (param result "tools") #'%parse-tool)))

(defmethod call-tool ((client mcp-client) name arguments &key)
  (let ((result (%rpc-call client "tools/call"
                           (json-object "name" name
                                        "arguments" (or arguments (json-object))))))
    (if (and (hash-table-p result)
             (equal (gethash "resultType" result) "input_required"))
        (%rpc-call client "tools/call"
                   (json-object "name" name
                                "arguments" (or arguments (json-object))
                                "inputResponses"
                                (fulfill-input-requests
                                 client (gethash "inputRequests" result))
                                "requestState" (or (gethash "requestState" result)
                                                   :omit)))
        result)))

(defmethod list-resources ((client mcp-client) &key cursor)
  (let ((result (%rpc-call client "resources/list"
                           (json-object "cursor" (or cursor :omit)))))
    (%vec-map (param result "resources") #'%parse-resource)))

(defmethod read-resource ((client mcp-client) uri &key)
  (%rpc-call client "resources/read" (json-object "uri" uri)))

(defmethod list-prompts ((client mcp-client) &key)
  (let ((result (%rpc-call client "prompts/list" (json-object))))
    (%vec-map (param result "prompts") #'%parse-prompt)))

(defmethod get-prompt ((client mcp-client) name &key arguments)
  (%rpc-call client "prompts/get"
             (json-object "name" name "arguments" (or arguments :omit))))

(defmethod list-resource-templates ((client mcp-client) &key cursor)
  (let ((result (%rpc-call client "resources/templates/list"
                           (json-object "cursor" (or cursor :omit)))))
    (%vec-map (param result "resourceTemplates")
              (lambda (obj)
                (make-mcp-resource-template
                 (param obj "uriTemplate")
                 :name (param obj "name")
                 :title (param obj "title")
                 :description (param obj "description")
                 :mime-type (param obj "mimeType"))))))

(defmethod complete ((client mcp-client) ref argument &key context)
  (%rpc-call client "completion/complete"
             (json-object "ref" ref
                          "argument" argument
                          "context" (or context :omit))))

(defmethod listen-subscriptions ((client mcp-client) filters &key)
  (%rpc-call client "subscriptions/listen"
             (json-object "notifications" (or filters (json-object)))))

(defmethod set-log-level ((client mcp-client) level &key)
  (%rpc-call client "logging/setLevel" (json-object "level" level)))

(defmethod notify-tools-list-changed ((client mcp-client) &key)
  (%rpc-notify client "notifications/tools/list_changed" (json-object)))

(defmethod notify-resources-list-changed ((client mcp-client) &key)
  (%rpc-notify client "notifications/resources/list_changed" (json-object)))

(defmethod notify-resources-updated ((client mcp-client) uri &key)
  (%rpc-notify client "notifications/resources/updated" (json-object "uri" uri)))

(defmethod notify-prompts-list-changed ((client mcp-client) &key)
  (%rpc-notify client "notifications/prompts/list_changed" (json-object)))

;;; --- JSON-RPC dispatch / serve --------------------------------------------

(defun %resource-template-json (tmpl)
  (json-object "uriTemplate" (mcp-resource-template-uri tmpl)
               "name" (mcp-resource-template-name tmpl)
               "title" (or (mcp-resource-template-title tmpl) :omit)
               "description" (mcp-resource-template-description tmpl)
               "mimeType" (or (mcp-resource-template-mime-type tmpl) :omit)))

(defun %paged-catalog (items encoder cursor)
  (multiple-value-bind (page next)
      (%paginate items cursor)
    (values (map 'vector encoder page) next)))

(defun dispatch-mcp-method (server method params)
  "HANDLER for rpc-serve. METHOD is a string. Returns a JSON-able result."
  (let ((params (%ensure-params params)))
    (%check-version params)
    (%check-modern-meta method params)
    (let ((modern-p (%request-modern-p method params))
          (meta (%server-result-meta server)))
      (flet ((fail (msg &optional (code rpc-protocol:+invalid-params+))
               (error 'rpc-protocol:rpc-error :message msg :code code))
             (done (obj)
               (%maybe-complete obj modern-p meta)))
        (handler-case
            (cond
              ((string= method "server/discover")
               (mcp-discover server :protocol-version (or (%request-version params)
                                                          +mcp-protocol-version+)))
              ((string= method "initialize")
               (mcp-initialize server
                               :protocol-version (param params "protocolVersion")
                               :capabilities (param params "capabilities")
                               :client-info (param params "clientInfo")))
              ((string= method "notifications/initialized")
               (json-object))
              ((string= method "ping")
               (done (mcp-ping server)))
              ((string= method "notifications/cancelled")
               (mcp-cancel server
                           :request-id (param params "requestId")
                           :reason (param params "reason"))
               (json-object))
              ((string= method "notifications/progress")
               (send-progress server (param params "progress")
                              :progress-token (param params "progressToken")
                              :total (param params "total")
                              :message (param params "message"))
               (json-object))
              ((string= method "notifications/message")
               (mcp-log server (param params "level") (param params "data")
                        :logger (param params "logger"))
               (json-object))
              ((string= method "tools/list")
               (multiple-value-bind (vec next)
                   (%paged-catalog (list-tools server :cursor (param params "cursor"))
                                   #'%tool-json (param params "cursor"))
                 (done (%with-cache
                        (json-object "tools" vec "nextCursor" (or next :omit))))))
              ((string= method "tools/call")
               (done (call-tool server (or (param params "name") (fail "missing tool name"))
                                (param params "arguments"))))
              ((string= method "resources/list")
               (multiple-value-bind (vec next)
                   (%paged-catalog (list-resources server :cursor (param params "cursor"))
                                   #'%resource-json (param params "cursor"))
                 (done (%with-cache
                        (json-object "resources" vec "nextCursor" (or next :omit))))))
              ((string= method "resources/read")
               (done (%with-cache
                      (read-resource server (or (param params "uri") (fail "missing uri"))))))
              ((string= method "resources/templates/list")
               (multiple-value-bind (vec next)
                   (%paged-catalog (list-resource-templates server
                                                            :cursor (param params "cursor"))
                                   #'%resource-template-json (param params "cursor"))
                 (done (%with-cache
                        (json-object "resourceTemplates" vec
                                     "nextCursor" (or next :omit))))))
              ((string= method "prompts/list")
               (multiple-value-bind (vec next)
                   (%paged-catalog (list-prompts server) #'%prompt-json
                                   (param params "cursor"))
                 (done (%with-cache
                        (json-object "prompts" vec "nextCursor" (or next :omit))))))
              ((string= method "prompts/get")
               (done (get-prompt server (or (param params "name") (fail "missing prompt name"))
                                 :arguments (param params "arguments"))))
              ((string= method "completion/complete")
               (done (complete server
                               (or (param params "ref") (fail "missing ref"))
                               (or (param params "argument") (fail "missing argument"))
                               :context (param params "context"))))
              ((string= method "subscriptions/listen")
               (done (listen-subscriptions server (param params "notifications"))))
              ((string= method "logging/setLevel")
               (done (set-log-level server
                                    (or (param params "level") (fail "missing level")))))
              ((string= method "sampling/createMessage")
               (fail "sampling/createMessage is an MRTR input request, not a client RPC"
                     rpc-protocol:+method-not-found+))
              ((string= method "elicitation/create")
               (fail "elicitation/create is an MRTR input request, not a client RPC"
                     rpc-protocol:+method-not-found+))
              ((string= method "roots/list")
               (fail "roots/list is an MRTR input request, not a client RPC"
                     rpc-protocol:+method-not-found+))
              (t
               (error 'rpc-protocol:rpc-error
                      :code rpc-protocol:+method-not-found+
                      :message (format nil "unknown MCP method ~s" method))))
          (mcp-input-required (c)
            (%maybe-complete
             (input-required-result (mcp-input-required-requests c)
                                    (mcp-input-required-state c))
             t meta)))))))

(defun serve-mcp (server &key (transport rpc-protocol:*rpc-transport*))
  (rpc-protocol:rpc-serve
   (lambda (method params)
     (handler-case
         (dispatch-mcp-method server method params)
       (mcp-input-required (c)
         (input-required-result (mcp-input-required-requests c)
                                (mcp-input-required-state c)))
       (mcp-error (c)
         (error 'rpc-protocol:rpc-error
                :message (or (mcp-error-message c) "mcp error")
                :code (or (mcp-error-code c) rpc-protocol:+internal-error+)
                :data (mcp-error-data c)))))
   :transport transport))

(defgeneric backend-mcp-connect (backend &key)
  (:method ((backend mcp-backend) &key)
    (error 'mcp-error :message "backend-mcp-connect not implemented")))

(defgeneric backend-mcp-serve (backend server &key)
  (:method ((backend mcp-backend) server &key)
    (declare (ignore server))
    (error 'mcp-error :message "backend-mcp-serve not implemented")))

(defun mcp-connect (&rest args &key (backend *mcp-backend*) &allow-other-keys)
  (apply #'backend-mcp-connect (%ensure-backend backend)
         (loop for (k v) on args by #'cddr
               unless (eq k :backend)
                 collect k and collect v)))

(defun mcp-serve (server &rest args &key (backend *mcp-backend*) &allow-other-keys)
  (apply #'backend-mcp-serve (%ensure-backend backend) server
         (loop for (k v) on args by #'cddr
               unless (eq k :backend)
                 collect k and collect v)))

(defun use-mcp-backend (backend)
  (setf *mcp-backend* backend))
