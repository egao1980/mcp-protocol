(in-package #:mcp-protocol)

;;; Dual-era MCP. JSON-RPC is rpc-protocol — do not invent a second codec.
;;;
;;; Modern (2026-07-28+): stateless. Per-request _meta, server/discover,
;;; resultType. No initialize / Mcp-Session-Id.
;;; Legacy (2025-11-25): initialize + notifications/initialized.

(defun json-object (&rest kvs)
  (let ((h (make-hash-table :test 'equal)))
    (loop for (k v) on kvs by #'cddr
          unless (or (null k) (eq v :omit))
            do (setf (gethash k h) v))
    h))

(defun param (obj key &optional default)
  (cond
    ((null obj) default)
    ((hash-table-p obj) (gethash key obj default))
    ((listp obj)
     (let ((cell (assoc key obj :test #'equal)))
       (if cell (cdr cell) default)))
    (t default)))

(defun %ensure-backend (&optional (backend *mcp-backend*))
  (or backend
      (error 'mcp-error :message "*mcp-backend* is nil — load an mcp-backend-*")))

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
               "isError" (if is-error t :false)))

(defun %coerce-tool-result (value)
  (cond
    ((hash-table-p value) value)
    ((stringp value) (tool-result (list (make-text-content value))))
    ((and (listp value) (every #'hash-table-p value))
     (tool-result value))
    (t (tool-result (list (make-text-content value))))))

(defun %tool-json (tool)
  (json-object "name" (mcp-tool-name tool)
               "description" (mcp-tool-description tool)
               "inputSchema" (or (mcp-tool-input-schema tool)
                                 (json-object "type" "object"))))

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

(defun %vec-map (seq fn)
  (map 'list fn (or seq #())))

(defun %server-info (server)
  (json-object "name" (mcp-peer-name server)
               "version" (mcp-peer-version server)))

(defun %server-capabilities (server)
  (declare (ignore server))
  (json-object "tools" (json-object)
               "resources" (json-object)
               "prompts" (json-object)))

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
     (json-object "supportedVersions" (coerce *supported-protocol-versions* 'vector)
                  "capabilities" (%server-capabilities server)
                  "instructions" (or (mcp-server-instructions server) :omit))
     t
     (json-object "io.modelcontextprotocol/serverInfo" (%server-info server)))))

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
      (error 'mcp-error :message (format nil "unknown tool ~s" name)
                        :code rpc-protocol:+method-not-found+))
    (let ((fn (mcp-tool-handler tool)))
      (unless fn
        (error 'mcp-error :message (format nil "tool ~s has no handler" name)))
      (handler-case
          (%coerce-tool-result (funcall fn arguments))
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
                        :code rpc-protocol:+method-not-found+))
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
                        :code rpc-protocol:+method-not-found+))
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
  (%rpc-call client "tools/call"
             (json-object "name" name "arguments" (or arguments (json-object)))))

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

;;; --- JSON-RPC dispatch / serve --------------------------------------------

(defun dispatch-mcp-method (server method params)
  "HANDLER for rpc-serve. METHOD is a string. Returns a JSON-able result."
  (let ((params (%ensure-params params)))
    (%check-version params)
    (let ((modern-p (%request-modern-p method params)))
      (flet ((fail (msg &optional (code rpc-protocol:+invalid-params+))
               (error 'rpc-protocol:rpc-error :message msg :code code))
             (done (obj &optional extra-meta)
               (%maybe-complete obj modern-p extra-meta)))
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
          ((string= method "tools/list")
           (done (json-object "tools" (map 'vector #'%tool-json
                                           (list-tools server :cursor (param params "cursor"))))))
          ((string= method "tools/call")
           (done (call-tool server (or (param params "name") (fail "missing tool name"))
                            (param params "arguments"))))
          ((string= method "resources/list")
           (done (json-object "resources" (map 'vector #'%resource-json
                                               (list-resources server :cursor (param params "cursor"))))))
          ((string= method "resources/read")
           (done (read-resource server (or (param params "uri") (fail "missing uri")))))
          ((string= method "prompts/list")
           (done (json-object "prompts" (map 'vector #'%prompt-json (list-prompts server)))))
          ((string= method "prompts/get")
           (done (get-prompt server (or (param params "name") (fail "missing prompt name"))
                             :arguments (param params "arguments"))))
          (t
           (error 'rpc-protocol:rpc-error
                  :code rpc-protocol:+method-not-found+
                  :message (format nil "unknown MCP method ~s" method))))))))

(defun serve-mcp (server &key (transport rpc-protocol:*rpc-transport*))
  (rpc-protocol:rpc-serve
   (lambda (method params)
     (handler-case
         (dispatch-mcp-method server method params)
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
