(in-package #:mcp-protocol)

;;; Spec GFs. Default methods are protocol-complete; backends specialize I/O.

(defun %not-implemented (what)
  (error 'mcp-error
         :message (format nil "~a is not implemented" what)
         :code rpc-protocol:+method-not-found+))

(defun missing-client-capability (capabilities)
  (error 'mcp-error
         :message "Missing required client capability"
         :code +mcp-error-missing-client-capability+
         :data (json-object "requiredCapabilities" capabilities)))

(defun require-client-capability (params capability-key)
  (let ((caps (param (param params "_meta")
                     "io.modelcontextprotocol/clientCapabilities")))
    (unless (and caps (param caps capability-key))
      (missing-client-capability (json-object capability-key (json-object))))))

(defun input-required-result (input-requests &optional request-state)
  (unless (or input-requests request-state)
    (error 'mcp-error
           :message "InputRequiredResult needs inputRequests or requestState"
           :code rpc-protocol:+invalid-params+))
  (json-object "resultType" "input_required"
               "inputRequests" (or input-requests :omit)
               "requestState" (or request-state :omit)))

(defun request-sampling (params &key (id "sample") request-state)
  (%signal-input-required
   :input-requests
   (json-object id (json-object "method" "sampling/createMessage"
                                "params" (or params (json-object))))
   :request-state request-state))

(defun request-elicitation (params &key (id "elicit") request-state)
  (%signal-input-required
   :input-requests
   (json-object id (json-object "method" "elicitation/create"
                                "params" (or params (json-object))))
   :request-state request-state))

(defun request-roots (&key (id "roots") request-state)
  (%signal-input-required
   :input-requests
   (json-object id (json-object "method" "roots/list"
                                "params" (json-object)))
   :request-state request-state))

(defgeneric register-resource-template (server template &key)
  (:method ((server mcp-server) (template mcp-resource-template) &key)
    (setf (gethash (mcp-resource-template-uri template)
                   (mcp-server-resource-templates server))
          template)
    template))

(defgeneric list-resource-templates (peer &key cursor)
  (:method ((server mcp-server) &key cursor)
    (declare (ignore cursor))
    (loop for tmpl being the hash-values of (mcp-server-resource-templates server)
          collect tmpl)))

(defgeneric complete (peer ref argument &key context)
  (:method ((server mcp-server) ref argument &key context)
    (declare (ignore context))
    (let* ((type (param ref "type"))
           (name (or (param ref "name") (param ref "uri")))
           (arg-name (param argument "name"))
           (arg-value (or (param argument "value") ""))
           (fn (cond
                 ((equal type "ref/prompt")
                  (let ((p (gethash name (mcp-server-prompts server))))
                    (and p (mcp-prompt-complete p))))
                 ((equal type "ref/resource")
                  (let ((tmpl (or (gethash name (mcp-server-resource-templates server))
                                  (gethash (param ref "uri")
                                           (mcp-server-resource-templates server)))))
                    (and tmpl (mcp-resource-template-complete tmpl)))))))
      (json-object
       "completion"
       (json-object
        "values" (coerce (if fn
                             (funcall fn arg-name arg-value)
                             #())
                         'vector)
        "hasMore" :false)))))

(defgeneric listen-subscriptions (peer filters &key)
  (:method ((server mcp-server) filters &key)
    (let ((sub (make-mcp-subscription
                (format nil "listen-~x" (random #x1000000))
                :tools-list-changed (and (param filters "toolsListChanged") t)
                :resources-list-changed (and (param filters "resourcesListChanged") t)
                :prompts-list-changed (and (param filters "promptsListChanged") t)
                :resource-uris (%as-list (param filters "resourceSubscriptions")))))
      (setf (gethash (mcp-subscription-id sub) (mcp-server-subscriptions server)) sub)
      (json-object
       "notifications"
       (json-object
        "toolsListChanged" (mcp-subscription-tools-list-changed sub)
        "resourcesListChanged" (mcp-subscription-resources-list-changed sub)
        "promptsListChanged" (mcp-subscription-prompts-list-changed sub)
        "resourceSubscriptions"
        (coerce (or (mcp-subscription-resource-uris sub) #()) 'vector))))))

(defgeneric create-message (peer params &key)
  (:documentation "sampling/createMessage — client GF. Backend/host supplies the LLM.")
  (:method ((client mcp-client) params &key)
    (let ((fn (mcp-client-sampling-handler client)))
      (if fn
          (funcall fn params)
          (%not-implemented "sampling/createMessage"))))
  (:method ((server mcp-server) params &key)
    (declare (ignore params))
    (%not-implemented "sampling/createMessage on server")))

(defgeneric elicit (peer params &key)
  (:documentation "elicitation/create — client GF. Host collects user input.")
  (:method ((client mcp-client) params &key)
    (let ((fn (mcp-client-elicitation-handler client)))
      (if fn
          (funcall fn params)
          (%not-implemented "elicitation/create"))))
  (:method ((server mcp-server) params &key)
    (declare (ignore params))
    (%not-implemented "elicitation/create on server")))

(defgeneric list-roots (peer &key)
  (:documentation "roots/list — client GF.")
  (:method ((client mcp-client) &key)
    (json-object
     "roots"
     (map 'vector
          (lambda (root)
            (json-object "uri" (mcp-root-uri root)
                         "name" (or (mcp-root-name root) :omit)))
          (or (mcp-client-roots client) #()))))
  (:method ((server mcp-server) &key)
    (%not-implemented "roots/list on server")))

(defgeneric mcp-log (peer level data &key logger)
  (:documentation "Emit notifications/message. Deprecated (SEP-2577); kept for the lifecycle window.")
  (:method ((server mcp-server) level data &key logger)
    (unless (member level *mcp-log-levels* :test #'string=)
      (error 'mcp-error :message (format nil "invalid log level ~s" level)
                        :code rpc-protocol:+invalid-params+))
    (make-mcp-log-message level data :logger logger))
  (:method ((client mcp-client) level data &key logger)
    (let ((fn (mcp-client-log-handler client)))
      (when fn
        (funcall fn (make-mcp-log-message level data :logger logger)))
      t)))

(defgeneric set-log-level (peer level &key)
  (:documentation "Legacy logging/setLevel (pre-2026 per-request logLevel).")
  (:method ((server mcp-server) level &key)
    (unless (member level *mcp-log-levels* :test #'string=)
      (error 'mcp-error :message (format nil "invalid log level ~s" level)
                        :code rpc-protocol:+invalid-params+))
    (setf (mcp-server-log-level server) level)
    (json-object)))

(defgeneric send-progress (peer progress &key progress-token total message)
  (:method ((server mcp-server) progress &key progress-token total message)
    (make-mcp-progress (or progress-token "progress") progress
                       :total total :message message))
  (:method ((client mcp-client) progress &key progress-token total message)
    (let ((fn (mcp-client-progress-handler client)))
      (when fn
        (funcall fn (make-mcp-progress (or progress-token "progress") progress
                                       :total total :message message)))
      t)))

(defgeneric notify-tools-list-changed (peer &key)
  (:method ((server mcp-server) &key) t))

(defgeneric notify-resources-list-changed (peer &key)
  (:method ((server mcp-server) &key) t))

(defgeneric notify-resources-updated (peer uri &key)
  (:method ((server mcp-server) uri &key)
    (declare (ignore uri))
    t))

(defgeneric notify-prompts-list-changed (peer &key)
  (:method ((server mcp-server) &key) t))

(defgeneric fulfill-input-requests (peer input-requests &key)
  (:method ((client mcp-client) input-requests &key)
    (let ((out (json-object)))
      (when (hash-table-p input-requests)
        (maphash
         (lambda (id req)
           (let* ((method (param req "method"))
                  (params (param req "params"))
                  (value (cond
                           ((string= method "sampling/createMessage")
                            (create-message client params))
                           ((string= method "elicitation/create")
                            (elicit client params))
                           ((string= method "roots/list")
                            (list-roots client))
                           (t
                            (error 'mcp-error
                                   :message (format nil "unknown input request ~s" method)
                                   :code rpc-protocol:+invalid-params+)))))
             (setf (gethash id out) value)))
         input-requests))
      out)))

(defun %trivial-object-schema-p (schema)
  "Bare {type:object} with no constraints — skip compile (always valid)."
  (and (hash-table-p schema)
       (equal (gethash "type" schema) "object")
       (null (gethash "properties" schema))
       (null (gethash "required" schema))
       (null (gethash "additionalProperties" schema))))

(defun validate-json-schema (schema value &optional compiled)
  "Validate VALUE against a JSON Schema via schema-protocol-json:compile-validator."
  (unless (and schema (hash-table-p schema))
    (return-from validate-json-schema value))
  (when (%trivial-object-schema-p schema)
    (return-from validate-json-schema value))
  (handler-case
      (let ((validator (or compiled
                           (schema-protocol-json:compile-validator schema))))
        (schema-protocol-json:validate-instance validator (or value (json-object)))
        value)
    (schema-protocol-json:json-schema-validation-error (e)
      (error 'mcp-error
             :message (princ-to-string e)
             :code rpc-protocol:+invalid-params+
             :data (json-object "reason" "inputSchema")))
    (schema-protocol:schema-validation-error (e)
      (error 'mcp-error
             :message (princ-to-string e)
             :code rpc-protocol:+invalid-params+
             :data (json-object "reason" "inputSchema")))))

(defun validate-tool-arguments (tool arguments)
  (let ((schema (mcp-tool-input-schema tool)))
    (unless (or (mcp-tool-compiled-input-schema tool)
                (%trivial-object-schema-p schema)
                (not (hash-table-p schema)))
      (setf (mcp-tool-compiled-input-schema tool)
            (schema-protocol-json:compile-validator schema)))
    (validate-json-schema schema arguments (mcp-tool-compiled-input-schema tool))))
