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

(defvar *mcp-notifier* nil
  "Function (METHOD PARAMS) a backend binds for one request, so notifications sent
while handling it travel with the response (an HTTP SSE stream). Takes precedence
over MCP-SERVER-NOTIFIER.")

(defvar *progress-token* nil
  "progressToken from _meta of the request being handled, or NIL.")

(defun mcp-notify (server method params)
  "Send notification METHOD with PARAMS to the client of SERVER.
T when sent; NIL when the current transport has no path for it."
  (let ((fn (or *mcp-notifier* (mcp-server-notifier server))))
    (when fn
      (funcall fn method params)
      t)))

(defun %check-log-level (level)
  (unless (member level *mcp-log-levels* :test #'string=)
    (error 'mcp-error :message (format nil "invalid log level ~s" level)
                      :code rpc-protocol:+invalid-params+)))

(defun log-level-enabled-p (server level)
  "True when LEVEL is at or above the level the client set (all levels by default)."
  (let ((min (mcp-server-log-level server)))
    (or (null min)
        (>= (position level *mcp-log-levels* :test #'string=)
            (position min *mcp-log-levels* :test #'string=)))))

(defgeneric mcp-log (peer level data &key logger)
  (:documentation "Emit notifications/message. Deprecated (SEP-2577); kept for the lifecycle window.")
  (:method ((server mcp-server) level data &key logger)
    (%check-log-level level)
    (when (log-level-enabled-p server level)
      (mcp-notify server "notifications/message"
                  (json-object "level" level "logger" (or logger :omit) "data" data)))
    (make-mcp-log-message level data :logger logger))
  (:method ((client mcp-client) level data &key logger)
    (let ((fn (mcp-client-log-handler client)))
      (when fn
        (funcall fn (make-mcp-log-message level data :logger logger)))
      t)))

(defgeneric set-log-level (peer level &key)
  (:documentation "Legacy logging/setLevel (pre-2026 per-request logLevel).")
  (:method ((server mcp-server) level &key)
    (%check-log-level level)
    (setf (mcp-server-log-level server) level)
    (json-object)))

(defgeneric send-progress (peer progress &key progress-token total message)
  (:documentation "notifications/progress for PROGRESS-TOKEN (default: the token of the
request being handled). Without a token nothing is sent.")
  (:method ((server mcp-server) progress &key progress-token total message)
    (let ((token (or progress-token *progress-token*)))
      (when token
        (mcp-notify server "notifications/progress"
                    (json-object "progressToken" token "progress" progress
                                 "total" (or total :omit) "message" (or message :omit))))
      (make-mcp-progress (or token "progress") progress :total total :message message)))
  (:method ((client mcp-client) progress &key progress-token total message)
    (let ((fn (mcp-client-progress-handler client)))
      (when fn
        (funcall fn (make-mcp-progress (or progress-token "progress") progress
                                       :total total :message message)))
      t)))

(defgeneric notify-tools-list-changed (peer &key)
  (:method ((server mcp-server) &key)
    (mcp-notify server "notifications/tools/list_changed" (json-object))))

(defgeneric notify-resources-list-changed (peer &key)
  (:method ((server mcp-server) &key)
    (mcp-notify server "notifications/resources/list_changed" (json-object))))

(defun resource-subscribed-p (server uri)
  "True when URI was subscribed with resources/subscribe or subscriptions/listen."
  (or (gethash uri (mcp-server-resource-subscriptions server))
      (loop for sub being the hash-values of (mcp-server-subscriptions server)
              thereis (member uri (mcp-subscription-resource-uris sub) :test #'equal))))

(defgeneric notify-resources-updated (peer uri &key)
  (:documentation "notifications/resources/updated, sent only when URI is subscribed.")
  (:method ((server mcp-server) uri &key)
    (and (resource-subscribed-p server uri)
         (mcp-notify server "notifications/resources/updated" (json-object "uri" uri)))))

(defgeneric notify-prompts-list-changed (peer &key)
  (:method ((server mcp-server) &key)
    (mcp-notify server "notifications/prompts/list_changed" (json-object))))

(defgeneric subscribe-resource (peer uri &key)
  (:documentation "resources/subscribe (2025-11-25 era).")
  (:method ((server mcp-server) uri &key)
    (setf (gethash uri (mcp-server-resource-subscriptions server)) t)
    (json-object)))

(defgeneric unsubscribe-resource (peer uri &key)
  (:documentation "resources/unsubscribe (2025-11-25 era).")
  (:method ((server mcp-server) uri &key)
    (remhash uri (mcp-server-resource-subscriptions server))
    (json-object)))

;;; URI templates (RFC 6570 level 1 and 2 shapes: {var} and {+var})

(defun %parse-uri-template (template)
  "List of literal strings and (:var name reserved-p) parts."
  (let ((parts '()) (start 0))
    (loop for open = (position #\{ template :start start)
          while open
          do (let ((close (or (position #\} template :start open)
                              (error 'mcp-error :message (format nil "bad URI template ~s" template)
                                                :code rpc-protocol:+invalid-params+))))
               (when (> open start) (push (subseq template start open) parts))
               (let* ((name (subseq template (1+ open) close))
                      (reserved (and (plusp (length name)) (char= (char name 0) #\+))))
                 (push (list :var (if reserved (subseq name 1) name) reserved) parts))
               (setf start (1+ close))))
    (when (< start (length template)) (push (subseq template start) parts))
    (nreverse parts)))

(defun %utf-8-encode-char (c out)
  (let ((code (char-code c)))
    (cond ((< code #x80) (vector-push-extend code out))
          ((< code #x800)
           (vector-push-extend (logior #xC0 (ash code -6)) out)
           (vector-push-extend (logior #x80 (logand code #x3F)) out))
          ((< code #x10000)
           (vector-push-extend (logior #xE0 (ash code -12)) out)
           (vector-push-extend (logior #x80 (logand (ash code -6) #x3F)) out)
           (vector-push-extend (logior #x80 (logand code #x3F)) out))
          (t
           (vector-push-extend (logior #xF0 (ash code -18)) out)
           (vector-push-extend (logior #x80 (logand (ash code -12) #x3F)) out)
           (vector-push-extend (logior #x80 (logand (ash code -6) #x3F)) out)
           (vector-push-extend (logior #x80 (logand code #x3F)) out)))))

(defun %utf-8-decode (octets)
  "Decode OCTETS; a malformed sequence decodes byte by byte as Latin-1."
  (with-output-to-string (s)
    (loop with i = 0 with n = (length octets)
          while (< i n)
          do (let* ((b (aref octets i))
                    (len (cond ((< b #x80) 1) ((= (logand b #xE0) #xC0) 2)
                               ((= (logand b #xF0) #xE0) 3) ((= (logand b #xF8) #xF0) 4) (t 0))))
               (if (and (> len 1) (<= (+ i len) n)
                        (loop for k from 1 below len always (= (logand (aref octets (+ i k)) #xC0) #x80)))
                   (let ((code (logand b (ash #xFF (- (1+ len))))))
                     (loop for k from 1 below len
                           do (setf code (logior (ash code 6) (logand (aref octets (+ i k)) #x3F))))
                     (write-char (code-char code) s)
                     (incf i len))
                   (progn (write-char (code-char b) s) (incf i)))))))

(defun %percent-decode (string)
  (if (not (find #\% string))
      string
      (let ((octets (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
        (loop with i = 0
              while (< i (length string))
              do (let ((c (char string i)))
                   (if (and (char= c #\%) (<= (+ i 3) (length string))
                            (digit-char-p (char string (+ i 1)) 16)
                            (digit-char-p (char string (+ i 2)) 16))
                       (progn (vector-push-extend (parse-integer string :start (1+ i) :end (+ i 3) :radix 16) octets)
                              (incf i 3))
                       (progn (%utf-8-encode-char c octets) (incf i)))))
        (%utf-8-decode octets))))

(defun match-uri-template (template uri)
  "Alist of (name . value) when URI matches TEMPLATE, else NIL (a match with no
variables returns T). {var} matches no /, {+var} matches anything."
  (labels ((walk (parts pos acc)
             (cond ((null parts)
                    (and (= pos (length uri)) (or (reverse acc) t)))
                   ((stringp (first parts))
                    (let* ((lit (first parts)) (end (+ pos (length lit))))
                      (and (<= end (length uri)) (string= lit uri :start2 pos :end2 end)
                           (walk (rest parts) end acc))))
                   (t
                    (destructuring-bind (name reserved) (rest (first parts))
                      (loop for end from (length uri) downto (1+ pos)
                            for value = (subseq uri pos end)
                            when (or reserved (not (find #\/ value)))
                              do (let ((m (walk (rest parts) end
                                                (acons name (%percent-decode value) acc))))
                                   (when m (return m)))))))))
    (walk (%parse-uri-template template) 0 '())))

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
