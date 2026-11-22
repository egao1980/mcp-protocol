(in-package #:mcp-protocol)

;;; 0.4.0 (contract, egao1980/mcp-protocol#14): the client side a host needs.
;;; Found while contracting cl-stack-code's MCP servers (H3e-2a, brief:
;;; cl-stack/docs/capabilities/cl-stack-code.md "H3e-2 — locked") and
;;; ai-agent-protocol/mcp 0.2.0 (egao1980/ai-agent-protocol#37), which build on
;;; this. Prior art: pydantic-ai's MCP client (cached tool lists invalidated by
;;; list_changed, per-call timeouts, a per-server log handler).
;;;
;;; 1. Every field of a tool (%PARSE-TOOL in src/protocol.lisp).
;;;    A tool object from tools/list becomes an MCP-TOOL with NAME, TITLE,
;;;    DESCRIPTION ("" when absent), INPUT-SCHEMA, OUTPUT-SCHEMA, ANNOTATIONS
;;;    (the "annotations" JSON object as given, or NIL), ICONS (as given) and
;;;    META ("_meta" as given, or NIL). The server side emits all of them
;;;    (%TOOL-JSON, given), so a tool survives a round trip unchanged.
;;;    MCP-TOOL-ANNOTATION TOOL KEY → the annotation KEY ("readOnlyHint",
;;;    "destructiveHint", "idempotentHint", "openWorldHint", "title"), NIL when
;;;    absent. JSON false / null (NIL or :NULL as decoded) are NIL.
;;;    MCP-TOOL-READ-ONLY-P TOOL → T exactly when readOnlyHint is JSON true.
;;;    Hints are untrusted metadata: a host may lower a default with them, never
;;;    raise trust (the host's rule; documented here so hosts agree).
;;;    MCP-TOOL-VISIBLE-TO-MODEL-P TOOL → NIL when _meta.ui.visibility is an
;;;    array that does not contain "model" (the MCP Apps extension: an
;;;    app-only tool); T otherwise (no _meta, no ui, no visibility, or not an
;;;    array).
;;;
;;; 2. Pagination.
;;;    LIST-TOOLS (client) → (values TOOLS NEXT-CURSOR): NEXT-CURSOR is the
;;;    result's "nextCursor" string, NIL when absent (0.3 returned one value
;;;    and dropped it). LIST-ALL-TOOLS CLIENT &key (MAX-PAGES +MAX-TOOL-PAGES+)
;;;    follows the cursor from the first page and → every tool, in server
;;;    order. A cursor seen twice, or more than MAX-PAGES pages, signals
;;;    MCP-PAGINATION-ERROR (an MCP-ERROR), whose CONTINUE restart returns the
;;;    tools collected so far.
;;;
;;; 3. Messages the server sends (notifications and requests).
;;;    MCP-CLIENT-HANDLE-NOTIFICATION CLIENT METHOD PARAMS:
;;;      notifications/tools/list_changed → (incf MCP-CLIENT-TOOLS-GENERATION),
;;;        then (funcall MCP-CLIENT-ON-TOOLS-CHANGED CLIENT) when set;
;;;      notifications/message → MCP-LOG on the client (the log handler gets
;;;        an MCP-LOG-MESSAGE with level, data and logger);
;;;      notifications/progress → SEND-PROGRESS on the client (the progress
;;;        handler gets an MCP-PROGRESS with token, progress, total, message);
;;;      anything else → nothing built in.
;;;      Then, for every method, MCP-CLIENT-NOTIFICATION-HOOK (lambda (client
;;;      method params)) when set. An error in a handler or the hook is not
;;;      signalled out (a notification has no answer): it is dropped.
;;;      → T.
;;;    MCP-CLIENT-HANDLE-REQUEST CLIENT METHOD PARAMS → the result object:
;;;      ping → {}; sampling/createMessage → CREATE-MESSAGE; elicitation/create
;;;      → ELICIT; roots/list → LIST-ROOTS. No handler for sampling or
;;;      elicitation, or any other method → RPC-PROTOCOL:RPC-ERROR -32601
;;;      ("method not found: <method>"). An MCP-ERROR from a handler becomes an
;;;      RPC-ERROR with its code, message and data.
;;;    MCP-CLIENT-TRANSPORT-HANDLERS CLIENT → (values ON-NOTIFICATION
;;;      ON-REQUEST): two (lambda (method params)) closures over the two
;;;      functions above, in the shape rpc-backend-stdio's
;;;      MAKE-STDIO-RPC-TRANSPORT :ON-NOTIFICATION / :ON-REQUEST take, so a
;;;      backend wires a client in one call (mcp-backend-stdio 0.3.0 does,
;;;      egao1980/mcp-backend-stdio; streamable HTTP feeds SSE notifications
;;;      through the same function).
;;;    Caveat (pydantic-ai's too): modern-era (2026-07-28) servers send
;;;    notifications only while a request is open, so a host must not rely on
;;;    list_changed alone; hosts offer a "no cache" switch per server.
;;;
;;; 4. Timeouts (CALL-TOOL and LIST-TOOLS on a client).
;;;    CALL-TOOL CLIENT NAME ARGUMENTS &key TIMEOUT, LIST-TOOLS … &key CURSOR
;;;    TIMEOUT: TIMEOUT (seconds, a positive real, NIL = none) goes to
;;;    RPC-PROTOCOL:RPC-CALL :TIMEOUT with a client-chosen id "mcp-N" (N from
;;;    MCP-CLIENT-REQUEST-COUNTER, from 1; strings, so they never collide with a
;;;    transport's integer counter). When the call fails with an RPC-ERROR
;;;    after at least TIMEOUT seconds (transports name their timeouts
;;;    differently, so the elapsed time decides), the client sends
;;;    notifications/cancelled {"requestId": "mcp-N", "reason": "timeout"} and
;;;    signals MCP-TIMEOUT (an MCP-ERROR: method, seconds, request id; code
;;;    -32001; message "<method> timed out after S s"). A notify failure while
;;;    cancelling is ignored. The input_required retry of CALL-TOOL gets the
;;;    same timeout.

(defparameter +max-tool-pages+ 100
  "LIST-ALL-TOOLS's default page limit.")

(define-condition mcp-timeout (mcp-error)
  ((method :initarg :method :reader mcp-timeout-method :initform nil)
   (seconds :initarg :seconds :reader mcp-timeout-seconds :initform nil)
   (request-id :initarg :request-id :reader mcp-timeout-request-id :initform nil))
  (:default-initargs :code -32001)
  (:documentation "0.4.0: a client request got no answer within its :TIMEOUT."))

(define-condition mcp-pagination-error (mcp-error) ()
  (:documentation "0.4.0: LIST-ALL-TOOLS saw a cursor twice or too many pages.
Restart CONTINUE returns the tools collected so far."))

;;; --- 1. tool fields ---------------------------------------------------------

(defun mcp-tool-annotation (tool key)
  "The annotation KEY of TOOL (\"readOnlyHint\", \"destructiveHint\", …), NIL when
absent, JSON false or JSON null."
  (%json-value (param (mcp-tool-annotations tool) key)))

(defun mcp-tool-read-only-p (tool)
  "T exactly when TOOL's readOnlyHint is JSON true. Hints are untrusted: a host
may lower a default with them, never raise trust."
  (eq t (mcp-tool-annotation tool "readOnlyHint")))

(defun %json-get (object key)
  "KEY of the JSON object OBJECT (a hash table); NIL for anything else or null."
  (and (hash-table-p object) (%json-value (gethash key object))))

(defun mcp-tool-visible-to-model-p (tool)
  "NIL when _meta.ui.visibility is an array without \"model\" (an MCP Apps
app-only tool); T otherwise."
  (let ((visibility (%json-get (%json-get (mcp-tool-meta tool) "ui") "visibility")))
    (not (and (vectorp visibility)
              (not (stringp visibility))
              (not (find "model" visibility :test #'equal))))))

;;; --- 2. pagination ----------------------------------------------------------

(defun list-all-tools (client &key (max-pages +max-tool-pages+) timeout)
  "Every tool of CLIENT's server, in server order, following nextCursor.
A cursor seen twice, or a further page past MAX-PAGES, signals
MCP-PAGINATION-ERROR; its CONTINUE restart returns the tools collected so far."
  (check-type max-pages (integer 1))
  (let ((collected '())                 ; newest first
        (seen (make-hash-table :test 'equal))
        (pages 0)
        (cursor nil))
    (restart-case
        (loop
          (multiple-value-bind (tools next)
              (list-tools client :cursor cursor :timeout timeout)
            (incf pages)
            (map nil (lambda (tool) (push tool collected)) tools)
            (when (null next)
              (return (reverse collected)))
            (let ((repeated (gethash next seen)))
              (when (or repeated (>= pages max-pages))
                (error 'mcp-pagination-error
                       :message (if repeated
                                    (format nil "tools/list returned cursor ~s twice" next)
                                    (format nil "tools/list needs more than ~d pages"
                                            max-pages))
                       :data (json-object "cursor" next "pages" pages))))
            (setf (gethash next seen) t
                  cursor next)))
      (continue ()
        :report "Return the tools collected so far."
        ;; REVERSE, not NREVERSE: the list may be inspected again.
        (reverse collected)))))

;;; --- 3. messages the server sends ---------------------------------------------

(defmacro %dropping-errors (&body body)
  "Run BODY; an ERROR is dropped (→ NIL). Only for notification handling, which
the rule says has no answer and must not signal out."
  `(handler-case (progn ,@body)
     (error () nil)))

(defun mcp-client-handle-notification (client method params)
  "Built-in handling of the server notification METHOD, then the notification
hook. Errors in a handler or the hook are dropped. → T."
  (cond
    ((equal method "notifications/tools/list_changed")
     ;; Bumped before any user code runs, so a failing callback cannot undo it.
     (incf (mcp-client-tools-generation client))
     (let ((fn (mcp-client-on-tools-changed client)))
       (when fn
         (%dropping-errors (funcall fn client)))))
    ((equal method "notifications/message")
     (%dropping-errors
       (mcp-log client
                (%json-value (param params "level"))
                (%json-value (param params "data"))
                :logger (%json-value (param params "logger")))))
    ((equal method "notifications/progress")
     (%dropping-errors
       (send-progress client
                      (%json-value (param params "progress"))
                      :progress-token (%json-value (param params "progressToken"))
                      :total (%json-value (param params "total"))
                      :message (%json-value (param params "message"))))))
  (let ((hook (mcp-client-notification-hook client)))
    (when hook
      (%dropping-errors (funcall hook client method params))))
  t)

(defun %client-method-not-found (method)
  (error 'rpc-protocol:rpc-error
         :code rpc-protocol:+method-not-found+
         :message (format nil "method not found: ~a" method)))

(defun mcp-client-handle-request (client method params)
  "The result object for the server request METHOD, or RPC-PROTOCOL:RPC-ERROR
(-32601 for a method without a handler; an MCP-ERROR keeps its code, message
and data)."
  (handler-case
      (cond
        ((equal method "ping") (json-object))
        ((equal method "sampling/createMessage")
         (if (mcp-client-sampling-handler client)
             (create-message client params)
             (%client-method-not-found method)))
        ((equal method "elicitation/create")
         (if (mcp-client-elicitation-handler client)
             (elicit client params)
             (%client-method-not-found method)))
        ((equal method "roots/list") (list-roots client))
        (t (%client-method-not-found method)))
    ;; Same mapping as SERVE-MCP. RPC-ERROR is not an MCP-ERROR, so -32601 passes.
    (mcp-error (c)
      (error 'rpc-protocol:rpc-error
             :message (or (mcp-error-message c) "mcp error")
             :code (or (mcp-error-code c) rpc-protocol:+internal-error+)
             :data (mcp-error-data c)))))

(defun mcp-client-transport-handlers (client)
  "→ (values ON-NOTIFICATION ON-REQUEST), two (lambda (method params)) closures
over CLIENT for a transport's :ON-NOTIFICATION / :ON-REQUEST."
  (values (lambda (method params) (mcp-client-handle-notification client method params))
          (lambda (method params) (mcp-client-handle-request client method params))))
