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

(defun %contract-pending (what)
  "Contract stub (0.4.0). The implementation removes it and every call."
  (error 'mcp-error :message (format nil "not implemented yet (0.4.0 contract): ~a" what)))

(defun mcp-tool-annotation (tool key)
  (declare (ignore tool key))
  (%contract-pending "mcp-tool-annotation"))

(defun mcp-tool-read-only-p (tool)
  (declare (ignore tool))
  (%contract-pending "mcp-tool-read-only-p"))

(defun mcp-tool-visible-to-model-p (tool)
  (declare (ignore tool))
  (%contract-pending "mcp-tool-visible-to-model-p"))

(defun list-all-tools (client &key (max-pages +max-tool-pages+) timeout)
  (declare (ignore client max-pages timeout))
  (%contract-pending "list-all-tools"))

(defun mcp-client-handle-notification (client method params)
  (declare (ignore client method params))
  (%contract-pending "mcp-client-handle-notification"))

(defun mcp-client-handle-request (client method params)
  (declare (ignore client method params))
  (%contract-pending "mcp-client-handle-request"))

(defun mcp-client-transport-handlers (client)
  (declare (ignore client))
  (%contract-pending "mcp-client-transport-handlers"))
