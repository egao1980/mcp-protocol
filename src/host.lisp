(in-package #:mcp-protocol)

;;; 0.5.0 (contract, egao1980/mcp-protocol H3e-2b issue): what a host needs to
;;; serve a server's sampling and elicitation. Found while contracting
;;; cl-stack-code's MCP part 2 (H3e-2b, brief: cl-stack
;;; docs/capabilities/cl-stack-code.md "H3e-2b — locked"; this library's brief:
;;; docs/capabilities/mcp.md "0.5.0"). GIVEN: the specials, the constant and
;;; the exports. STUBBED: MCP-CLIENT-DEFAULT-CAPABILITIES and MCP-INPUT-RESPONSE;
;;; the changes to src/protocol.lisp, src/features.lisp and src/client.lisp
;;; below are the implementation's.
;;;
;;; 1. A client advertises what it can answer.
;;;    MCP-CLIENT-DEFAULT-CAPABILITIES client → a fresh JSON object:
;;;      "sampling": {}             when MCP-CLIENT-SAMPLING-HANDLER is set
;;;      "elicitation": {"form": {}} when MCP-CLIENT-ELICITATION-HANDLER is set
;;;                                 (form mode only: a handler gets the params,
;;;                                 url mode is a host's own capability)
;;;      "roots": {}                when MCP-CLIENT-ROOTS is not NIL
;;;    in that key order, nothing else (no "sampling.tools": a host that serves
;;;    tool use in sampling passes its own capabilities). The capabilities a
;;;    client sends are MCP-CLIENT-CLIENT-CAPABILITIES when set, else these:
;;;      - 2025-11-25 initialize: "capabilities" (%LEGACY-INITIALIZE; 0.4 sent {})
;;;      - 2026-07-28: every request's _meta
;;;        "io.modelcontextprotocol/clientCapabilities" (%CLIENT-META; 0.4 sent {})
;;;    MCP-INITIALIZE / MCP-DISCOVER :CAPABILITIES still set
;;;    MCP-CLIENT-CLIENT-CAPABILITIES, which then replaces the defaults whole
;;;    (no merge). So a backend that probes (mcp-backend-stdio,
;;;    mcp-backend-streamable-http :PROBE T) advertises its client's handlers
;;;    without passing anything.
;;;
;;; 2. MRTR on the server side: a tool that asks gets the answer on the retry.
;;;    While DISPATCH-MCP-METHOD handles tools/call (also through SERVE-MCP),
;;;    *MCP-INPUT-RESPONSES* is bound to the request's "inputResponses" (a hash
;;;    table, id → the client's result object; NIL when absent) and
;;;    *MCP-REQUEST-STATE* to its "requestState" (or NIL). A handler re-runs from
;;;    the top on every retry (MRTR's model), so:
;;;    REQUEST-SAMPLING params &key id request-state, REQUEST-ELICITATION …,
;;;    REQUEST-ROOTS …: when *MCP-INPUT-RESPONSES* holds ID, return that value
;;;    (the result object) and signal nothing; otherwise signal
;;;    MCP-INPUT-REQUIRED as 0.4 does.
;;;    MCP-INPUT-RESPONSE id → (values value present-p) from *MCP-INPUT-RESPONSES*.
;;;
;;; 3. The client keeps answering until the server is done.
;;;    CALL-TOOL on a client: while the result is input_required, fulfill its
;;;    inputRequests (FULFILL-INPUT-REQUESTS) and call again, carrying every
;;;    response given so far in this call (merged by id, the later wins) and the
;;;    latest requestState, each round with the full TIMEOUT. After
;;;    +MAX-INPUT-ROUNDS+ rounds still input_required → MCP-ERROR (code -32603,
;;;    message "tools/call NAME still needs input after N rounds"). 0.4 retried
;;;    once and returned the second input_required result as if it were a tool
;;;    result.
;;;
;;; Unchanged: a handler's MCP-ERROR (a user's refusal, a bad request) inside
;;; FULFILL-INPUT-REQUESTS ends CALL-TOOL with that error (the host maps it to
;;; a tool error); a client without a handler answers -32601.
;;; Non-goals (later): server-initiated JSON-RPC requests for 2025-11-25
;;; servers built on this library (MCP-SERVER-REQUEST: needs a transport that
;;; routes responses while it serves; rpc-backend-stdio's server loop ignores
;;; them), url-mode elicitation helpers, sampling with tools.

(defvar *mcp-input-responses* nil
  "0.5.0: inputResponses of the tools/call being handled (id → result), or NIL.")

(defvar *mcp-request-state* nil
  "0.5.0: requestState of the tools/call being handled, or NIL.")

(defparameter +max-input-rounds+ 10
  "0.5.0: CALL-TOOL's limit of input_required rounds in one call.")

(defun mcp-client-default-capabilities (client)
  "→ the capabilities CLIENT's handlers make (see the header): a fresh JSON
object with \"sampling\" {} (a sampling handler), \"elicitation\" {\"form\" {}}
(an elicitation handler) and \"roots\" {} (roots), in that order; {} when
CLIENT has none of them, never NIL."
  ;; JSON-OBJECT makes new tables (nested ones too) on every call; keys are
  ;; inserted in rule order and never REMHASHed, so the order holds.
  (json-object
    "sampling" (if (mcp-client-sampling-handler client) (json-object) :omit)
    "elicitation" (if (mcp-client-elicitation-handler client)
                      (json-object "form" (json-object))
                      :omit)
    "roots" (if (mcp-client-roots client) (json-object) :omit)))

(defun mcp-input-response (id)
  "→ (values value present-p) of ID in *MCP-INPUT-RESPONSES*: the client's
result object for input request ID in the tools/call being handled.
(values NIL NIL) outside a request or when the request did not answer ID."
  (if (hash-table-p *mcp-input-responses*)
      (gethash id *mcp-input-responses*)
      (values nil nil)))

(defun %client-capabilities (client)
  "Rule 1: the capabilities CLIENT sends. MCP-CLIENT-CLIENT-CAPABILITIES when
set (whole, no merge), else MCP-CLIENT-DEFAULT-CAPABILITIES. Never NIL."
  (or (mcp-client-client-capabilities client)
      (mcp-client-default-capabilities client)))

(defun %call-with-input-responses (params thunk)
  "Rule 2: call THUNK with *MCP-INPUT-RESPONSES* and *MCP-REQUEST-STATE* bound
from the tools/call PARAMS. \"inputResponses\" counts only when it is a JSON
object (anything else binds NIL); a JSON null \"requestState\" binds NIL.
Bound for every tools/call, NIL included, so a server nested in-process never
sees an outer request's answers."
  ;; Only this file names *MCP-INPUT-RESPONSES*, *MCP-REQUEST-STATE* and
  ;; +MAX-INPUT-ROUNDS+. It is compiled after src/features.lisp and
  ;; src/protocol.lisp, which call these helpers by function, so this LET is a
  ;; real dynamic binding.
  (let ((responses (param params "inputResponses")))
    (let ((*mcp-input-responses* (and (hash-table-p responses) responses))
          (*mcp-request-state* (%json-value (param params "requestState"))))
      (funcall thunk))))

(defun %input-required-result-p (result)
  (and (hash-table-p result)
       (equal (gethash "resultType" result) "input_required")))

(defun %merge-input-responses (answers new)
  "A fresh JSON object: ANSWERS, then every id of NEW on top (the later wins).
Fresh each round, so a request is never changed after it was sent."
  (let ((out (json-object)))
    (dolist (from (list answers new) out)
      (when (hash-table-p from)
        (maphash (lambda (id value) (setf (gethash id out) value)) from)))))

(defun %call-tool-rounds (client name arguments timeout)
  "Rule 3: CALL-TOOL on CLIENT. Round 0 is the plain tools/call. While the
result is input_required, answer its inputRequests (FULFILL-INPUT-REQUESTS)
and call again, carrying every answer given so far in this call (merged by id,
the later wins) and the latest requestState (none when the latest result has
none). Every round is its own request with the full TIMEOUT. A result still
input_required after +MAX-INPUT-ROUNDS+ answered rounds is not answered: it
signals MCP-ERROR -32603 \"tools/call NAME still needs input after N rounds\".
A handler's MCP-ERROR, and any transport error, passes through unchanged."
  (let ((arguments (or arguments (json-object)))
        (limit +max-input-rounds+)      ; read once per call
        (answers nil)                   ; NIL until the first answered round
        (state nil))                    ; requestState of the latest result
    (loop for round from 0
          for result = (%rpc-call client "tools/call"
                                  (json-object
                                    "name" name
                                    "arguments" arguments
                                    "inputResponses" (or answers :omit)
                                    "requestState" (or state :omit))
                                  :timeout timeout)
          unless (%input-required-result-p result)
            return result
          when (>= round limit)
            do (error 'mcp-error
                      :message (format nil "tools/call ~a still needs input after ~d rounds"
                                       name round)
                      :code rpc-protocol:+internal-error+
                      :data (json-object "name" name "rounds" round))
          do (setf answers (%merge-input-responses
                            answers
                            (fulfill-input-requests client (gethash "inputRequests" result)))
                   state (%json-value (gethash "requestState" result))))))
