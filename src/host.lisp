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
  "→ the capabilities CLIENT's handlers make (see the header)."
  (declare (ignore client))
  (error 'mcp-error :message "0.5.0 contract: mcp-client-default-capabilities is not implemented"))

(defun mcp-input-response (id)
  "→ (values value present-p) of ID in *MCP-INPUT-RESPONSES*."
  (declare (ignore id))
  (error 'mcp-error :message "0.5.0 contract: mcp-input-response is not implemented"))
