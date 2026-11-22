---
system: mcp-protocol
test-system: mcp-protocol/tests
brief: ../cl-stack/docs/capabilities/mcp.md
---

# mcp-protocol 0.4.0: the client side a host needs (egao1980/mcp-protocol#14)

Implement the rules in the header of `src/client.lisp`, remove `%contract-pending` and every call:

1. `%parse-tool` (src/protocol.lisp) keeps title, outputSchema, annotations, icons and `_meta`;
   `mcp-tool-annotation`, `mcp-tool-read-only-p`, `mcp-tool-visible-to-model-p`.
2. `list-tools` on a client returns `(values tools next-cursor)`; `list-all-tools` follows the
   cursor, `mcp-pagination-error` with `continue`.
3. `mcp-client-handle-notification` (list_changed → generation + hook, message → log handler,
   progress → progress handler, then the notification hook), `mcp-client-handle-request`
   (ping, sampling, elicitation, roots; -32601 otherwise), `mcp-client-transport-handlers`.
4. `call-tool` / `list-tools` `:timeout` with client-chosen ids `"mcp-N"`, a
   `notifications/cancelled` on timeout, `mcp-timeout`.

The given code (slots on `mcp-client` and `mcp-tool`, `make-mcp-tool :meta`, `%tool-json`
emitting icons and `_meta`, the two conditions, exports) stays. Consumers: ai-agent-protocol/mcp
0.2.0 (#37) and cl-stack-code 0.13.0 (H3e-2a). Then mcp-backend-stdio 0.3.0 wires
`mcp-client-transport-handlers` into its transport.
