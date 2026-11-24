# mcp-protocol

CLOS MCP client/server for cl-stack. Dual-era:

| Era | Revision | Wire |
|-----|----------|------|
| **Modern** (default) | `2026-07-28` | stateless `_meta`, `server/discover`, `resultType` |
| **Legacy** | `2025-11-25` | `initialize` + `notifications/initialized` |

JSON-RPC via [`rpc-protocol`](https://github.com/egao1980/rpc-protocol). **Not** a wrap of `cl-mcp`.

```lisp
(asdf:load-system "mcp-protocol")
(asdf:load-system "rpc-backend-inprocess")

(let* ((server (make-instance 'mcp-protocol:mcp-server :name "demo" :version "0.1.0"))
       (transport (rpc-backend-inprocess:make-inprocess-rpc-transport))
       (client (make-instance 'mcp-protocol:mcp-client :transport transport)))
  (mcp-protocol:register-tool
   server (mcp-protocol:make-mcp-tool "ping" :handler (lambda (args)
                                                        (declare (ignore args))
                                                        "pong")))
  (mcp-protocol:serve-mcp server :transport transport)
  (mcp-protocol:mcp-initialize client)   ; discover; any non-retryable error → initialize
  (mcp-protocol:call-tool client "ping" (mcp-protocol:json-object)))
```

Transports: [`mcp-backend-stdio`](https://github.com/egao1980/mcp-backend-stdio), [`mcp-backend-streamable-http`](https://github.com/egao1980/mcp-backend-streamable-http).

Spec surface (GFs + CLOS). Backends may leave I/O unimplemented:

| Area | Types / GFs |
|------|-------------|
| Tools / resources / prompts | `mcp-tool`, `mcp-resource`, `mcp-prompt`, `list-*`, `call-tool`, `read-resource`, `get-prompt` |
| Resource templates | `mcp-resource-template` (`:handler (lambda (uri vars) ...)` serves `resources/read` for matching URIs), `register-resource-template`, `list-resource-templates`, `match-uri-template` (`{var}`, `{+var}`) |
| Completions | `mcp-completion-ref`, `complete` |
| Subscriptions | `mcp-subscription`, `listen-subscriptions`, `subscribe-resource` / `unsubscribe-resource` (`resources/subscribe`), `notify-*-list-changed`, `notify-resources-updated` (sent only for subscribed URIs) |
| Sampling / elicitation / roots | `mcp-sampling-request`, `mcp-elicit-request`, `mcp-root`, `create-message`, `elicit`, `list-roots` |
| MRTR | `mcp-input-required`, `request-sampling` / `request-elicitation` / `request-roots`, `fulfill-input-requests` |
| Logging / progress | `mcp-log-message`, `mcp-progress`, `mcp-log` (filtered by `set-log-level`), `send-progress` (uses the request's `_meta.progressToken`, bound as `*progress-token*`) |
| Server → client notifications | `mcp-notify`; the backend supplies the path: `mcp-server-notifier` (any thread, set while serving stdio) or `*mcp-notifier*` (bound per request, e.g. HTTP SSE) |
| Validation | `validate-json-schema` / `validate-tool-arguments` via `schema-protocol-json` **≥0.1.1** |
| Argument repair | `repair-tool-arguments` (GF) runs before validation: a property under another spelling (camelCase / snake_case / kebab-case) is renamed; `"false"` / `"20"` / `"1.5"` for boolean / integer / number and a number for a string are coerced. Anything with more than one reading is left for validation to reject |

OAuth stays transport-level (HTTP backend), not a protocol GF.

### 0.5.0: host answers

Rules: the header of `src/host.lisp`.

- **Capabilities from handlers.** A client without `:capabilities` advertises what it can answer: `sampling: {}` (a sampling handler), `elicitation: {form: {}}` (an elicitation handler), `roots: {}` (roots). They go in the 2025-11-25 `initialize` and every 2026-07-28 request's `_meta`. `mcp-client-default-capabilities` returns them. Explicit `:capabilities` replace them whole; to extend them, start from `(mcp-client-default-capabilities client)`.
- **MRTR, server side.** While `tools/call` is handled, `*mcp-input-responses*` and `*mcp-request-state*` hold the request's `inputResponses` / `requestState`. A tool re-runs from the top on every round. `request-sampling` / `request-elicitation` / `request-roots` return an answer that is there instead of signalling; `mcp-input-response id` → `(values value present-p)`.
- **MRTR, client side.** `call-tool` answers `input_required` rounds through `fulfill-input-requests`. Each round carries every answer so far (merged by id, the later wins) and the latest `requestState`, with the full `:timeout`. After `+max-input-rounds+` (10) rounds it signals `mcp-error` (-32603, "tools/call NAME still needs input after N rounds"). A handler's `mcp-error` (a refusal) ends `call-tool` with that error. Note that `with-auto-retry` retries any `mcp-error`, these included.

Brief: [cl-stack/docs/capabilities/mcp.md](https://github.com/egao1980/cl-stack/blob/main/docs/capabilities/mcp.md). Tracks [cl-stack#185](https://github.com/egao1980/cl-stack/issues/185).

## License

MIT
