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

Wave-1 does not implement sampling, elicitation, roots, completions, OAuth, or Tasks.

Brief: [cl-stack/docs/capabilities/mcp.md](https://github.com/egao1980/cl-stack/blob/main/docs/capabilities/mcp.md). Tracks [cl-stack#185](https://github.com/egao1980/cl-stack/issues/185).

## License

MIT
