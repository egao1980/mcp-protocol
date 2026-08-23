(defpackage #:mcp-protocol
  (:use #:cl)
  (:nicknames #:stack-mcp)
  (:export #:mcp-error
           #:mcp-error-message
           #:mcp-peer
           #:mcp-server
           #:mcp-client
           #:mcp-backend
           #:mcp-tool
           #:mcp-resource
           #:*mcp-backend*
           #:mcp-initialize
           #:list-tools
           #:call-tool
           #:list-resources
           #:read-resource
           #:list-prompts
           #:get-prompt))

(in-package #:mcp-protocol)
