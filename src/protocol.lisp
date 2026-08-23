(in-package #:mcp-protocol)

(defclass mcp-peer () ())
(defclass mcp-server (mcp-peer) ())
(defclass mcp-client (mcp-peer) ())
(defclass mcp-backend () ())
(defclass mcp-tool () ())
(defclass mcp-resource () ())

(defvar *mcp-backend* nil)

(defgeneric mcp-initialize (peer &key protocol-version capabilities client-info server-info))
(defgeneric list-tools (server &key cursor))
(defgeneric call-tool (server name arguments &key))
(defgeneric list-resources (server &key cursor))
(defgeneric read-resource (server uri &key))
(defgeneric list-prompts (server &key))
(defgeneric get-prompt (server name &key arguments))
