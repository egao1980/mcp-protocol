(in-package #:mcp-protocol/tests)

(deftest classes-exist
  (ok (find-class 'mcp-protocol:mcp-server))
  (ok (find-class 'mcp-protocol:mcp-client))
  (ok (find-class 'mcp-protocol:mcp-backend)))
