(defsystem "mcp-protocol"
  :version "0.1.1"
  :description "CLOS MCP client/server protocol (2026-07-28 + 2025-11-25 dual-era)"
  :author "egao1980"
  :license "MIT"
  :depends-on ("rpc-protocol")
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "conditions")
               (:file "types")
               (:file "protocol"))
  :in-order-to ((test-op (test-op "mcp-protocol/tests"))))

(defsystem "mcp-protocol/tests"
  :depends-on ("mcp-protocol" "rpc-backend-inprocess" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "protocol-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
