(defsystem "mcp-protocol"
  :version "0.1.0"
  :description "CLOS MCP client/server protocol for cl-stack"
  :author "egao1980"
  :license "MIT"
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "conditions")
               (:file "protocol"))
  :in-order-to ((test-op (test-op "mcp-protocol/tests"))))

(defsystem "mcp-protocol/tests"
  :depends-on ("mcp-protocol" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "protocol-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
