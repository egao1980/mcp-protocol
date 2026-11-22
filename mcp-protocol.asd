(defsystem "mcp-protocol"
  :version "0.4.0"
  :description "CLOS MCP client/server protocol (2026-07-28 + 2025-11-25 dual-era)"
  :author "egao1980"
  :license "MIT"
  :depends-on ("rpc-protocol"
               "schema-protocol"
               (:version "schema-protocol-json" "0.1.1")
               (:version "encoding-protocol" "0.3.0"))
  :properties (:cl-repo (:ci (:with ("dissect"))))
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "conditions")
               (:file "types")
               (:file "features")
               (:file "protocol")
               ;; 0.4.0 (#14)
               (:file "client"))
  :in-order-to ((test-op (test-op "mcp-protocol/tests"))))

(defsystem "mcp-protocol/tests"
  :depends-on ("mcp-protocol" "rpc-backend-inprocess"
               ;; 0.4.0 (#14): the client over a real transport (in-memory pipes)
               (:version "rpc-backend-stdio" "0.3.2") "rpc-protocol-json" "bordeaux-threads"
               "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "protocol-test")
               ;; 0.4.0 (#14)
               (:file "client-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
