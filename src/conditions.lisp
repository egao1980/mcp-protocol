(in-package #:mcp-protocol)

(define-condition mcp-error (error)
  ((message :initarg :message :reader mcp-error-message :initform "MCP error")
   (code :initarg :code :reader mcp-error-code :initform -32603)
   (data :initarg :data :reader mcp-error-data :initform nil))
  (:report (lambda (c s)
             (format s "~A~@[ [~A]~]" (mcp-error-message c) (mcp-error-code c)))))
