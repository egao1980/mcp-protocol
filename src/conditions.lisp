(in-package #:mcp-protocol)

(define-condition mcp-error (error)
  ((message :initarg :message :reader mcp-error-message :initform nil))
  (:report (lambda (c s)
             (format s "mcp error~@[: ~a~]" (mcp-error-message c)))))
