(in-package #:mcp-protocol)

(define-condition mcp-error (error)
  ((message :initarg :message :reader mcp-error-message :initform "MCP error")
   (code :initarg :code :reader mcp-error-code :initform -32603)
   (data :initarg :data :reader mcp-error-data :initform nil))
  (:report (lambda (c s)
             (format s "~A~@[ [~A]~]" (mcp-error-message c) (mcp-error-code c)))))

(define-condition mcp-input-required (condition)
  ((input-requests :initarg :input-requests :reader mcp-input-required-requests
                   :initform nil)
   (request-state :initarg :request-state :reader mcp-input-required-state
                  :initform nil))
  (:report (lambda (c s)
             (format s "MCP input required~@[ (~A)~]"
                     (mcp-input-required-state c))))
  (:documentation
   "Signaled by a server handler to return resultType=input_required (MRTR)."))
