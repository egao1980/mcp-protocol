(in-package #:mcp-protocol)

;;; Conditions + restarts (pathlib shape).
;;;
;;; MCP-INPUT-REQUIRED is a non-error control condition (MRTR). PROVIDE-INPUT
;;; resumes with a result; DECLINE-INPUT raises MCP-ERROR. Unhandled SIGNAL
;;; still maps to resultType=input_required via the existing handler-case.

(define-condition mcp-error (error)
  ((message :initarg :message :reader mcp-error-message :initform "MCP error")
   (code :initarg :code :reader mcp-error-code :initform -32603)
   (data :initarg :data :reader mcp-error-data :initform nil)
   (cause :initarg :cause :reader mcp-error-cause :initform nil))
  (:report (lambda (c s)
             (format s "~A~@[ [~A]~]" (mcp-error-message c) (mcp-error-code c)))))

(define-condition mcp-missing-backend (mcp-error) ()
  (:report (lambda (c s)
             (format s "mcp backend missing~@[: ~A~]" (mcp-error-message c)))))

(define-condition mcp-unknown-tool (mcp-error)
  ((name :initarg :name :reader mcp-unknown-tool-name :initform nil))
  (:report (lambda (c s)
             (format s "unknown MCP tool~@[ ~s~]~@[: ~A~]"
                     (mcp-unknown-tool-name c)
                     (mcp-error-message c)))))

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

;;; --- restart helpers -------------------------------------------------------

(defun call-with-mcp-restarts (thunk)
  "Establish RETRY / USE-VALUE around THUNK."
  (tagbody
   :retry
     (return-from call-with-mcp-restarts
       (restart-case (funcall thunk)
         (retry ()
           :report "Retry the MCP operation"
           (go :retry))
         (use-value (value)
           :report "Use a supplied value instead"
           :interactive (lambda ()
                          (format *query-io* "Value to use: ")
                          (force-output *query-io*)
                          (list (read *query-io*)))
           value)))))

(defmacro with-mcp-restarts (&body body)
  `(call-with-mcp-restarts (lambda () ,@body)))

(defun invoke-retry (&optional condition)
  (let ((r (find-restart 'retry condition)))
    (when r (invoke-restart r))))

(defun invoke-use-value (value &optional condition)
  (let ((r (find-restart 'use-value condition)))
    (when r (invoke-restart r value))))

(defun invoke-provide-input (value &optional condition)
  (let ((r (find-restart 'provide-input condition)))
    (when r (invoke-restart r value))))

(defun invoke-decline-input (&optional condition)
  (let ((r (find-restart 'decline-input condition)))
    (when r (invoke-restart r))))

(defun invoke-skip (&optional condition)
  (let ((r (find-restart 'skip condition)))
    (when r (invoke-restart r))))

(defun auto-retry (condition)
  (when (find-restart 'retry condition)
    (invoke-retry condition)))

(defun auto-decline-input (condition)
  (when (find-restart 'decline-input condition)
    (invoke-decline-input condition)))

(defmacro with-auto-retry (&body body)
  `(handler-bind ((mcp-error #'auto-retry))
     (with-mcp-restarts ,@body)))

(defun %signal-input-required (&key input-requests request-state)
  (restart-case
      (signal 'mcp-input-required
              :input-requests input-requests
              :request-state request-state)
    (provide-input (result)
      :report "Supply the host/client result and continue"
      :interactive (lambda ()
                     (format *query-io* "Input result: ")
                     (force-output *query-io*)
                     (list (read *query-io*)))
      result)
    (decline-input ()
      :report "Decline the input request"
      (error 'mcp-error
             :message "input declined"
             :code rpc-protocol:+internal-error+))))
