(in-package #:mcp-protocol/tests)

;;; 0.5.0 (H3e-2b): capabilities from the client's handlers, MRTR answers on
;;; the server side, CALL-TOOL rounds (src/host.lisp). Client and server talk
;;; over rpc-backend-stdio's in-memory pair, as client-test.lisp's do; the
;;; server loop here records every request's params. Every wait is bounded.

(defun %host-server ()
  (let ((server (make-instance 'mcp-protocol:mcp-server :name "host" :version "1.0.0")))
    (flet ((tool (name handler)
             (mcp-protocol:register-tool
              server (mcp-protocol:make-mcp-tool name :input-schema (%obj "type" "object") :handler handler))))
      (tool "summarize"
            (lambda (args)
              (declare (ignore args))
              (let ((r (mcp-protocol:request-sampling
                        (%obj "messages" (vector (%obj "role" "user"
                                                       "content" (%obj "type" "text" "text" "Summarize this")))
                              "maxTokens" 50)
                        :id "s1")))
                (format nil "summary: ~a" (gethash "text" (gethash "content" r))))))
      (tool "confirm"
            (lambda (args)
              (declare (ignore args))
              (let ((r (mcp-protocol:request-elicitation
                        (%obj "mode" "form" "message" "Sure?"
                              "requestedSchema" (%obj "type" "object"
                                                      "properties" (%obj "ok" (%obj "type" "boolean"))))
                        :id "e1")))
                (format nil "answer: ~a" (gethash "action" r)))))
      (tool "both"
            (lambda (args)
              (declare (ignore args))
              (let* ((s (mcp-protocol:request-sampling
                         (%obj "messages" (vector (%obj "role" "user"
                                                        "content" (%obj "type" "text" "text" "Summarize")))
                               "maxTokens" 50)
                         :id "s1"))
                     (e (mcp-protocol:request-elicitation
                         (%obj "mode" "form" "message" "Sure?"
                               "requestedSchema" (%obj "type" "object" "properties" (%obj)))
                         :id "e1")))
                (format nil "both: ~a / ~a" (gethash "text" (gethash "content" s)) (gethash "action" e)))))
      (tool "greedy"
            (lambda (args)
              (declare (ignore args))
              ;; asks a new question on every round
              (let ((n (if mcp-protocol:*mcp-input-responses*
                           (hash-table-count mcp-protocol:*mcp-input-responses*)
                           0)))
                (mcp-protocol:request-elicitation
                 (%obj "mode" "form" "message" "Again?" "requestedSchema" (%obj "type" "object" "properties" (%obj)))
                 :id (format nil "e~d" n))
                "never"))))
    server))

(defun %sampling-answer (text)
  (lambda (params)
    (declare (ignore params))
    (%obj "role" "assistant" "content" (%obj "type" "text" "text" text) "model" "m" "stopReason" "endTurn")))

(defmacro with-host-pair ((client seen &key (era :unknown) sampling elicitation roots) &body body)
  "CLIENT (with the given handlers) talks to %HOST-SERVER over an in-memory
pair; SEEN is a list cell whose car collects (method . params) of every
request the server got, newest first."
  (let ((server (gensym "SERVER")) (st (gensym "ST")) (ct (gensym "CT")) (thread (gensym "THREAD"))
        (on-n (gensym)) (on-r (gensym)))
    `(let* ((,server (%host-server))
            (,seen (list '()))
            (,client (make-instance 'mcp-protocol:mcp-client
                                    :name "host-test" :version "0.1.0" :era ,era
                                    :sampling-handler ,sampling :elicitation-handler ,elicitation
                                    :roots ,roots)))
       (multiple-value-bind (,on-n ,on-r) (mcp-protocol:mcp-client-transport-handlers ,client)
         (multiple-value-bind (,st ,ct)
             (rpc-backend-stdio:make-stdio-rpc-pair :on-notification ,on-n :on-request ,on-r
                                                    :dispatch :concurrent)
           (setf (mcp-protocol:mcp-client-transport ,client) ,ct)
           (let ((,thread
                   (bt2:make-thread
                    (lambda ()
                      (ignore-errors
                       (rpc-protocol:rpc-serve
                        (lambda (method params)
                          (push (cons method params) (car ,seen))
                          (handler-case (mcp-protocol:dispatch-mcp-method ,server method params)
                            (mcp-protocol:mcp-error (c)
                              (error 'rpc-protocol:rpc-error
                                     :message (or (mcp-protocol:mcp-error-message c) "mcp error")
                                     :code (or (mcp-protocol:mcp-error-code c) -32603)
                                     :data (mcp-protocol:mcp-error-data c)))))
                        :transport ,st)))
                    :name "host test server")))
             (unwind-protect (progn ,@body)
               (rpc-backend-stdio:close-stdio-rpc-transport ,ct :force t)
               (ignore-errors (bt2:join-thread ,thread)))))))))

(defun %seen-capabilities (seen method)
  "The client capabilities of the newest METHOD request SEEN: initialize's
\"capabilities\", or the _meta's for a 2026-07-28 request."
  (let ((params (cdr (find method (car seen) :key #'car :test #'equal))))
    (and params
         (if (equal method "initialize")
             (gethash "capabilities" params)
             (let ((meta (gethash "_meta" params)))
               (and (hash-table-p meta) (gethash "io.modelcontextprotocol/clientCapabilities" meta)))))))

(defun %keys (object)
  (and (hash-table-p object) (loop for k being the hash-keys of object collect k)))

(defun %text (result)
  "The first text item of a tools/call RESULT."
  (let ((content (and (hash-table-p result) (gethash "content" result))))
    (and (plusp (length content)) (gethash "text" (elt content 0)))))

;;; 1. Capabilities

(deftest host-default-capabilities
  (let ((bare (make-instance 'mcp-protocol:mcp-client :name "c" :version "1")))
    (ok (hash-table-p (mcp-protocol:mcp-client-default-capabilities bare)))
    (ok (null (%keys (mcp-protocol:mcp-client-default-capabilities bare))) "no handler, nothing"))
  (let* ((c (make-instance 'mcp-protocol:mcp-client :name "c" :version "1"
                                                    :sampling-handler (%sampling-answer "x")
                                                    :elicitation-handler (lambda (p) (declare (ignore p)) (%obj))
                                                    :roots (list (mcp-protocol:make-mcp-root "file:///p"))))
         (caps (mcp-protocol:mcp-client-default-capabilities c)))
    (ok (equal '("sampling" "elicitation" "roots") (%keys caps)) "in that order")
    (ok (hash-table-p (gethash "sampling" caps)))
    (ok (hash-table-p (gethash "form" (gethash "elicitation" caps))) "form mode")
    (ok (null (gethash "url" (gethash "elicitation" caps))) "not url mode")
    (ok (null (gethash "tools" (gethash "sampling" caps))) "no sampling.tools")))

(deftest host-capabilities-in-the-modern-meta
  (with-host-pair (client seen :sampling (%sampling-answer "x")
                               :elicitation (lambda (p) (declare (ignore p)) (%obj "action" "decline")))
    (mcp-protocol:mcp-initialize client)
    (mcp-protocol:list-tools client)
    (let ((caps (%seen-capabilities seen "tools/list")))
      (ok (hash-table-p (gethash "sampling" caps)) "2026-07-28: every request's _meta")
      (ok (hash-table-p (gethash "form" (gethash "elicitation" caps)))))))

(deftest host-capabilities-in-the-legacy-initialize
  (with-host-pair (client seen :era :legacy :sampling (%sampling-answer "x"))
    (mcp-protocol:mcp-initialize client)
    (let ((caps (%seen-capabilities seen "initialize")))
      (ok (hash-table-p (gethash "sampling" caps)) "2025-11-25: initialize's capabilities")
      (ok (null (gethash "elicitation" caps)) "no elicitation handler, no elicitation"))))

(deftest host-explicit-capabilities-replace
  (with-host-pair (client seen :sampling (%sampling-answer "x"))
    (mcp-protocol:mcp-initialize client :capabilities (%obj "experimental" (%obj)))
    (mcp-protocol:list-tools client)
    (ok (equal '("experimental") (%keys (%seen-capabilities seen "tools/list")))
        "the host's own capabilities, whole")))

;;; 2-3. MRTR

(deftest host-mrtr-sampling-round-trip
  (let ((calls 0) (got nil))
    (with-host-pair (client seen :sampling (lambda (params)
                                             (incf calls)
                                             (setf got params)
                                             (funcall (%sampling-answer "short") params)))
      (mcp-protocol:mcp-initialize client)
      (ok (equal "summary: short" (%text (mcp-protocol:call-tool client "summarize" (%obj))))
          "the retried call's handler got the answer")
      (ok (= 1 calls) "asked once")
      (ok (eql 50 (and got (gethash "maxTokens" got))) "with the server's params"))))

(deftest host-mrtr-elicitation-round-trip
  (with-host-pair (client seen :elicitation (lambda (p) (declare (ignore p))
                                              (%obj "action" "accept" "content" (%obj "ok" t))))
    (mcp-protocol:mcp-initialize client)
    (ok (equal "answer: accept" (%text (mcp-protocol:call-tool client "confirm" (%obj)))))))

(deftest host-mrtr-two-rounds
  (let ((asked '()))
    (with-host-pair (client seen
                     :sampling (lambda (p) (push :sampling asked) (funcall (%sampling-answer "s") p))
                     :elicitation (lambda (p) (declare (ignore p)) (push :elicitation asked)
                                    (%obj "action" "decline")))
      (mcp-protocol:mcp-initialize client)
      (ok (equal "both: s / decline" (%text (mcp-protocol:call-tool client "both" (%obj))))
          "the third round carries both answers")
      (ok (equal '(:sampling :elicitation) (reverse asked)) "each asked once"))))

(deftest host-mrtr-round-limit
  (with-host-pair (client seen :elicitation (lambda (p) (declare (ignore p)) (%obj "action" "cancel")))
    (mcp-protocol:mcp-initialize client)
    (let ((c (handler-case (progn (mcp-protocol:call-tool client "greedy" (%obj)) nil)
               (mcp-protocol:mcp-error (e) e))))
      (ok c "an MCP-ERROR after +MAX-INPUT-ROUNDS+ rounds")
      (ok (and c (search "still needs input" (or (mcp-protocol:mcp-error-message c) "")))))
    (ok (<= (count "tools/call" (car seen) :key #'car :test #'equal)
            (1+ mcp-protocol:+max-input-rounds+)))))

(deftest host-mrtr-without-a-handler
  (with-host-pair (client seen)
    (mcp-protocol:mcp-initialize client)
    (ok (signals (mcp-protocol:call-tool client "summarize" (%obj)) 'mcp-protocol:mcp-error)
        "no sampling handler: -32601, as 0.4")))

(deftest host-input-response
  (multiple-value-bind (v p) (mcp-protocol:mcp-input-response "s1")
    (ok (null v))
    (ok (null p) "outside a request: nothing"))
  (let ((mcp-protocol:*mcp-input-responses* (%obj "s1" (%obj "action" "accept"))))
    (multiple-value-bind (v p) (mcp-protocol:mcp-input-response "s1")
      (ok (equal "accept" (gethash "action" v)))
      (ok p)))
  (let ((mcp-protocol:*mcp-input-responses* (%obj "s1" (%obj "action" "accept"))))
    (ok (equal "accept" (gethash "action" (mcp-protocol:request-elicitation (%obj) :id "s1")))
        "REQUEST-ELICITATION returns a response that is there")))
