(in-package #:mcp-protocol/tests)

;;; 0.4.0 (#14): the client side over a real transport — rpc-backend-stdio's
;;; in-memory pipe pair, the client's reader thread, the server's serve loop in
;;; a thread of its own. Every wait is bounded.

(defclass recording-server (mcp-protocol:mcp-server)
  ((cancels :initform '() :accessor recording-server-cancels)))

(defmethod mcp-protocol:mcp-cancel :after ((server recording-server) &key request-id reason)
  (push (list request-id reason) (recording-server-cancels server)))

(defun %wait-until (fn &key (timeout 5))
  (loop with end = (+ (get-internal-real-time)
                      (* timeout internal-time-units-per-second))
        for v = (funcall fn)
        when v return v
        when (> (get-internal-real-time) end) return nil
        do (sleep 0.02)))

(defun %obj (&rest kvs) (apply #'mcp-protocol:json-object kvs))

(defun %fixture-server (&key (extra-tools 0))
  (let ((server (make-instance 'recording-server :name "fixture" :version "1.0.0")))
    (flet ((tool (&rest args) (mcp-protocol:register-tool server (apply #'mcp-protocol:make-mcp-tool args))))
      (tool "read_file" :title "Read a file" :description "reads"
                        :input-schema (%obj "type" "object")
                        :output-schema (%obj "type" "object" "properties" (%obj "text" (%obj "type" "string")))
                        :annotations (%obj "readOnlyHint" t "openWorldHint" nil)
                        :icons (vector (%obj "src" "https://example.invalid/i.png"))
                        :meta (%obj "x.example/owner" "tests")
                        :handler (lambda (args) (declare (ignore args)) "contents"))
      (tool "write_file" :description "writes" :input-schema (%obj "type" "object")
                         :annotations (%obj "readOnlyHint" nil "destructiveHint" t)
                         :handler (lambda (args) (declare (ignore args)) "ok"))
      (tool "app_only" :description "for the app" :input-schema (%obj "type" "object")
                       :meta (%obj "ui" (%obj "visibility" (vector "app")))
                       :handler (lambda (args) (declare (ignore args)) "app"))
      (tool "touch" :description "changes the tool list" :input-schema (%obj "type" "object")
                    :handler (lambda (args)
                               (declare (ignore args))
                               (mcp-protocol:mcp-log server "info" "touched" :logger "fixture")
                               (mcp-protocol:notify-tools-list-changed server)
                               "touched"))
      (tool "slow" :description "sleeps" :input-schema (%obj "type" "object")
                   :handler (lambda (args) (declare (ignore args)) (sleep 3) "late"))
      (dotimes (i extra-tools)
        (tool (format nil "extra_~2,'0d" i) :input-schema (%obj "type" "object")
                                             :handler (lambda (args) (declare (ignore args)) "x"))))
    server))

(defmacro with-wired-client ((client server &key (page-size 50) (extra-tools 0)
                                             on-tools-changed hook log-handler)
                             &body body)
  "CLIENT talks to SERVER over an in-memory stdio pair; the client's
transport handlers come from MCP-CLIENT-TRANSPORT-HANDLERS."
  (let ((st (gensym "ST")) (ct (gensym "CT")) (thread (gensym "THREAD"))
        (on-n (gensym)) (on-r (gensym)))
    `(let* ((,server (%fixture-server :extra-tools ,extra-tools))
            (,client (make-instance 'mcp-protocol:mcp-client
                                    :name "test-client" :version "0.1.0"
                                    :on-tools-changed ,on-tools-changed
                                    :notification-hook ,hook
                                    :log-handler ,log-handler)))
       (multiple-value-bind (,on-n ,on-r) (mcp-protocol:mcp-client-transport-handlers ,client)
         (multiple-value-bind (,st ,ct)
             (rpc-backend-stdio:make-stdio-rpc-pair :on-notification ,on-n :on-request ,on-r
                                                    :dispatch :concurrent)
           (setf (mcp-protocol:mcp-client-transport ,client) ,ct
                 (mcp-protocol:mcp-server-notifier ,server)
                 (lambda (method params)
                   (rpc-protocol:rpc-notify method params :transport ,st)))
           (let ((,thread (bt2:make-thread
                           (lambda ()
                             (let ((mcp-protocol:*mcp-page-size* ,page-size))
                               (ignore-errors (mcp-protocol:serve-mcp ,server :transport ,st))))
                           :name "fixture mcp server")))
             (unwind-protect
                  (progn (mcp-protocol:mcp-initialize ,client) ,@body)
               (rpc-backend-stdio:close-stdio-rpc-transport ,ct :force t)
               (ignore-errors (bt2:join-thread ,thread)))))))))

(defun %tool (tools name)
  (find name tools :key #'mcp-protocol:mcp-tool-name :test #'string=))

(deftest client-tool-fields-round-trip
  (with-wired-client (client server)
    (let* ((tools (mcp-protocol:list-tools client))
           (read (%tool tools "read_file")))
      (ok read)
      (ok (equal "Read a file" (mcp-protocol:mcp-tool-title read)))
      (ok (equal "reads" (mcp-protocol:mcp-tool-description read)))
      (ok (hash-table-p (mcp-protocol:mcp-tool-output-schema read)))
      (ok (hash-table-p (mcp-protocol:mcp-tool-annotations read)))
      (ok (equal "https://example.invalid/i.png"
                 (gethash "src" (elt (mcp-protocol:mcp-tool-icons read) 0))))
      (ok (equal "tests" (gethash "x.example/owner" (mcp-protocol:mcp-tool-meta read)))))))

(deftest-parametrize client-tool-annotations
    ((name key expected)
     ("read_file" "readOnlyHint" t)
     ("read_file" "openWorldHint" nil)
     ("read_file" "destructiveHint" nil)
     ("write_file" "readOnlyHint" nil)
     ("write_file" "destructiveHint" t)
     ("app_only" "readOnlyHint" nil))
  (with-wired-client (client server)
    (let ((tool (%tool (mcp-protocol:list-tools client) name)))
      (ok (eq expected (and (mcp-protocol:mcp-tool-annotation tool key) t))))))

(deftest-parametrize client-tool-read-only-p
    ((name expected)
     ("read_file" t)
     ("write_file" nil)
     ("touch" nil))
  (with-wired-client (client server)
    (ok (eq expected (mcp-protocol:mcp-tool-read-only-p
                      (%tool (mcp-protocol:list-tools client) name))))))

(deftest-parametrize tool-visible-to-model
    ((meta expected)
     (nil t)
     ((%obj "ui" (%obj "visibility" (vector "model" "app"))) t)
     ((%obj "ui" (%obj "visibility" (vector "app"))) nil)
     ((%obj "ui" (%obj "visibility" (vector))) nil)
     ((%obj "ui" (%obj "visibility" "app")) t)
     ((%obj "ui" (%obj)) t))
  (ok (eq expected (mcp-protocol:mcp-tool-visible-to-model-p
                    (mcp-protocol:make-mcp-tool "t" :meta meta)))))

(deftest tool-annotation-false-and-null
  (let ((tool (mcp-protocol:make-mcp-tool "t" :annotations (%obj "readOnlyHint" :null))))
    (ok (null (mcp-protocol:mcp-tool-annotation tool "readOnlyHint")))
    (ok (null (mcp-protocol:mcp-tool-read-only-p tool))))
  (ok (null (mcp-protocol:mcp-tool-read-only-p (mcp-protocol:make-mcp-tool "bare")))))

(deftest client-list-tools-next-cursor
  (with-wired-client (client server :page-size 3 :extra-tools 4)
    (multiple-value-bind (page next) (mcp-protocol:list-tools client)
      (ok (= 3 (length page)))
      (ok (stringp next))
      (multiple-value-bind (page2 next2) (mcp-protocol:list-tools client :cursor next)
        (ok (= 3 (length page2)))
        (ok (stringp next2))))))

(deftest client-list-all-tools-follows-cursor
  (with-wired-client (client server :page-size 3 :extra-tools 4)
    (let ((all (mcp-protocol:list-all-tools client)))
      (ok (= 9 (length all)))
      (ok (= 9 (length (remove-duplicates (mapcar #'mcp-protocol:mcp-tool-name all)
                                          :test #'string=)))))))

(deftest client-list-all-tools-page-limit
  (with-wired-client (client server :page-size 2 :extra-tools 4)
    (ok (signals (mcp-protocol:list-all-tools client :max-pages 2)
                 'mcp-protocol:mcp-pagination-error))
    (let ((partial (handler-bind ((mcp-protocol:mcp-pagination-error
                                    (lambda (c) (declare (ignore c)) (invoke-restart 'continue))))
                     (mcp-protocol:list-all-tools client :max-pages 2))))
      (ok (= 4 (length partial))))))

(deftest client-list-changed-bumps-generation
  (let ((changed 0) (seen '()) (logs '()))
    (with-wired-client (client server
                        :on-tools-changed (lambda (c) (declare (ignore c)) (incf changed))
                        :hook (lambda (c method params)
                                (declare (ignore c params))
                                (push method seen))
                        :log-handler (lambda (m) (push m logs)))
      (ok (= 0 (mcp-protocol:mcp-client-tools-generation client)))
      (mcp-protocol:call-tool client "touch" (%obj))
      (ok (%wait-until (lambda () (= 1 (mcp-protocol:mcp-client-tools-generation client)))))
      (ok (= 1 changed))
      (ok (%wait-until (lambda () (member "notifications/tools/list_changed" seen :test #'string=))))
      (ok (%wait-until (lambda () (member "notifications/message" seen :test #'string=))))
      (ok (= 1 (length logs)))
      (ok (equal "touched" (mcp-protocol::mcp-log-message-data (first logs)))))))

(deftest client-handle-notification-errors-dropped
  (let ((client (make-instance 'mcp-protocol:mcp-client
                               :on-tools-changed (lambda (c) (declare (ignore c)) (error "boom")))))
    (ok (eq t (mcp-protocol:mcp-client-handle-notification
               client "notifications/tools/list_changed" (%obj))))
    (ok (= 1 (mcp-protocol:mcp-client-tools-generation client)))
    (ok (eq t (mcp-protocol:mcp-client-handle-notification client "notifications/unknown" nil)))))

(deftest client-handle-request
  (let ((client (make-instance 'mcp-protocol:mcp-client
                               :sampling-handler (lambda (params)
                                                   (declare (ignore params))
                                                   (%obj "role" "assistant" "model" "m"))
                               :roots (list (mcp-protocol:make-mcp-root "file:///tmp/" :name "tmp")))))
    (ok (hash-table-p (mcp-protocol:mcp-client-handle-request client "ping" nil)))
    (ok (equal "m" (gethash "model" (mcp-protocol:mcp-client-handle-request
                                     client "sampling/createMessage" (%obj "messages" #())))))
    (ok (equal "file:///tmp/"
               (gethash "uri" (elt (gethash "roots" (mcp-protocol:mcp-client-handle-request
                                                     client "roots/list" nil))
                                   0))))
    (handler-case (progn (mcp-protocol:mcp-client-handle-request client "elicitation/create" (%obj))
                         (ok nil "no elicitation handler must be -32601"))
      (rpc-protocol:rpc-error (e) (ok (eql -32601 (rpc-protocol:rpc-error-code e)))))
    (handler-case (progn (mcp-protocol:mcp-client-handle-request client "x/unknown" nil)
                         (ok nil "an unknown method must be -32601"))
      (rpc-protocol:rpc-error (e) (ok (eql -32601 (rpc-protocol:rpc-error-code e)))))))

(deftest client-call-tool-timeout-cancels
  (with-wired-client (client server)
    (let ((start (get-internal-real-time)))
      (handler-case (progn (mcp-protocol:call-tool client "slow" (%obj) :timeout 0.5)
                           (ok nil "slow must time out"))
        (mcp-protocol:mcp-timeout (e)
          (ok (equal "tools/call" (mcp-protocol:mcp-timeout-method e)))
          (ok (= 0.5 (mcp-protocol:mcp-timeout-seconds e)))
          (ok (stringp (mcp-protocol:mcp-timeout-request-id e)))
          (ok (uiop:string-prefix-p "mcp-" (mcp-protocol:mcp-timeout-request-id e)))
          (ok (eql -32001 (mcp-protocol:mcp-error-code e)))
          (ok (< (/ (- (get-internal-real-time) start) internal-time-units-per-second) 2.5))
          (ok (%wait-until (lambda ()
                             (find (mcp-protocol:mcp-timeout-request-id e)
                                   (recording-server-cancels server)
                                   :key #'first :test #'equal))))
          (ok (equal "timeout" (second (first (recording-server-cancels server))))))))))

(deftest client-call-tool-within-timeout
  (with-wired-client (client server)
    (let ((result (mcp-protocol:call-tool client "read_file" (%obj) :timeout 5)))
      (ok (equal "contents" (gethash "text" (elt (gethash "content" result) 0)))))
    (ok (null (recording-server-cancels server)))))

(deftest client-handler-payload-readers-exported
  "0.4.1: a log or progress handler reads what it gets with exported readers."
  (let* ((logs '()) (progress '())
         (client (make-instance 'mcp-protocol:mcp-client
                                :log-handler (lambda (m) (push m logs))
                                :progress-handler (lambda (p) (push p progress)))))
    (mcp-protocol:mcp-client-handle-notification
     client "notifications/message" (%obj "level" "warning" "logger" "srv" "data" "careful"))
    (mcp-protocol:mcp-client-handle-notification
     client "notifications/progress" (%obj "progressToken" "t1" "progress" 1 "total" 4 "message" "a quarter"))
    (let ((m (first logs)))
      (ok (equal "warning" (mcp-protocol:mcp-log-message-level m)))
      (ok (equal "srv" (mcp-protocol:mcp-log-message-logger m)))
      (ok (equal "careful" (mcp-protocol:mcp-log-message-data m))))
    (let ((p (first progress)))
      (ok (equal "t1" (mcp-protocol:mcp-progress-token p)))
      (ok (eql 1 (mcp-protocol:mcp-progress-value p)))
      (ok (eql 4 (mcp-protocol:mcp-progress-total p)))
      (ok (equal "a quarter" (mcp-protocol:mcp-progress-message p))))))
