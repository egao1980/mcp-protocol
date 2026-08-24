(in-package #:mcp-protocol)

;;; Strings are not EQL across reloads — SBCL DEFCONSTANT-UNEQL. Boundp guard.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (boundp '+mcp-protocol-version+)
    (defconstant +mcp-protocol-version+ "2026-07-28"
      "Preferred / current MCP revision (modern, stateless)."))
  (unless (boundp '+mcp-legacy-protocol-version+)
    (defconstant +mcp-legacy-protocol-version+ "2025-11-25"
      "Last handshake-based MCP revision (legacy initialize).")))

(defconstant +mcp-error-header-mismatch+ -32020
  "JSON-RPC: HeaderMismatch.")
(defconstant +mcp-error-missing-client-capability+ -32021
  "JSON-RPC: MissingRequiredClientCapability.")
(defconstant +mcp-error-unsupported-protocol-version+ -32022
  "JSON-RPC: UnsupportedProtocolVersion.")

(defconstant +mcp-default-ttl-ms+ 60000
  "SEP-2549 default freshness hint for static catalog / read results.")

(defparameter *supported-protocol-versions*
  (list +mcp-protocol-version+ +mcp-legacy-protocol-version+)
  "Newest-first versions this implementation speaks.")

(defparameter *mcp-log-levels*
  '("debug" "info" "notice" "warning" "error" "critical" "alert" "emergency"))

(defparameter *mcp-page-size* 100
  "Default catalog page size for tools/resources/prompts/templates list.")

(defvar *mcp-backend* nil)

(defun json-object (&rest kvs)
  (let ((h (make-hash-table :test 'equal)))
    (loop for (k v) on kvs by #'cddr
          unless (or (null k) (eq v :omit))
            do (setf (gethash k h) v))
    h))

(defun param (obj key &optional default)
  (cond
    ((null obj) default)
    ((hash-table-p obj) (gethash key obj default))
    ((listp obj)
     (let ((cell (assoc key obj :test #'equal)))
       (if cell (cdr cell) default)))
    (t default)))

(defun %vec-map (seq fn)
  (map 'list fn (or seq #())))

(defun %as-list (seq)
  (cond
    ((null seq) nil)
    ((vectorp seq) (coerce seq 'list))
    ((listp seq) seq)
    (t (list seq))))

(defclass mcp-peer ()
  ((name :initarg :name :accessor mcp-peer-name :initform "cl-stack-mcp")
   (version :initarg :version :accessor mcp-peer-version :initform "0.1.0")
   (title :initarg :title :accessor mcp-peer-title :initform nil)
   (icons :initarg :icons :accessor mcp-peer-icons :initform nil)
   (website-url :initarg :website-url :accessor mcp-peer-website-url :initform nil)))

(defclass mcp-annotations ()
  ((audience :initarg :audience :accessor mcp-annotations-audience :initform nil)
   (priority :initarg :priority :accessor mcp-annotations-priority :initform nil)
   (last-modified :initarg :last-modified :accessor mcp-annotations-last-modified
                  :initform nil)))

(defun make-mcp-annotations (&key audience priority last-modified)
  (make-instance 'mcp-annotations
                 :audience audience :priority priority
                 :last-modified last-modified))

(defclass mcp-icon ()
  ((src :initarg :src :accessor mcp-icon-src)
   (mime-type :initarg :mime-type :accessor mcp-icon-mime-type :initform nil)
   (sizes :initarg :sizes :accessor mcp-icon-sizes :initform nil)
   (theme :initarg :theme :accessor mcp-icon-theme :initform nil)))

(defun make-mcp-icon (src &key mime-type sizes theme)
  (make-instance 'mcp-icon :src src :mime-type mime-type :sizes sizes :theme theme))

(defclass mcp-content ()
  ((annotations :initarg :annotations :accessor mcp-content-annotations :initform nil)
   (meta :initarg :meta :accessor mcp-content-meta :initform nil)))

(defclass mcp-text-content (mcp-content)
  ((text :initarg :text :accessor mcp-text-content-text)))

(defclass mcp-image-content (mcp-content)
  ((data :initarg :data :accessor mcp-image-content-data)
   (mime-type :initarg :mime-type :accessor mcp-image-content-mime-type)))

(defclass mcp-audio-content (mcp-content)
  ((data :initarg :data :accessor mcp-audio-content-data)
   (mime-type :initarg :mime-type :accessor mcp-audio-content-mime-type)))

(defclass mcp-embedded-resource (mcp-content)
  ((resource :initarg :resource :accessor mcp-embedded-resource-resource)))

(defclass mcp-resource-link (mcp-content)
  ((uri :initarg :uri :accessor mcp-resource-link-uri)
   (name :initarg :name :accessor mcp-resource-link-name)
   (title :initarg :title :accessor mcp-resource-link-title :initform nil)
   (description :initarg :description :accessor mcp-resource-link-description
                :initform nil)
   (mime-type :initarg :mime-type :accessor mcp-resource-link-mime-type :initform nil)
   (icons :initarg :icons :accessor mcp-resource-link-icons :initform nil)
   (size :initarg :size :accessor mcp-resource-link-size :initform nil)))

(defclass mcp-server (mcp-peer)
  ((protocol-version :initarg :protocol-version
                     :accessor mcp-server-protocol-version
                     :initform +mcp-protocol-version+)
   (tools :initarg :tools :accessor mcp-server-tools
          :initform (make-hash-table :test 'equal))
   (resources :initarg :resources :accessor mcp-server-resources
              :initform (make-hash-table :test 'equal))
   (resource-templates :initarg :resource-templates
                       :accessor mcp-server-resource-templates
                       :initform (make-hash-table :test 'equal))
   (prompts :initarg :prompts :accessor mcp-server-prompts
            :initform (make-hash-table :test 'equal))
   (instructions :initarg :instructions :accessor mcp-server-instructions
                 :initform nil)
   (log-level :initarg :log-level :accessor mcp-server-log-level :initform nil)
   (subscriptions :initarg :subscriptions :accessor mcp-server-subscriptions
                  :initform (make-hash-table :test 'equal))))

(defclass mcp-client (mcp-peer)
  ((transport :initarg :transport :accessor mcp-client-transport :initform nil)
   (era :initarg :era :accessor mcp-client-era :initform :unknown)
   (protocol-version :initarg :protocol-version
                     :accessor mcp-client-protocol-version
                     :initform +mcp-protocol-version+)
   (client-capabilities :initarg :client-capabilities
                        :accessor mcp-client-client-capabilities
                        :initform nil)
   (server-info :initarg :server-info :accessor mcp-client-server-info :initform nil)
   (server-capabilities :initarg :server-capabilities
                        :accessor mcp-client-server-capabilities
                        :initform nil)
   (instructions :initarg :instructions :accessor mcp-client-instructions
                 :initform nil)
   (roots :initarg :roots :accessor mcp-client-roots :initform nil)
   (sampling-handler :initarg :sampling-handler
                     :accessor mcp-client-sampling-handler :initform nil)
   (elicitation-handler :initarg :elicitation-handler
                        :accessor mcp-client-elicitation-handler :initform nil)
   (log-handler :initarg :log-handler :accessor mcp-client-log-handler :initform nil)
   (progress-handler :initarg :progress-handler
                     :accessor mcp-client-progress-handler :initform nil)))

(defclass mcp-tool ()
  ((name :initarg :name :accessor mcp-tool-name)
   (title :initarg :title :accessor mcp-tool-title :initform nil)
   (description :initarg :description :accessor mcp-tool-description :initform "")
   (input-schema :initarg :input-schema :accessor mcp-tool-input-schema :initform nil)
   (output-schema :initarg :output-schema :accessor mcp-tool-output-schema :initform nil)
   (annotations :initarg :annotations :accessor mcp-tool-annotations :initform nil)
   (icons :initarg :icons :accessor mcp-tool-icons :initform nil)
   (handler :initarg :handler :accessor mcp-tool-handler)
   (compiled-input-schema :initarg :compiled-input-schema
                          :accessor mcp-tool-compiled-input-schema
                          :initform nil)))

(defclass mcp-resource ()
  ((uri :initarg :uri :accessor mcp-resource-uri)
   (name :initarg :name :accessor mcp-resource-name)
   (title :initarg :title :accessor mcp-resource-title :initform nil)
   (description :initarg :description :accessor mcp-resource-description :initform "")
   (mime-type :initarg :mime-type :accessor mcp-resource-mime-type
              :initform "text/plain")
   (annotations :initarg :annotations :accessor mcp-resource-annotations :initform nil)
   (icons :initarg :icons :accessor mcp-resource-icons :initform nil)
   (size :initarg :size :accessor mcp-resource-size :initform nil)
   (handler :initarg :handler :accessor mcp-resource-handler)))

(defclass mcp-resource-template ()
  ((uri-template :initarg :uri-template :accessor mcp-resource-template-uri)
   (name :initarg :name :accessor mcp-resource-template-name)
   (title :initarg :title :accessor mcp-resource-template-title :initform nil)
   (description :initarg :description :accessor mcp-resource-template-description
                :initform "")
   (mime-type :initarg :mime-type :accessor mcp-resource-template-mime-type
              :initform nil)
   (annotations :initarg :annotations :accessor mcp-resource-template-annotations
                :initform nil)
   (icons :initarg :icons :accessor mcp-resource-template-icons :initform nil)
   (complete :initarg :complete :accessor mcp-resource-template-complete
             :initform nil)))

(defclass mcp-prompt ()
  ((name :initarg :name :accessor mcp-prompt-name)
   (title :initarg :title :accessor mcp-prompt-title :initform nil)
   (description :initarg :description :accessor mcp-prompt-description :initform "")
   (arguments :initarg :arguments :accessor mcp-prompt-arguments :initform nil)
   (icons :initarg :icons :accessor mcp-prompt-icons :initform nil)
   (complete :initarg :complete :accessor mcp-prompt-complete :initform nil)
   (handler :initarg :handler :accessor mcp-prompt-handler)))

(defclass mcp-root ()
  ((uri :initarg :uri :accessor mcp-root-uri)
   (name :initarg :name :accessor mcp-root-name :initform nil)))

(defclass mcp-sampling-message ()
  ((role :initarg :role :accessor mcp-sampling-message-role :initform "user")
   (content :initarg :content :accessor mcp-sampling-message-content)))

(defclass mcp-model-preferences ()
  ((hints :initarg :hints :accessor mcp-model-preferences-hints :initform nil)
   (cost-priority :initarg :cost-priority
                  :accessor mcp-model-preferences-cost-priority :initform nil)
   (speed-priority :initarg :speed-priority
                   :accessor mcp-model-preferences-speed-priority :initform nil)
   (intelligence-priority :initarg :intelligence-priority
                          :accessor mcp-model-preferences-intelligence-priority
                          :initform nil)))

(defclass mcp-sampling-request ()
  ((messages :initarg :messages :accessor mcp-sampling-request-messages)
   (model-preferences :initarg :model-preferences
                      :accessor mcp-sampling-request-model-preferences :initform nil)
   (system-prompt :initarg :system-prompt
                  :accessor mcp-sampling-request-system-prompt :initform nil)
   (max-tokens :initarg :max-tokens :accessor mcp-sampling-request-max-tokens
               :initform nil)
   (include-context :initarg :include-context
                    :accessor mcp-sampling-request-include-context :initform nil)
   (temperature :initarg :temperature :accessor mcp-sampling-request-temperature
                :initform nil)
   (stop-sequences :initarg :stop-sequences
                   :accessor mcp-sampling-request-stop-sequences :initform nil)
   (metadata :initarg :metadata :accessor mcp-sampling-request-metadata :initform nil)
   (tools :initarg :tools :accessor mcp-sampling-request-tools :initform nil)
   (tool-choice :initarg :tool-choice :accessor mcp-sampling-request-tool-choice
                :initform nil)))

(defclass mcp-elicit-request ()
  ((mode :initarg :mode :accessor mcp-elicit-request-mode :initform "form")
   (message :initarg :message :accessor mcp-elicit-request-message)
   (requested-schema :initarg :requested-schema
                     :accessor mcp-elicit-request-schema :initform nil)
   (url :initarg :url :accessor mcp-elicit-request-url :initform nil)
   (elicitation-id :initarg :elicitation-id
                   :accessor mcp-elicit-request-id :initform nil)))

(defclass mcp-completion-ref ()
  ((type :initarg :type :accessor mcp-completion-ref-type)
   (name :initarg :name :accessor mcp-completion-ref-name :initform nil)
   (uri :initarg :uri :accessor mcp-completion-ref-uri :initform nil)))

(defclass mcp-log-message ()
  ((level :initarg :level :accessor mcp-log-message-level)
   (logger :initarg :logger :accessor mcp-log-message-logger :initform nil)
   (data :initarg :data :accessor mcp-log-message-data)))

(defclass mcp-progress ()
  ((progress-token :initarg :progress-token :accessor mcp-progress-token)
   (progress :initarg :progress :accessor mcp-progress-value)
   (total :initarg :total :accessor mcp-progress-total :initform nil)
   (message :initarg :message :accessor mcp-progress-message :initform nil)))

(defclass mcp-subscription ()
  ((id :initarg :id :accessor mcp-subscription-id)
   (tools-list-changed :initarg :tools-list-changed
                       :accessor mcp-subscription-tools-list-changed :initform nil)
   (resources-list-changed :initarg :resources-list-changed
                           :accessor mcp-subscription-resources-list-changed
                           :initform nil)
   (prompts-list-changed :initarg :prompts-list-changed
                         :accessor mcp-subscription-prompts-list-changed
                         :initform nil)
   (resource-uris :initarg :resource-uris
                  :accessor mcp-subscription-resource-uris :initform nil)))

(defclass mcp-backend ()
  ()
  (:documentation "Transport adapter. Concrete backends live in mcp-backend-*."))

(defun make-mcp-tool (name &key description title input-schema output-schema
                             annotations icons handler)
  (make-instance 'mcp-tool
                 :name name
                 :title title
                 :description (or description "")
                 :input-schema input-schema
                 :output-schema output-schema
                 :annotations annotations
                 :icons icons
                 :handler handler))

(defun make-mcp-resource (uri &key name title description mime-type annotations
                                icons size handler)
  (make-instance 'mcp-resource
                 :uri uri
                 :name (or name uri)
                 :title title
                 :description (or description "")
                 :mime-type (or mime-type "text/plain")
                 :annotations annotations
                 :icons icons
                 :size size
                 :handler handler))

(defun make-mcp-resource-template (uri-template &key name title description
                                                  mime-type annotations icons
                                                  complete)
  (make-instance 'mcp-resource-template
                 :uri-template uri-template
                 :name (or name uri-template)
                 :title title
                 :description (or description "")
                 :mime-type mime-type
                 :annotations annotations
                 :icons icons
                 :complete complete))

(defun make-mcp-prompt (name &key description title arguments icons complete handler)
  (make-instance 'mcp-prompt
                 :name name
                 :title title
                 :description (or description "")
                 :arguments arguments
                 :icons icons
                 :complete complete
                 :handler handler))

(defun make-mcp-root (uri &key name)
  (make-instance 'mcp-root :uri uri :name name))

(defun make-mcp-sampling-request (messages &key model-preferences system-prompt
                                             max-tokens include-context
                                             temperature stop-sequences
                                             metadata tools tool-choice)
  (make-instance 'mcp-sampling-request
                 :messages messages
                 :model-preferences model-preferences
                 :system-prompt system-prompt
                 :max-tokens max-tokens
                 :include-context include-context
                 :temperature temperature
                 :stop-sequences stop-sequences
                 :metadata metadata
                 :tools tools
                 :tool-choice tool-choice))

(defun make-mcp-elicit-request (message &key (mode "form") requested-schema url
                                          elicitation-id)
  (make-instance 'mcp-elicit-request
                 :mode mode
                 :message message
                 :requested-schema requested-schema
                 :url url
                 :elicitation-id elicitation-id))

(defun make-mcp-completion-ref (type &key name uri)
  (make-instance 'mcp-completion-ref :type type :name name :uri uri))

(defun make-mcp-log-message (level data &key logger)
  (make-instance 'mcp-log-message :level level :data data :logger logger))

(defun make-mcp-progress (progress-token progress &key total message)
  (make-instance 'mcp-progress
                 :progress-token progress-token
                 :progress progress
                 :total total
                 :message message))

(defun make-mcp-subscription (id &key tools-list-changed resources-list-changed
                                   prompts-list-changed resource-uris)
  (make-instance 'mcp-subscription
                 :id id
                 :tools-list-changed tools-list-changed
                 :resources-list-changed resources-list-changed
                 :prompts-list-changed prompts-list-changed
                 :resource-uris resource-uris))
