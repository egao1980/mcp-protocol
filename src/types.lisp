(in-package #:mcp-protocol)

;;; Strings are not EQL across reloads — SBCL DEFCONSTANT-UNEQL. Boundp guard.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (boundp '+mcp-protocol-version+)
    (defconstant +mcp-protocol-version+ "2026-07-28"
      "Preferred / current MCP revision (modern, stateless)."))
  (unless (boundp '+mcp-legacy-protocol-version+)
    (defconstant +mcp-legacy-protocol-version+ "2025-11-25"
      "Last handshake-based MCP revision (legacy initialize).")))

(defconstant +mcp-error-unsupported-protocol-version+ -32022
  "JSON-RPC application error: UnsupportedProtocolVersion.")

(defconstant +mcp-default-ttl-ms+ 60000
  "SEP-2549 default freshness hint for static catalog / read results.")

(defparameter *supported-protocol-versions*
  (list +mcp-protocol-version+ +mcp-legacy-protocol-version+)
  "Newest-first versions this implementation speaks.")

(defvar *mcp-backend* nil)

(defclass mcp-peer ()
  ((name :initarg :name :accessor mcp-peer-name :initform "cl-stack-mcp")
   (version :initarg :version :accessor mcp-peer-version :initform "0.1.0")))

(defclass mcp-server (mcp-peer)
  ((protocol-version :initarg :protocol-version
                     :accessor mcp-server-protocol-version
                     :initform +mcp-protocol-version+)
   (tools :initarg :tools :accessor mcp-server-tools
          :initform (make-hash-table :test 'equal))
   (resources :initarg :resources :accessor mcp-server-resources
              :initform (make-hash-table :test 'equal))
   (prompts :initarg :prompts :accessor mcp-server-prompts
            :initform (make-hash-table :test 'equal))
   (instructions :initarg :instructions :accessor mcp-server-instructions
                 :initform nil)))

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
                 :initform nil)))

(defclass mcp-tool ()
  ((name :initarg :name :accessor mcp-tool-name)
   (description :initarg :description :accessor mcp-tool-description :initform "")
   (input-schema :initarg :input-schema :accessor mcp-tool-input-schema :initform nil)
   (handler :initarg :handler :accessor mcp-tool-handler)))

(defclass mcp-resource ()
  ((uri :initarg :uri :accessor mcp-resource-uri)
   (name :initarg :name :accessor mcp-resource-name)
   (description :initarg :description :accessor mcp-resource-description :initform "")
   (mime-type :initarg :mime-type :accessor mcp-resource-mime-type
              :initform "text/plain")
   (handler :initarg :handler :accessor mcp-resource-handler)))

(defclass mcp-prompt ()
  ((name :initarg :name :accessor mcp-prompt-name)
   (description :initarg :description :accessor mcp-prompt-description :initform "")
   (arguments :initarg :arguments :accessor mcp-prompt-arguments :initform nil)
   (handler :initarg :handler :accessor mcp-prompt-handler)))

(defclass mcp-backend ()
  ()
  (:documentation "Transport adapter. Concrete backends live in mcp-backend-*."))

(defun make-mcp-tool (name &key description input-schema handler)
  (make-instance 'mcp-tool
                 :name name
                 :description (or description "")
                 :input-schema input-schema
                 :handler handler))

(defun make-mcp-resource (uri &key name description mime-type handler)
  (make-instance 'mcp-resource
                 :uri uri
                 :name (or name uri)
                 :description (or description "")
                 :mime-type (or mime-type "text/plain")
                 :handler handler))

(defun make-mcp-prompt (name &key description arguments handler)
  (make-instance 'mcp-prompt
                 :name name
                 :description (or description "")
                 :arguments arguments
                 :handler handler))
