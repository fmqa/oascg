;; OAS CODE GENERATOR - CORE DATA STRUCTURES & FUNCTIONS
;;
;; This module contains functions for converting OAS structures to concrete COMMON LISP types.
;;
;; +--------------+-------------+------------------------+--------------------------------------+
;; |OAS TYPE      |CLASS        |SLOTS                   |DESCRIPTION                           |
;; +--------------+-------------+------------------------+--------------------------------------+
;; |integer       |SIMPLE-SCHEMA|:type :integer          |-                                     |
;; |              |             |                        |                                      |
;; +--------------+-------------+------------------------+--------------------------------------+
;; |number        |SIMPLE-SCHEMA|:type :number           |-                                     |
;; |              |             |                        |                                      |
;; +--------------+-------------+------------------------+--------------------------------------+
;; |string        |SIMPLE-SCHEMA|:type :string           |-                                     |
;; |              |             |                        |                                      |
;; +--------------+-------------+------------------------+--------------------------------------+
;; |boolean       |SIMPLE-SCHEMA|:type :boolean          |-                                     |
;; |              |             |                        |                                      |
;; +--------------+-------------+------------------------+--------------------------------------+
;; |array         |ARRAY-SCHEMA |:items SCHEMA           |SCHEMA is the schema of the array     |
;; |              |             |                        |element(s)                            |
;; |              |             |                        |                                      |
;; +--------------+-------------+------------------------+--------------------------------------+
;; |object        |OBJECT-SCHEMA|:properties HASH-TABLE  |HASH-TABLE is a                       |
;; |              |             |                        |                                      |
;; |              |             |                        |PROPERTY-NAME -> (SCHEMA . REQUIRED)  |
;; |              |             |                        |                                      |
;; |              |             |                        |mapping.                              |
;; +--------------+-------------+------------------------+--------------------------------------+
;; |$ref          |REF-SCHEMA   |:uri URI                |URI is a STRING referencing a local   |
;; |              |             |:resolved SCHEMA-OR-NIL |or remote schema object.              |
;; |              |             |                        |                                      |
;; |              |             |                        |If SCHEMA-OR-NIL is non-NIL, the      |
;; |              |             |                        |schema is resolved. Otherwise, the    |
;; |              |             |                        |schem is unresolved                   |
;; +--------------+-------------+------------------------+--------------------------------------+
;; |anyOf         |COMB-SCHEMA  |:op :any-of             |-                                     |
;; +--------------+-------------+------------------------+--------------------------------------+
;; |oneOf         |COMB-SCHEMA  |:op :one-of             |-                                     |
;; +--------------+-------------+------------------------+--------------------------------------+
;; |allOf         |COMB-SCHEMA  |:op :all-of             |-                                     |
;; +--------------+-------------+------------------------+--------------------------------------+
;;
;; An abstract factory SCHEMA-FROM-HT is provided to translate from deserialized
;; OAS datums (represented as hash tables) into objects of one of the previous
;; classes.

(defpackage oascg-core
  (:use :cl)
  (:export #:unresolved-schema-error #:unresolved-schema-uri
           #:nullable-schema #:schema-nullable
           #:simple-schema #:simple-schema-type #:simple-schema-format #:simple-schema-enum
           #:array-schema #:array-schema-items
           #:object-schema #:object-schema-properties
           #:ref-schema #:ref-schema-uri #:ref-schema-resolved #:schema-ref-resolve #:schema-ensure-resolved
           #:discriminated-property #:discriminated-property-name #:discriminated-property-mapping
           #:comb-schema #:comb-schema-op #:comb-schema-operands #:comb-schema-discriminator
           #:component #:component-name #:component-schema #:component-ensure-schema
           #:make-component #:register-component-schema #:register-components-from-ht
           #:with-components-from-ht #:map-components #:resolve-local-component-by-uri #:resolve-local-component-handler
           #:schema-iterator
           #:schema-unboundedp
           #:schema-from-ht))

(in-package :oascg-core)

;; Should be bound to a hash table representing the relationship
;;
;;   COMPONENT-NAME -> COMPONENT
;;
;; Where SCHEMA-NAME is a STRING and COMPONENT is a COMPONENT object.
(defparameter *components* nil "Components by name.")

;; UNRESOLVED-SCHEMA-ERROR is signalled when a $ref schema could not be resolved.
;; The caller can invoke the restarts
;;
;;    USE-VALUE to use the given value as a replacement schema.
;;    CONTINUE to ignore the error.
;;
(define-condition unresolved-schema-error (error)
  ((uri :initarg :uri :reader unresolved-schema-uri
        :documentation "URI of the unresolved schema.")))

;; Superclass for schemas that have the 'nullable' OAS property.
;; This included integer, number, string, array, object schemas, as well
;; as the unbounded schema.
(defclass nullable-schema ()
  ((nullable :initarg :nullable :accessor schema-nullable :initform nil
             :documentation "Indicates whether schema is nullable.")))

;; Scalar schema type.
(defclass simple-schema (nullable-schema)
  ((type :initarg :type :accessor simple-schema-type :documentation "Schema type.")
   (format :initarg :format :accessor simple-schema-format :initform nil :documentation "Schema format.")
   (enum :initarg :enum :accessor simple-schema-enum :initform nil :documentation "Enumeration vector.")))

;; Array schema type.
(defclass array-schema (nullable-schema)
  ((items :initarg :items :accessor array-schema-items :documentation "Schema of array elements.")))

;; Object schema type.
(defclass object-schema (nullable-schema)
  ((properties :initarg :properties
               :accessor object-schema-properties
               :initform (make-hash-table :test 'equal)
               :documentation "Schema of each property, with the property name as key.")))

;; $ref pseudo-schema type.
(defclass ref-schema ()
  ((uri :initarg :uri :accessor ref-schema-uri :documentation "Reference target URI.")
   (resolved :initarg :resolved :accessor ref-schema-resolved :initform nil
             :documentation "Resolved target schema.")))

;; Sum type discriminator.
(defclass discriminated-property ()
  ((name
    :initarg :property-name
    :accessor discriminated-property-name
    :documentation "Discriminator property name.")
   (mapping
    :initarg :mapping
    :accessor discriminated-property-mapping
    :initform nil
    :documentation "Maps property values to schemas.")))

;; Combinatoric pseudo-schema.
;;
;; This represents allOf, oneOf, and anyOf schemas.
(defclass comb-schema ()
  ((op :initarg :op :accessor comb-schema-op :documentation "Schema combinator operator.")
   (operands :initarg :operands :accessor comb-schema-operands :documentation "Schemas to combine.")
   (discriminator :initarg :discriminator :accessor comb-schema-discriminator :initform nil
                  :documentation "Sum type discriminator property.")))

;; Named component schema type.
(defclass component ()
  ((name :initarg :name :accessor component-name
         :documentation "Component name.")
   (schema :initarg :schema :accessor component-schema :initform nil
           :documentation "Component schema or schema factory.")))

;; Returns the schema of a COMPONENT object, ensuring that a lazy schema is evaluated.
(defun component-ensure-schema (comp)
  (if (functionp (component-schema comp))
      (setf (component-schema comp) (funcall (component-schema comp)))
      (component-schema comp)))

;; Wraps a schema for lazy evaluation.
;;
;; If the given OAS schema is a hash table representing an OAS schema, it is wrapped
;; in a function that returns a schema object when called.
;;
;; Otherwise, it is equivalent to the identity function.
(defun wrap-schema (schema)
  (etypecase schema
    (hash-table (lambda () (schema-from-ht schema)))
    (t schema)))

;; Factory for component objects.
;;
;; The schema is wrapped using WRAP-SCHEMA to enable lazy evaluation.
(defun make-component (name schema)
  (make-instance 'component :name name :schema (wrap-schema schema)))

;; Adds the given schema as local component with name NAME under *COMPONENTS*.
(defun register-component-schema (name schema)
  (let ((key (format nil "#/components/schemas/~A" name)))
    (setf (gethash key *components*) (make-component name schema))))

;; Given a hash table of the form
;;
;;     SCHEMA-NAME -> SCHEMA
;;
;; Registers all SCHEMA objects under *COMPONENTS*.
(defun register-components-from-ht (table)
  (maphash #'register-component-schema table))

;; Binds *COMPONENTS* to a hash table representing a URI -> COMPONENT
;; shaped registry of OAS components.
;;
;; The registry is populated from the OAS components document represented by
;; the given TABLE.
(defun with-components-from-ht (table f)
  (let ((*components* (make-hash-table :test 'equal)))
    (register-components-from-ht table)
    (funcall f)))

;; Calls the given function on every component in the *COMPONENTS* registry.
(defun map-components (f)
  (when *components*
    (maphash (lambda (name comp) (declare (ignore name)) (funcall f comp))
             *components*)))

;; Returns T if the given URI is a local component URI, i.e.
;; if it starts with "#/components/schemas/".
(defun local-component-urip (uri)
  (= 0 (search "#/components/schemas/" uri)))

;; If the given URI is a local component URI, look it up in the
;; *COMPONENTS* registry.
;; Otherwise, return NIL.
(defun resolve-local-component-by-uri (uri)
  (and (local-component-urip uri)
       (gethash uri *components*)))

;; Creates a handler function for UNRESOLVED-SCHEMA-URI errors.
;;
;; If the URI of the unresolved schema refers to a local component, it is
;; looked up in the *COMPONENTS* registry, and passed as an argument to the
;; USE-VALUE restart.
;; Otherwise, the NEXT function - if given - is called with the URI.
;;
;; If NEXT is given, this implements a "chain of responsibility" for resolving
;; unresolved URIs.
(defun resolve-local-component-handler (&optional next)
  (lambda (unresolved-err)
    (let* ((uri (unresolved-schema-uri unresolved-err))
           (found (resolve-local-component-by-uri uri)))
      (if found
          (invoke-restart 'use-value found)
          (and next (funcall next uri))))))

;; Add a schema PROPERTY-SCHEMA as a subschema representing a named property
;; NAME to OBJECT-SCHEMA.
(defun object-schema-add-property (object-schema name property-schema &optional required)
  (setf (gethash name (object-schema-properties object-schema))
        (cons property-schema required)))

(defgeneric schema-iterator (parent)
  (:documentation "Returns an iterator function that calls an argument function with all subschemas"))

(defmethod schema-iterator ((parent array-schema))
  (lambda (it) (funcall it (array-schema-items parent))))

(defmethod schema-iterator ((parent object-schema))
  (lambda (it)
    (when (object-schema-properties parent)
      (maphash (lambda (name pair)
                 (destructuring-bind (schema &rest required) pair
                   (funcall it schema name required)))
               (object-schema-properties parent)))))

(defgeneric schema-unboundedp (schema) (:documentation "Tests whether a given schema is unbounded"))
(defmethod schema-unboundedp (schema) nil)
(defmethod schema-unboundedp ((schema object-schema))
  (and (object-schema-properties schema)
       (= 0 (hash-table-count (object-schema-properties schema)))))

;; Factory for REF-SCHEMA objects.
;;
;; TABLE should be an OAS hash table / dictionary.
(defun schema-ref-from-ht (table)
  (let ((uri (gethash "$ref" table)))
    (if uri
        (make-instance 'ref-schema :uri uri)
        (error "$ref has no target URI"))))

;; Resolves a REF-SCHEMA object.
;;
;; May signal an UNRESOLVED-SCHEMA-ERROR condition.
(defun schema-ref-resolve (schema)
  (or (ref-schema-resolved schema)
      (setf (ref-schema-resolved schema)
            (restart-case (error 'unresolved-schema-error :uri (ref-schema-uri schema))
              (use-value (value) :report "Use a new schema" value)
              (continue () :report "Continue without resolving" nil)))))

(defgeneric schema-ensure-resolved (schema) (:documentation "Ensure a schema is resolved"))
(defmethod schema-ensure-resolved (schema) schema)
(defmethod schema-ensure-resolved ((schema ref-schema))
  (schema-ensure-resolved (schema-ref-resolve schema)))

;; Infers the type of the OAS schema represented by the hash table TABLE.
;; Returns a symbol representing the inferred type.
(defun schema-ht-infer-type (table)
  (labels ((has (key) (gethash key table))
           (has= (key expected) (string= expected (has key)))
           (type= (expected) (has= "type" expected)))
    (cond ((has "$ref") :ref)
          ((has "allOf") :all-of)
          ((has "oneOf") :one-of)
          ((has "anyOf") :any-of)
          ((type= "array") :array)
          ((type= "object") :object)
          ((type= "number") :number)
          ((type= "integer") :integer)
          ((type= "string") :string)
          ((type= "boolean") :boolean)
          (t :object))))

;; Factory for ARRAY-SCHEMA objects.
;;
;; TABLE should be an OAS hash table / dictionary.
(defun schema-array-from-ht (table)
  (let* ((items (gethash "items" table))
         (nullable (gethash "nullable" table))
         (instance (make-instance 'array-schema :items (schema-from-ht items))))
    (setf (schema-nullable instance) nullable)
    instance))

;; Factory for OBJECT-SCHEMA objects.
;;
;; TABLE should be an OAS hash table / dictionary.
(defun schema-object-from-ht (table)
  (let ((properties (gethash "properties" table))
        (required (let ((names (gethash "required" table)))
                    (when names
                      (let ((ht (make-hash-table :test 'equal)))
                        (loop for name across names
                              do (setf (gethash name ht) t))
                        ht))))
        (nullable (gethash "nullable" table))
        (instance (make-instance 'object-schema)))
    (setf (schema-nullable instance) nullable)
    (when properties
      (maphash (lambda (name value) (object-schema-add-property
                                     instance
                                     name
                                     (schema-from-ht value)
                                     (and required (gethash name required))))
               properties))
    instance))

;; Factory for SIMPLE-SCHEMA objects.
;;
;; TABLE should be an OAS hash table / dictionary.
(defun schema-simple-from-ht (table type)
  (let* ((format (gethash "format" table))
         (enum (gethash "enum" table))
         (nullable (gethash "nullable" table))
         (instance (make-instance 'simple-schema
                                  :type type :format format :enum enum)))
    (setf (schema-nullable instance) nullable)
    instance))

(defun discriminator-from-ht (table)
  (let ((discriminator (gethash "discriminator" table)))
    (when discriminator
      (let ((name (gethash "propertyName" discriminator))
            (mapping (gethash "mapping" discriminator)))
        (when name
             (make-instance 'discriminated-property :name name :mapping mapping))))))

(defun comb-type-key (type)
  (ecase type
    (:all-of "allOf")
    (:any-of "anyOf")
    (:one-of "oneOf")))

;; Factory for COMB-SCHEMA objects.
;;
;; TABLE should be an OAS hash table / dictionary.
(defun schema-comb-from-ht (table type)
  (let ((schemas (gethash (comb-type-key type) table)))
    (if schemas
        (make-instance 'comb-schema
                       :op type
                       :operands (map 'list #'schema-from-ht schemas)
                       :discriminator (discriminator-from-ht table))
        (error "Empty ~A schema" type))))

;; Abstract factory for schema objects.
;;
;; TABLE should be an OAS hash table / dictionary.
(defun schema-from-ht (table)
  (let ((inferred-type (schema-ht-infer-type table)))
    (ecase inferred-type
      (:ref (schema-ref-from-ht table))
      (:array (schema-array-from-ht table))
      (:object (schema-object-from-ht table))
      ((:number :integer :string :boolean) (schema-simple-from-ht table inferred-type))
      ((:all-of :any-of :one-of) (schema-comb-from-ht table inferred-type)))))
