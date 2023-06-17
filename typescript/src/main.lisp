;; OAS CODE GENERATOR - TYPESCRIPT SUPPORT
;;
;; Generates TypeScript type declarations from OAS Schemas.
;;

(defpackage oascg-typescript
  (:use :cl)
  (:export #:*export*
           #:typescript #:export-typescript-components))

(in-package :oascg-typescript)

(defparameter *export* t "Exports declarations if set to t")

(defgeneric typescript (schema &optional name required)
  (:documentation "Computes the TypeScript type corresponding to the given schema."))

(defgeneric export-typescript-schema (schema name)
  (:documentation "Computes a TypeScript export clause for the given schema."))

(defun simple-type-to-typescript (type)
  (ecase type
    ((:integer :number) "number")
    (:string "string")
    (:boolean "boolean")))

(defun simple-enum-to-typescript (type enum)
  (format nil "~{~A~^|~}"
          (ecase type
            ((:integer :number) (coerce enum 'list))
            (:string (map 'list #'com.inuoe.jzon:stringify enum)))))

;; Determines whether a schema should be parenthesized, given op is one
;; of (:all-of :any-of :one-of).
;;
;; This is used when translating nested expressions similar to
;;   (allOf (anyOf ...) (anyOf (oneOf ...) ...) ...)
(defun should-parenthesize-p (op schema)
  (and (typep schema 'oascg-core:comb-schema)
       (not (eq op (oascg-core:comb-schema-op schema)))))

(defun parenthesized-typescript (schema)
  (format nil "(~A)" (typescript schema)))

;; Translate (op ...), (op ...), (op ...) schemas where
;; op a member of (:all-of :any-of :one-of)
(defun comb-op-to-typescript (op operands fstr)
  (format nil fstr
          (map 'list
               (lambda (schema) (if (should-parenthesize-p op schema)
                               (parenthesized-typescript schema)
                               (typescript schema)))
               operands)))

;; Translate (allOf ...)
(defun all-of-to-typescript (op operands)
  (comb-op-to-typescript op operands "~{~A~^ & ~}"))

;; Translate (anyOf ...) and (oneOf ...)
(defun any-one-of-to-typescript (op operands)
  (comb-op-to-typescript op operands "~{~A~^ | ~}"))

(defun comb-to-typescript (op operands &optional discriminator)
  (declare (ignore discriminator))
  (ecase op
    (:all-of (all-of-to-typescript op operands))
    ((:any-of :one-of) (any-one-of-to-typescript op operands))))

;; Translate an object schema member canonically, taking into account
;; its required/nullable flags.
(defun typescript-member-norm (right name required &optional nullable)
  (when nullable
    (setf right (format nil "~A | null" right)))
  (if name
      (format nil (if required "~A: ~A" "~A?: ~A") name right)
      right))

(defmethod typescript ((schema oascg-core:simple-schema) &optional name required)
  (with-accessors ((type oascg-core:simple-schema-type)
                   (enum oascg-core:simple-schema-enum)
                   (nullable oascg-core:schema-nullable))
      schema
    (let ((right (if enum
                     (simple-enum-to-typescript type enum)
                     (simple-type-to-typescript type))))
      (typescript-member-norm right name required nullable))))

(defmethod typescript ((schema oascg-core:array-schema) &optional name required)
  (let* ((tparam (typescript (oascg-core:array-schema-items schema)))
         (nullable (oascg-core:schema-nullable schema))
         (right (format nil "Array<~A>" tparam)))
    (typescript-member-norm right name required nullable)))

(defmethod typescript ((schema oascg-core:object-schema) &optional name required)
  (if (oascg-core:schema-unboundedp schema)
      ;; Translate unbounded/any schemas to 'unknown' type
      (typescript-member-norm "unknown" name required
                              (oascg-core:schema-nullable schema))
      ;; Translate other schemas
      (let ((members nil))
        (funcall (oascg-core:schema-iterator schema)
                 (lambda (&rest args) (push (apply #'typescript args) members)))
        (let ((nullable (oascg-core:schema-nullable schema))
              (right (format nil "{~{~A;~}}" (nreverse members))))
          (typescript-member-norm right name required nullable)))))

(defmethod typescript ((schema oascg-core:comb-schema) &optional name required)
  (with-accessors ((op oascg-core:comb-schema-op)
                   (operands oascg-core:comb-schema-operands)
                   (discriminator oascg-core:comb-schema-discriminator))
      schema
    (let ((right (comb-to-typescript op operands discriminator)))
      (typescript-member-norm right name required))))

(defmethod typescript ((comp oascg-core:component) &optional name required)
  (typescript-member-norm (oascg-core:component-name comp) name required))

(defmethod typescript ((schema oascg-core:ref-schema) &optional name required)
  (typescript-member-norm (typescript (oascg-core:schema-ref-resolve schema))
                          name required))

(defun export-typescript-alias (name schema)
  (format nil "type ~A = ~A" name (typescript schema)))

(defmethod export-typescript-schema ((schema oascg-core:object-schema) name)
  (format nil "interface ~A ~A" name (typescript schema)))

(defmethod export-typescript-schema ((schema oascg-core:array-schema) name)
  (export-typescript-alias name schema))

(defmethod export-typescript-schema ((schema oascg-core:simple-schema) name)
  (export-typescript-alias name schema))

(defmethod export-typescript-schema ((schema oascg-core:comb-schema) name)
  (export-typescript-alias name schema))

(defmethod export-typescript-schema ((schema oascg-core:ref-schema) name)
  (export-typescript-schema (oascg-core:schema-ref-resolve schema) name))

(defmethod export-typescript-schema ((schema oascg-core:component) name)
  (format nil "type ~A = ~A" name (oascg-core:component-name schema)))

(defun export-if (s)
  (if *export* (format nil "export ~A" s) s))

(defun export-typescript-component (comp)
  (export-if (export-typescript-schema (oascg-core:component-ensure-schema comp)
                                       (oascg-core:component-name comp))))

(defun do-export-typescript-components ()
  (let ((exports nil))
    (handler-bind ((oascg-core:unresolved-schema-error (oascg-core:resolve-local-component-handler)))
      (oascg-core:map-components
       (lambda (comp) (push (export-typescript-component comp) exports))))
    (nreverse exports)))

;; Returns a list of typescript type declarations corresponding to the OAS
;; component mapping given in TABLE.
;;
;; If *EXPORT* is set, all declarations will be prefixed with the "export"
;; keyword.
(defun export-typescript-components (table)
  (oascg-core:with-components-from-ht table #'do-export-typescript-components))
