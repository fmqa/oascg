(defpackage #:oascg-core-tests
  (:use :cl :fiveam)
  (:export #:run! #:oascg-core))

(in-package #:oascg-core-tests)

(def-suite oascg-core)

(in-suite oascg-core)

(test parse-unbounded-schema
  (let* ((example-json "{}")
         (oas-schema (com.inuoe.jzon:parse example-json))
         (schema (oascg-core:schema-from-ht oas-schema)))
    (is (identity (oascg-core:schema-unboundedp schema)))
    (is (not (oascg-core:schema-nullable schema)))))

(test parse-unbounded-schema-nullable
  (let* ((example-json "{\"nullable\": true}")
         (oas-schema (com.inuoe.jzon:parse example-json))
         (schema (oascg-core:schema-from-ht oas-schema)))
    (is (identity (oascg-core:schema-unboundedp schema)))
    (is (oascg-core:schema-nullable schema))))

(test parse-string-schema
  (let* ((example-json "{\"type\": \"string\"}")
         (oas-schema (com.inuoe.jzon:parse example-json))
         (schema (oascg-core:schema-from-ht oas-schema)))
    (is (typep schema 'oascg-core:simple-schema))
    (is (eq :string (oascg-core:simple-schema-type schema)))
    (is (not (oascg-core:simple-schema-format schema)))
    (is (not (oascg-core:simple-schema-enum schema)))
    (is (not (oascg-core:schema-nullable schema)))))

(test parse-string-schema-enum
  (let* ((example-json "{\"type\": \"string\", \"enum\":[\"a\",\"b\"]}")
         (oas-schema (com.inuoe.jzon:parse example-json))
         (schema (oascg-core:schema-from-ht oas-schema)))
    (is (typep schema 'oascg-core:simple-schema))
    (is (eq :string (oascg-core:simple-schema-type schema)))
    (is (not (oascg-core:simple-schema-format schema)))
    (is (equalp #("a" "b") (oascg-core:simple-schema-enum schema)))
    (is (not (oascg-core:schema-nullable schema)))))

(test parse-number-schema
  (let* ((example-json "{\"type\": \"number\"}")
         (oas-schema (com.inuoe.jzon:parse example-json))
         (schema (oascg-core:schema-from-ht oas-schema)))
    (is (typep schema 'oascg-core:simple-schema))
    (is (eq :number (oascg-core:simple-schema-type schema)))
    (is (not (oascg-core:simple-schema-format schema)))
    (is (not (oascg-core:simple-schema-enum schema)))
    (is (not (oascg-core:schema-nullable schema)))))

(test parse-integer-schema
  (let* ((example-json "{\"type\": \"integer\"}")
         (oas-schema (com.inuoe.jzon:parse example-json))
         (schema (oascg-core:schema-from-ht oas-schema)))
    (is (typep schema 'oascg-core:simple-schema))
    (is (eq :integer (oascg-core:simple-schema-type schema)))
    (is (not (oascg-core:simple-schema-format schema)))
    (is (not (oascg-core:simple-schema-enum schema)))
    (is (not (oascg-core:schema-nullable schema)))))

(test parse-integer-schema-format-int32
  (let* ((example-json "{\"type\": \"integer\", \"format\": \"int32\"}")
         (oas-schema (com.inuoe.jzon:parse example-json))
         (schema (oascg-core:schema-from-ht oas-schema)))
    (is (typep schema 'oascg-core:simple-schema))
    (is (eq :integer (oascg-core:simple-schema-type schema)))
    (is (string= "int32" (oascg-core:simple-schema-format schema)))
    (is (not (oascg-core:simple-schema-enum schema)))
    (is (not (oascg-core:schema-nullable schema)))))

(test parse-boolean-schema
  (let* ((example-json "{\"type\": \"boolean\"}")
         (oas-schema (com.inuoe.jzon:parse example-json))
         (schema (oascg-core:schema-from-ht oas-schema)))
    (is (typep schema 'oascg-core:simple-schema))
    (is (eq :boolean (oascg-core:simple-schema-type schema)))
    (is (not (oascg-core:simple-schema-format schema)))
    (is (not (oascg-core:simple-schema-enum schema)))
    (is (not (oascg-core:schema-nullable schema)))))

(test parse-array-schema
  (let* ((example-json "{\"type\": \"array\", \"items\": {\"type\": \"number\"}}")
         (oas-schema (com.inuoe.jzon:parse example-json))
         (schema (oascg-core:schema-from-ht oas-schema)))
    (is (typep schema 'oascg-core:array-schema))
    (is (not (oascg-core:schema-nullable schema)))
    (funcall (oascg-core:schema-iterator schema)
             (lambda (schema)
               (is (typep schema 'oascg-core:simple-schema))
               (is (eq :number (oascg-core:simple-schema-type schema)))
               (is (not (oascg-core:simple-schema-format schema)))
               (is (not (oascg-core:simple-schema-enum schema)))
               (is (not (oascg-core:schema-nullable schema)))))))

(test parse-object-schema
  (let* ((example-json "{\"type\": \"object\", \"required\":[\"a\"], \"properties\": {\"a\": {\"type\": \"string\"}, \"b\": {\"type\": \"number\"}}}")
         (oas-schema (com.inuoe.jzon:parse example-json))
         (schema (oascg-core:schema-from-ht oas-schema))
         (names nil))
    (is (typep schema 'oascg-core:object-schema))
    (is (not (oascg-core:schema-nullable schema)))
    (funcall (oascg-core:schema-iterator schema)
             (lambda (schema name required)
               (push name names)
               (cond
                 ((string= "a" name)
                  (is (identity required))
                  (is (typep schema 'oascg-core:simple-schema))
                  (is (eq :string (oascg-core:simple-schema-type schema)))
                  (is (not (oascg-core:simple-schema-format schema)))
                  (is (not (oascg-core:simple-schema-enum schema)))
                  (is (not (oascg-core:schema-nullable schema))))
                 ((string= "b" name)
                  (is (typep schema 'oascg-core:simple-schema))
                  (is (eq :number (oascg-core:simple-schema-type schema)))
                  (is (not (oascg-core:simple-schema-format schema)))
                  (is (not (oascg-core:simple-schema-enum schema)))
                  (is (not (oascg-core:schema-nullable schema)))))))
    (is (equal '("b" "a") names))))

(test test-ref
  (let* ((json "{\"$ref\": \"#/components/schemas/a\"}")
         (ht (com.inuoe.jzon:parse json))
         (schema (oascg-core:schema-from-ht ht))
         (schema-uri nil)
         (handle-unresolved (lambda (err)
                              (setf schema-uri (oascg-core:unresolved-schema-uri err))
                              (invoke-restart 'use-value (make-instance 'oascg-core:object-schema)))))
    (handler-bind ((oascg-core:unresolved-schema-error handle-unresolved))
      (oascg-core:schema-ref-resolve schema))
    (is (string= "#/components/schemas/a" schema-uri))))
