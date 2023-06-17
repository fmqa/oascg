(defpackage #:oascg-typescript-tests
  (:use :cl :fiveam)
  (:export #:run! #:oascg-typescript))

(in-package #:oascg-typescript-tests)

(def-suite oascg-typescript)

(in-suite oascg-typescript)

(test test-basic-types-any
  (let* ((json "{}")
         (ht (com.inuoe.jzon:parse json))
         (schema (oascg-core:schema-from-ht ht)))
    (is (string= "unknown" (oascg-typescript:typescript schema)))))

(test test-basic-types-number
  (let* ((json "{\"type\": \"number\"}")
         (ht (com.inuoe.jzon:parse json))
         (schema (oascg-core:schema-from-ht ht)))
    (is (string= "number" (oascg-typescript:typescript schema)))))

(test test-basic-types-number-nullable
  (let* ((json "{\"type\": \"number\", \"nullable\": true}")
         (ht (com.inuoe.jzon:parse json))
         (schema (oascg-core:schema-from-ht ht)))
    (is (string= "number | null" (oascg-typescript:typescript schema)))))

(test test-basic-types-string
  (let* ((json "{\"type\": \"string\"}")
         (ht (com.inuoe.jzon:parse json))
         (schema (oascg-core:schema-from-ht ht)))
    (is (string= "string" (oascg-typescript:typescript schema)))))

(test test-string-enum
  (let* ((json "{\"type\": \"string\", \"enum\":[\"a\",\"b\"]}")
         (ht (com.inuoe.jzon:parse json))
         (schema (oascg-core:schema-from-ht ht)))
    (is (string= "\"a\"|\"b\"" (oascg-typescript:typescript schema)))))

(test test-array
  (let* ((json "{\"type\": \"array\", \"items\":{\"type\": \"number\"}}")
         (ht (com.inuoe.jzon:parse json))
         (schema (oascg-core:schema-from-ht ht)))
    (is (string= "Array<number>" (oascg-typescript:typescript schema)))))

(test test-object
  (let* ((json "{\"type\": \"object\", \"properties\":{\"a\": {\"type\": \"number\"}}}")
         (ht (com.inuoe.jzon:parse json))
         (schema (oascg-core:schema-from-ht ht)))
    (is (string= "{a?: number;}" (oascg-typescript:typescript schema)))))

(test test-object-required
  (let* ((json "{\"type\": \"object\", \"required\": [\"a\"], \"properties\":{\"a\": {\"type\": \"number\"}}}")
         (ht (com.inuoe.jzon:parse json))
         (schema (oascg-core:schema-from-ht ht)))
    (is (string= "{a: number;}" (oascg-typescript:typescript schema)))))

(test test-comb-allof-objects
  (let* ((json-a "{\"type\": \"object\", \"required\": [\"a\"], \"properties\":{\"a\": {\"type\": \"number\"}}}")
         (json-b "{\"type\": \"object\", \"required\": [\"b\"], \"properties\":{\"b\": {\"type\": \"string\"}}}")
         (json (format nil "{\"allOf\": [~A, ~A]}" json-a json-b))
         (ht (com.inuoe.jzon:parse json))
         (schema (oascg-core:schema-from-ht ht)))
    (is (string= "{a: number;} & {b: string;}" (oascg-typescript:typescript schema)))))

(test test-comb-anyof-objects
  (let* ((json-a "{\"type\": \"object\", \"required\": [\"a\"], \"properties\":{\"a\": {\"type\": \"number\"}}}")
         (json-b "{\"type\": \"object\", \"required\": [\"b\"], \"properties\":{\"b\": {\"type\": \"string\"}}}")
         (json (format nil "{\"anyOf\": [~A, ~A]}" json-a json-b))
         (ht (com.inuoe.jzon:parse json))
         (schema (oascg-core:schema-from-ht ht)))
    (is (string= "{a: number;} | {b: string;}" (oascg-typescript:typescript schema)))))

(test test-comb-oneof-objects
  (let* ((json-a "{\"type\": \"object\", \"required\": [\"a\"], \"properties\":{\"a\": {\"type\": \"number\"}}}")
         (json-b "{\"type\": \"object\", \"required\": [\"b\"], \"properties\":{\"b\": {\"type\": \"string\"}}}")
         (json (format nil "{\"oneOf\": [~A, ~A]}" json-a json-b))
         (ht (com.inuoe.jzon:parse json))
         (schema (oascg-core:schema-from-ht ht)))
    (is (string= "{a: number;} | {b: string;}" (oascg-typescript:typescript schema)))))

(test test-comb-allof-objects-nested
  (let* ((json-a "{\"type\": \"object\", \"required\": [\"a\"], \"properties\":{\"a\": {\"type\": \"number\"}}}")
         (json-b "{\"type\": \"object\", \"required\": [\"b\"], \"properties\":{\"b\": {\"type\": \"string\"}}}")
         (json-c "{\"type\": \"object\", \"properties\": {\"xyz\": {\"type\": \"string\"}}}")
         (one-of-a (format nil "{\"oneOf\": [~A, ~A]}" json-a json-b))
         (one-of-b (format nil "{\"oneOf\": [~A, ~A]}" json-a json-b))
         (all-of (format nil "{\"allOf\": [~A, ~A, ~A]}" one-of-a one-of-b json-c))
         (ht (com.inuoe.jzon:parse all-of))
         (schema (oascg-core:schema-from-ht ht)))
    (is (string= "({a: number;} | {b: string;}) & ({a: number;} | {b: string;}) & {xyz?: string;}"
                 (oascg-typescript:typescript schema)))))

(test test-object-export-one
  (let* ((json "{\"type\": \"object\", \"properties\":{\"a\": {\"type\": \"number\"}}}")
         (components (format nil "{\"first\": ~A}" json))
         (ht (com.inuoe.jzon:parse components)))
    (is (equal (list "export interface first {a?: number;}")
               (oascg-typescript:export-typescript-components ht)))))

(test test-object-export-two
  (let* ((one "{\"type\": \"object\", \"properties\":{\"a\": {\"type\": \"number\"}}}")
         (two "{\"type\": \"string\", \"enum\":[\"a\",\"b\"]}")
         (components (format nil "{\"first\": ~A, \"second\": ~A}" one two))
         (ht (com.inuoe.jzon:parse components)))
    (is (equal (list "export interface first {a?: number;}"
                     "export type second = \"a\"|\"b\"")
               (oascg-typescript:export-typescript-components ht)))))

(test test-component-ref
  (let* ((one "{\"type\": \"object\", \"properties\":{\"a\": {\"type\": \"number\"}}}")
         (two "{\"type\": \"array\", \"items\":{\"$ref\": \"#/components/schemas/first\"}}")
         (components (format nil "{\"first\": ~A, \"second\": ~A}" one two))
         (ht (com.inuoe.jzon:parse components)))
    (is (equal (list "export interface first {a?: number;}"
                     "export type second = Array<first>")
               (oascg-typescript:export-typescript-components ht)))))

(test test-component-ref-nested
  (let* ((one "{\"type\": \"object\", \"required\":[\"b\"], \"properties\":{\"a\": {\"type\": \"number\"}, \"b\": {\"$ref\": \"#/components/schemas/third\"}}}")
         (two "{\"type\": \"array\", \"items\":{\"$ref\": \"#/components/schemas/first\"}}")
         (three "{\"type\": \"integer\"}")
         (components (format nil "{\"first\": ~A, \"second\": ~A, \"third\": ~A}" one two three))
         (ht (com.inuoe.jzon:parse components)))
    (is (equal (list "export interface first {a?: number;b: third;}"
                     "export type second = Array<first>"
                     "export type third = number")
               (oascg-typescript:export-typescript-components ht)))))
