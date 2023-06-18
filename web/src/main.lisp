;; OAS CODE GENERATOR - WEB SERVICE
;;
;; This module provides a code generator HTTP server.

(defpackage oascg-web
  (:use :cl)
  (:export #:start-server #:start-server-and-wait
           #:stop-server))

(in-package :oascg-web)

(defparameter *current-server* nil)

(defmacro define-easy-method-handler (name uri &body body)
  `(hunchentoot:define-easy-handler (,name :uri ,uri) ()
     (cond
       ,@(mapcar (lambda (clause)
                   (destructuring-bind (method func) clause
                     `((eq ,method (hunchentoot:request-method*)) (funcall ,func))))
                 body)
       (t (setf (hunchentoot:return-code*) hunchentoot:+http-method-not-allowed+)
          (setf (hunchentoot:content-type*) "text/plain")
          "Method Not Allowed"))))

(defmacro media-type-case (&body body)
  `(cond
     ,@(mapcar (lambda (clause)
                 (destructuring-bind (media-type func) clause
                   `((equal ,media-type (hunchentoot:header-in* :accept)) (funcall ,func))))
               body)
     (t (setf (hunchentoot:return-code*) hunchentoot:+http-not-acceptable+)
        (setf (hunchentoot:content-type*) "text/plain")
        "Not Acceptable")))

(defun oas-request-body ()
  (let ((post-data (hunchentoot:raw-post-data)))
    (when post-data
      (com.inuoe.jzon:parse post-data))))

(defun oas-components (table)
  (let ((components (gethash "components" table)))
    (and components (gethash "schemas" components))))

(defun bad-request-response ()
  (setf (hunchentoot:return-code*) hunchentoot:+http-bad-request+)
  (setf (hunchentoot:content-type*) "text/plain")
  "Bad Request")

(defun oas-to-typescript ()
  (let* ((body (handler-case (oas-request-body)
                 (com.inuoe.jzon:json-parse-error ()
                   (return-from oas-to-typescript (bad-request-response)))))
         (schemas (and body (oas-components body))))
    (if schemas
        (progn (setf (hunchentoot:content-type*) "text/typescript")
               (format nil "~{~A~%~}" (oascg-typescript:export-typescript-components schemas)))
        (bad-request-response))))

(defun handle-oas-post ()
  (media-type-case ("text/typescript" #'oas-to-typescript)))

(define-easy-method-handler oas-code-generator "/"
  (:POST #'handle-oas-post))

(defun start-server (&optional (port 8080))
  (when *current-server*
    (hunchentoot:stop *current-server*)
    (setf *current-server* nil))
  (hunchentoot:start (setf *current-server*
                           (make-instance 'hunchentoot:easy-acceptor :port port))))

(defun start-server-and-wait (&rest args)
  (apply #'start-server args)
  (sleep most-positive-fixnum))

(defun stop-server ()
  (hunchentoot:stop *current-server*)
  (setf *current-server* nil))
