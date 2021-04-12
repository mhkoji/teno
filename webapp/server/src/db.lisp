(defpackage :teno.db
  (:use :cl)
  (:export :call-with-connection
           :with-connection
           :connection
           :db))
(in-package :teno.db)

(defclass db () ())

(defclass connection () ())

(defgeneric call-with-connection (db callback))

(defmacro with-connection ((conn db) &body body)
  `(call-with-connection ,db (lambda (,conn) ,@body)))
