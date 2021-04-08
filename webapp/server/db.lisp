(defpackage :teno.db
  (:use :cl)
  (:export :call-with-connection
           :with-connection
           :connection
           :db

           :note-select-by-id
           :note-select
           :note-insert
           :note-delete
           :note-text-select
           :note-text-insert
           :note-text-delete
           :note-text-update))
(in-package :teno.db)

(defclass db () ())

(defclass connection () ())

(defgeneric call-with-connection (db callback))

(defmacro with-connection ((conn db) &body body)
  `(call-with-connection ,db (lambda (,conn) ,@body)))

;;;

(defgeneric note-select-by-id (conn node-id))

(defgeneric note-select (conn))

(defgeneric note-insert (conn note))

(defgeneric note-delete (conn note-id-list))


(defgeneric note-text-select (conn note-id))

(defgeneric note-text-insert (conn node-id string))

(defgeneric note-text-delete (conn note-id-list))

(defgeneric note-text-update (conn note-id string))
