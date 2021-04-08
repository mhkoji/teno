(defpackage :teno
  (:use :cl)
  (:export :note
           :note-id
           :note-created-on
           :note-text
           :create-note
           :construct-note
           :save-note
           :delete-note
           :load-note-by-id
           :load-notes
           :update-note-text
           :render-note-text))
(in-package :teno)

(defstruct note id created-on)

(defun construct-note (id created-on)
  (make-note :id id :created-on created-on))


(defgeneric load-note-by-id (db note-id))

(defgeneric load-notes (db))

(defgeneric save-note (db note))

(defgeneric delete-note (db note-id))

(defgeneric note-text (db note))

(defgeneric update-note-text (db note string))


(defun create-note (db)
  (let ((note-id (teno.id:gen)))
    (save-note db (make-note
                   :id note-id
                   :created-on (local-time:now)))
    (load-note-by-id db note-id)))

(defun render-note-text (db note)
  (teno.markdown:to-html (note-text db note)))
