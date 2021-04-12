(defpackage :teno
  (:use :cl)
  (:export :memo
           :memo-id
           :memo-created-on
           :memo-text
           :create-memo
           :construct-memo
           :save-memo
           :delete-memo
           :load-memo-by-id
           :load-memos
           :render-memo-text))
(in-package :teno)

(defstruct memo id created-on)

(defun construct-memo (id created-on)
  (make-memo :id id :created-on created-on))


(defgeneric load-memo-by-id (conn memo-id))

(defgeneric load-memos (conn))

(defgeneric save-memo (conn memo))

(defgeneric delete-memo (conn memo-id))

(defgeneric memo-text (conn memo))

(defun create-memo (conn)
  (let ((memo-id (teno.id:gen)))
    (save-memo conn (make-memo
                     :id memo-id
                     :created-on (local-time:now)))
    (load-memo-by-id conn memo-id)))

(defun render-memo-text (conn memo)
  (teno.markdown:to-html (memo-text conn memo)))
