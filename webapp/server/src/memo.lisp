(defpackage :teno.memo
  (:use :cl)
  (:export :list-text-types
           :text-string
           :text-type
           :text
           :make-text
           :parse-text-type
           :construct-text
           
           :memo
           :memo-id
           :memo-created-on
           :memo-text
           :memos-text-head-strings
           :create-memo
           :add-memo
           :construct-memo
           :save-memo
           :delete-memo
           :load-memo-by-id
           :load-memos
           :update-memo-text

           :load-head-text-memos
           :head-text-memo
           :head-text-memo-string))
(in-package :teno.memo)

(defvar +plain+ :plain)
(defvar +markdown+ :markdown)

(defstruct text string type)

(defun list-text-types ()
  (list +plain+
        +markdown+))

(defun assert-valid-text-type (type)
  (assert (member type (list-text-types)))
  type)

(defun parse-text-type (s)
  (assert-valid-text-type (alexandria:make-keyword s)))
    
(defun construct-text (&key type string)
  (make-text :type (assert-valid-text-type type)
             :string string))


(defclass memo ()
  ((id
    :initarg :id
    :reader memo-id)
   (created-on
    :initarg :created-on
    :reader memo-created-on)))

(defun construct-memo (&key id created-on)
  (make-instance 'memo
                 :id id
                 :created-on created-on))

(defgeneric load-memo-by-id (conn memo-id))

(defgeneric load-memos (conn))

(defgeneric save-memo (conn memo))

(defgeneric delete-memo (conn memo-id))


(defgeneric memo-text (conn memo))

(defgeneric update-memo-text (conn memo text))

(defun create-memo (conn)
  (let ((memo-id (teno.id:gen)))
    (save-memo conn (construct-memo
                     :id memo-id
                     :created-on (local-time:now)))
    (load-memo-by-id conn memo-id)))

(defun add-memo (conn string type)
  (let ((memo (create-memo conn))
        (text (construct-text :type type :string string)))
    (update-memo-text conn memo text)
    memo))

;;;

(defclass head-text-memo (memo)
  ((string
    :initarg :string
    :reader head-text-memo-string)))

(defgeneric load-head-text-memos (conn))
