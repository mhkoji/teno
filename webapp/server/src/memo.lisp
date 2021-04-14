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

           :detail
           :memo-detail
           :detail-text
           :detail-created-on
           
           :load-head-text-memos
           :head-text-memo
           :head-text-memo-string

           :group-by-created-on))
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

(defstruct detail text created-on)

(defun memo-detail (conn memo-id)
  (let ((memo (load-memo-by-id conn memo-id)))
    (when memo
      (make-detail
       :text (memo-text conn memo)
       :created-on (memo-created-on memo)))))
  

;;;

(defclass head-text-memo (memo)
  ((string
    :initarg :string
    :reader head-text-memo-string)))

(defgeneric load-head-text-memos (conn))


(defun group-by-created-on (memos)
  (labels ((universal-of-clock-today (ts)
             (local-time:timestamp-to-universal
              (local-time:clock-today ts))))
    (let ((universal-list nil)
          (universal->memos (make-hash-table :test #'equal)))
      (dolist (memo memos)
        (let ((u (universal-of-clock-today (memo-created-on memo))))
          (push memo (gethash u universal->memos))
          (pushnew u universal-list :test #'=)))
      (mapcar (lambda (u)
                (cons (local-time:universal-to-timestamp u)
                      (nreverse (gethash u universal->memos))))
              universal-list))))
