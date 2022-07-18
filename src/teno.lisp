(defpackage :teno
  (:use :cl)
  (:export :memo
           :memo-text
           :store
           :store-list-memos
           :store-save-memos
           :service
           :service-list-memos
           :service-add-memo
           :make-service
           :gui
           :gui-draw-memos))
(in-package :teno)

(defstruct memo
  created-at
  text)

;;;

(defclass store () ())

(defgeneric store-list-memos (store))

(defgeneric store-save-memo (store memo))

;;;

(defclass gui () ())

(defgeneric gui-draw-memos (gui memos))

;;;

(defstruct service
  store
  gui)

(defun service-list-memos (service)
  (let ((memos (store-list-memos (service-store service))))
    (gui-draw-memos (service-gui service) memos)))

(defun service-add-memo (service text)
  (let ((memo (make-memo :created-at "TODO" :text text)))
    (store-save-memo (service-store service) memo))
  (service-list-memos service))

;;;

(defclass easy-store (store)
  ((memos :initform nil)))

(defmethod store-list-memos ((store easy-store))
  (slot-value store 'memos))

(defmethod store-save-memo ((store easy-store) (memo memo))
  (push memo (slot-value store 'memos)))
