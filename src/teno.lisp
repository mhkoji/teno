(defpackage :teno
  (:use :cl)
  (:export :memo
           :memo-text
           :service
           :service-list-memos
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

;;;

(defclass gui () ())

(defgeneric gui-draw-memos (gui memos))

;;;

(defstruct service
  (store (make-instance 'store))
  gui)

(defun service-list-memos (service)
  (let ((memos (store-list-memos (service-store service))))
    (gui-draw-memos (service-gui service) memos)))

;;;

(defmethod store-list-memos ((store store))
  (list (make-memo :text "aaa")
        (make-memo :text "bbb")
        (make-memo :text "ccc")))
