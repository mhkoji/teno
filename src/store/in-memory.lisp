(defpackage :teno.store.in-memory
  (:use :cl)
  (:export :store))
(in-package :teno.store.in-memory)

(defclass store (teno:store)
  ((memos :initform nil)))

(defmethod teno:store-list-memos ((store store))
  (slot-value store 'memos))

(defmethod teno:store-save-memo ((store store) (memo teno:memo))
  (push memo (slot-value store 'memos)))
