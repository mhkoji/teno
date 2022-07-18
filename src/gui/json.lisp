(defpackage :teno.gui.json
  (:use :cl)
  (:export :gui
           :parse
           :op
           :arg))
(in-package :teno.gui.json)

(defclass gui (teno:gui)
  ((send-fn :initarg :send-fn
            :reader send-fn)))

;;;

(defun send-msg (gui msg)
  (funcall (send-fn gui) msg))

(defgeneric to-jsown (x))

(defmethod to-jsown ((xs list))
  (mapcar #'to-jsown xs))

(defmethod to-jsown ((memo teno:memo))
  (jsown:new-js
    ("text" (teno:memo-text memo))))

(defun msg-draw-memos (memos)
  (jsown:to-json
   (jsown:new-js
     ("op" "draw_memos")
     ("memos" (to-jsown memos)))))

(defun msg-clear-input ()
  (jsown:to-json
   (jsown:new-js
     ("op" "clear_input"))))

(defmethod teno:gui-draw-memos ((gui gui) (memos list))
  (send-msg gui (msg-draw-memos memos)))

(defmethod teno:gui-clear-input ((gui gui))
  (send-msg gui (msg-clear-input)))

;;;

(defun parse (string)
  (jsown:parse string))

(defun op (json)
  (let ((op-string (jsown:val-safe json "op")))
    (when op-string
      (alexandria:make-keyword
       (string-upcase op-string)))))

(defun arg (json key)
  (let ((args-json (jsown:val-safe json "args")))
    (when args-json
      (jsown:val-safe args-json key))))
