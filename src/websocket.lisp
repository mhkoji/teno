(defpackage :teno.websocket
  (:use :cl)
  (:export :main))
(in-package :teno.websocket)

(defvar *handler* nil)

(defgeneric to-jsown (x))

(defmethod to-jsown ((xs list))
  (mapcar #'to-jsown xs))

(defmethod to-jsown ((memo teno:memo))
  (jsown:new-js
    ("text" (teno:memo-text memo))))

;;;

(defclass websocket-gui (teno:gui)
  ((ws :initarg :ws
       :reader ws)))

(defun msg-draw-memos (memos)
  (jsown:to-json
   (jsown:new-js
     ("op" "draw_memos")
     ("memos" (to-jsown memos)))))

(defun gui-send-msg (gui msg)
  (websocket-driver:send (ws gui) msg))

(defmethod teno:gui-draw-memos ((gui websocket-gui) (memos list))
  (gui-send-msg gui (msg-draw-memos memos)))

(defun make-service (ws)
  (let ((gui (make-instance 'websocket-gui :ws ws)))
    (teno:make-service :gui gui)))


(defun json-op (json)
  (let ((op-string (jsown:val-safe json "op")))
    (when op-string
      (alexandria:make-keyword
       (string-upcase op-string)))))

(defun handle-message (service msg)
  (let ((json (jsown:parse msg)))
    (case (json-op json)
      (:list_memos
       (teno:service-list-memos service))
      (t
       (format t "Unknown msg: ~A" msg)))))

(defun ws-server (env)
  (let* ((ws (websocket-driver:make-server env))
         (service (make-service ws)))
    (websocket-driver:on :message ws
                         (lambda (msg)
                           (handle-message service msg)))
    (lambda (responder)
      (declare (ignore responder))
      (websocket-driver:start-connection ws))))

(defun main ()
  (when *handler*
    (clack:stop *handler*))
  (setq *handler*
        (clack:clackup
         (lambda (env)
           (ws-server env)))))
