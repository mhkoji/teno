(defpackage :teno.websocket
  (:use :cl)
  (:export :main))
(in-package :teno.websocket)

(defvar *handler* nil)

(defvar *service* nil)

;;;

(defun make-ws-gui (ws)
  (make-instance 'teno.gui.json:gui
   :send-fn (lambda (msg) (websocket-driver:send ws msg))))

(defun make-service (ws)
  (teno:make-service
   :gui (make-ws-gui ws)
   :store (make-instance 'teno.store.in-memory:store)))

(defun handle-message (service msg)
  (let ((json (teno.gui.json:parse msg)))
    (case (teno.gui.json:op json)
      (:list_memos
       (teno:service-list-memos
        service))
      (:add_memo
       (teno:service-add-memo
        service
        (teno.gui.json:arg json "text")))
      (t
       (format t "Unknown msg: ~A" msg)))))

(defun ws-server (env)
  (let* ((ws (websocket-driver:make-server env))
         (service (make-service ws)))
    (setq *service* service)
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
