(defpackage :teno.docker
  (:use :cl)
  (:export :main))
(in-package :teno.docker)
(ql:quickload :teno)

(defvar *locator*
  (teno.db.rdb.mysql:make-locator
   :user "root"
   :host "mysql"
   :port 3306))

;(setq *read-eval* nil)

(defun clack ()
  (teno.server.clack:start
   :static-root "/app/static/"

   :db (make-instance 'teno.db.rdb.mysql:mysql
                      :db-name "teno"
                      :locator *locator*)

   :use-thread nil))

(defun init ()
  (teno.db.rdb.mysql:create-database
   "/root/quicklisp/local-projects/teno/mysql/"
   "teno"
   *locator*))

#+sbcl
(defun main ()
  (let ((cmd (second sb-ext:*posix-argv*)))
    (cond ((string= cmd "clack") (clack))
          ((string= cmd "init") (init))
          (t (error "no such command: ~A" cmd)))))
