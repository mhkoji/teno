(defpackage :teno.server.clack
  (:use :cl)
  (:export :start))
(in-package :teno.server.clack)

(defun system-relative-pathname (name)
  (asdf:system-relative-pathname (asdf:find-system :teno) name))

(defvar *db* (make-instance 'teno.db.rdb.mysql:mysql
              :db-name "teno"
              :locator (teno.db.rdb.mysql:make-locator
                        :user "root"
                        :host "127.0.0.1"
                        :port 3306)))

(defvar *static-root*
  (system-relative-pathname "../static/"))

(defvar *handler* nil)

;;;

(defun start (&key (port 6000)
                   (static-root *static-root*)
                   (db *db*)
                   (use-thread t))
  (when *handler*
    (clack:stop *handler*))
  (setq *handler*
        (clack:clackup
         (lack:builder
          (:static :path "/static/" :root static-root)

          (teno.server.clack.middleware:make db)

          (lambda (env)
            (declare (ignore env))
            '(302 (:location "/") nil)))
         :address "0.0.0.0"
	 ;; Don't have to invoke a debugger. No one can take care of it.
	 ;; setq after clackup because clackup set the var to T.
	 :debug nil
         :use-thread use-thread
         :port port)))
