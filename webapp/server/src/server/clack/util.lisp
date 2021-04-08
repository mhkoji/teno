(defpackage :teno.server.clack.util
  (:use :cl)
  (:export :q
           :connect-all
           :mapper->middleware
           :html-response
           :json-response))
(in-package :teno.server.clack.util)

(defun q (req name)
  (let ((params (lack.request:request-parameters req)))
    (cdr (assoc name params :test #'string=))))

(defmacro connect-all (mapper arg-list)
  `(progn
     ,@(mapcar (lambda (arg)
                 (destructuring-bind (endpoint fn) arg
                   (destructuring-bind (url &rest rest)
                       (alexandria:ensure-list endpoint)
                     `(myway:connect ,mapper ,url
                                     (lambda (params)
                                       (lambda (req)
                                         (,fn params req)))
                                     ,@rest))))
               arg-list)))

(defun mapper->middleware (mapper)
  (lambda (app)
    (lambda (env)
      (or (let ((request (lack.request:make-request env)))
            (multiple-value-bind (handler foundp)
                (let ((method (lack.request:request-method request))
                      (path-info (lack.request:request-path-info request)))
                  (myway:dispatch mapper path-info :method method))
              (when foundp
                (funcall handler request))))
          (funcall app env)))))

(defun html-response (body-string &key (status-code 200))
  `(,status-code (:content-type "text/html")
                 (,body-string)))

(defun json-response (value &key (status-code 200) (success t))
  `(,status-code (:content-type "application/json")
                 (,(jsown:to-json
                    (jsown:new-js
                      ("success" (or success :f))
                      ("value" value))))))
