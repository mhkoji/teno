(defpackage :teno.server.clack.middleware
  (:use :cl :teno.server.clack.util)
  (:export :make))
(in-package :teno.server.clack.middleware)

(defun connect (mapper db)
  (connect-all mapper
    (("/"
      (lambda (params req)
        (declare (ignore params req))
        (html-response
         (teno.server.html:page
          "/static/gen/index.bundle.css"
          "/static/gen/index.bundle.js"))))
     ("/api/memos"
      (lambda (params req)
        (declare (ignore params req))
        (teno.db:with-connection (conn db)
          (json-response
           (teno.memo:group-by-created-on
            (teno.memo:load-head-text-memos conn))))))
     (("/api/memos/_add" :method :post)
      (lambda (params req)
        (declare (ignore params))
        (teno.db:with-connection (conn db)
          (json-response
           (teno.memo:memo-id
            (teno.memo:add-memo
             conn
             (cdr (assoc "text_string"
                         (lack.request:request-body-parameters req)
                         :test #'string=))
             teno.memo::+plain+)))))))))

(defun make (db)
  (let ((mapper (myway:make-mapper)))
    (connect mapper db)
    (mapper->middleware mapper)))
