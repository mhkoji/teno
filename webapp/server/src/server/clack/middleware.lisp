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
     #+ni
     ("/memos/:memo-id"
      (lambda (params req)
        (declare (ignore req))
        (let* ((memo-id
                (teno.id:parse-short-or-nil
                 (getf params :memo-id)))
               (memo
                (teno:load-memo-by-id db memo-id)))
          (if memo
              (html-response
               (teno.server.html:memo
                "/static/gen/memo.bundle.css"
                "/static/gen/memo.bundle.js"
                memo))
              (html-response
               (teno.server.html:not-found)
               :status-code 404)))))
     ("/api/memos"
      (lambda (params req)
        (declare (ignore params req))
        (teno.db:with-connection (conn db)
          (json-response (teno:load-memos conn)))))
     (("/api/memos/_create" :method :post)
      (lambda (params req)
        (declare (ignore params req))
        (teno.db:with-connection (conn db)
          (let ((memo (teno:create-memo conn)))
            (json-response
             (teno:memo-id memo)))))))))

(defun make (db)
  (let ((mapper (myway:make-mapper)))
    (connect mapper db)
    (mapper->middleware mapper)))
