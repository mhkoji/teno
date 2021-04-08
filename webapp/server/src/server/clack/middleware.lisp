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
         (teno.server.page:page
          "/static/gen/index.bundle.css"
          "/static/gen/index.bundle.js"))))
     #+ni
     ("/notes/:note-id"
      (lambda (params req)
        (declare (ignore req))
        (let* ((note-id
                (teno.id:parse-short-or-nil
                 (getf params :note-id)))
               (note
                (teno:load-note-by-id db note-id)))
          (if note
              (html-response
               (teno.server.page:note
                "/static/gen/note.bundle.css"
                "/static/gen/note.bundle.js"
                note))
              (html-response
               (teno.server.page:not-found)
               :status-code 404)))))
     ("/api/notes"
      (lambda (params req)
        (declare (ignore params req))
        (json-response (teno:load-notes db))))
     (("/api/notes/_create" :method :post)
      (lambda (params req)
        (declare (ignore params req))
        (let ((note (teno:create-note db)))
          (json-response
           (teno:note-id note))))))))

(defun make (db)
  (let ((mapper (myway:make-mapper)))
    (connect mapper db)
    (mapper->middleware mapper)))
