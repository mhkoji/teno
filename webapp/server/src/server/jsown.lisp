(defpackage :teno.server.jsown
  (:use :cl))
(in-package :teno.server.jsown)

(defmethod jsown:to-json ((o teno.id:id))
  (jsown:to-json
   (teno.id:to-string-short o)))

(defmethod jsown:to-json ((o local-time:timestamp))
  (jsown:to-json
   (local-time:to-rfc3339-timestring o)))

(defmethod jsown:to-json ((o teno:memo))
  (jsown:to-json
   (jsown:new-js
     ("id" (teno:memo-id o))
     ("created_on" (teno:memo-created-on o)))))
