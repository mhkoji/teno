(defpackage :teno.db.rdb
  (:use :cl)
  (:export :connection
           :insert-into
           :select-from
           :delete-from
           :timestamp-to-string
           :parse-timestamp))
(in-package :teno.db.rdb)

(defclass connection (teno.db:connection) ())

(defgeneric insert-into (conn table-name column-name-list values-list))

(defgeneric select-from (conn column-names table-name &key where order-by))

(defgeneric delete-from (conn table-name &key where))

(defun single (row-parser select-result)
  (car (mapcar row-parser select-result)))

(defgeneric timestamp-to-string (conn timestamp))

(defgeneric parse-timestamp (conn object))

;;;

(defun parse-note (conn row)
  (teno:construct-note
   (teno.id:parse (first row))
   (parse-timestamp conn (second row))))

(defmethod teno.db:note-select-by-id ((conn connection)
                                      (note-id teno.id:id))
  (single
   (alexandria:curry #'parse-note conn)
   (select-from conn "note_id, created_on" "notes"
    :where `(:= "note_id" (:p ,(teno.id:to-string note-id))))))


(defmethod teno.db:note-insert ((conn connection)
                                (note teno:note))
  (insert-into conn "notes" '("note_id" "created_on")
   (list (list
          (teno.id:to-string (teno:note-id note))
          (timestamp-to-string conn (teno:note-created-on note))))))

(defmethod teno.db:note-delete ((conn connection)
                                (note-id-list list))
  (delete-from conn "notes"
   :where `(:in "note_id" (:p ,(mapcar #'teno.id:to-string note-id-list)))))

(defmethod teno.db:note-text-select ((conn connection)
                                     (note-id teno.id:id))
  (single
   #'car
   (select-from conn "string" "note_text"
    :where `(:= "note_id" (:p ,(teno.id:to-string note-id))))))

(defmethod teno.db:note-text-insert ((conn connection)
                                     (note-id teno.id:id)
                                     (string string))
  (insert-into conn "note_text" '("note_id" "string")
   (list (list
          (teno.id:to-string note-id)
          string))))

(defmethod teno.db:note-text-delete ((conn connection)
                                     (note-id-list list))
  (delete-from conn "note_text"
   :where `(:in "note_id" (:p ,(mapcar #'teno.id:to-string note-id-list)))))
