(defpackage :teno.db.rdb
  (:use :cl)
  (:export :connection
           :insert-into
           :select-from
           :delete-from
           :timestamp-to-string
           :parse-timestamp
   
           :memo-select-by-id
           :memo-select
           :memo-insert
           :memo-delete
           :memo-text-select
           :memo-text-insert
           :memo-text-delete
           :memo-text-update))
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

(defgeneric memo-select-by-id (conn node-id))

(defgeneric memo-select (conn))

(defgeneric memo-insert (conn memo))

(defgeneric memo-delete (conn memo-id-list))


(defgeneric memo-text-select (conn memo-id))

(defgeneric memo-text-insert (conn node-id string))

(defgeneric memo-text-delete (conn memo-id-list))

(defgeneric memo-text-update (conn memo-id string))

;;;

(defun parse-memo (conn row)
  (teno:construct-memo
   (teno.id:parse (first row))
   (parse-timestamp conn (second row))))

(defmethod memo-select-by-id ((conn connection) (memo-id teno.id:id))
  (single
   (alexandria:curry #'parse-memo conn)
   (select-from conn "memo_id, created_on" "memos"
    :where `(:= "memo_id" (:p ,(teno.id:to-string memo-id))))))


(defmethod memo-insert ((conn connection) (memo teno:memo))
  (insert-into conn "memos" '("memo_id" "created_on")
   (list (list
          (teno.id:to-string (teno:memo-id memo))
          (timestamp-to-string conn (teno:memo-created-on memo))))))

(defmethod memo-delete ((conn connection) (memo-id-list list))
  (delete-from conn "memos"
   :where `(:in "memo_id" (:p ,(mapcar #'teno.id:to-string memo-id-list)))))

(defmethod memo-text-select ((conn connection) (memo-id teno.id:id))
  (single
   #'car
   (select-from conn "string" "memo_text"
    :where `(:= "memo_id" (:p ,(teno.id:to-string memo-id))))))

(defmethod memo-text-insert ((conn connection)
                             (memo-id teno.id:id)
                             (string string))
  (insert-into conn "memo_text" '("memo_id" "string")
   (list (list
          (teno.id:to-string memo-id)
          string))))

(defmethod memo-text-delete ((conn connection) (memo-id-list list))
  (delete-from conn "memo_text"
   :where `(:in "memo_id" (:p ,(mapcar #'teno.id:to-string memo-id-list)))))
