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
           :memo-text-update
           :memo-text-string-select))
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

(defgeneric memo-text-insert (conn node-id text))

(defgeneric memo-text-delete (conn memo-id-list))

(defgeneric memo-text-update (conn memo-id text))

(defgeneric memo-text-string-select (conn memo-id-list))

;;;

(defun parse-memo (conn row)
  (teno.memo:construct-memo
   :id (teno.id:parse (first row))
   :created-on (parse-timestamp conn (second row))))

(defmethod memo-select-by-id ((conn connection) (memo-id teno.id:id))
  (single
   (alexandria:curry #'parse-memo conn)
   (select-from conn "memo_id, created_on" "memos"
    :where `(:= "memo_id" (:p ,(teno.id:to-string memo-id))))))


(defmethod memo-insert ((conn connection)
                        (memo teno.memo:memo))
  (insert-into conn "memos" '("memo_id" "created_on")
   (list (list
          (teno.id:to-string (teno.memo:memo-id memo))
          (timestamp-to-string conn (teno.memo:memo-created-on memo))))))

(defmethod memo-delete ((conn connection) (memo-id-list list))
  (delete-from conn "memos"
   :where `(:in "memo_id" (:p ,(mapcar #'teno.id:to-string memo-id-list)))))


(defmethod memo-text-select ((conn connection) (memo-id teno.id:id))
  (single
   (lambda (row)
     (teno.memo:make-text
      :string (let ((octets (first row)))
                (when octets
                  (babel:octets-to-string octets)))
      :type (teno.memo:parse-text-type (second row))))
   (select-from conn "string, type" "memo_text"
    :where `(:= "memo_id" (:p ,(teno.id:to-string memo-id))))))

(defmethod memo-text-insert ((conn connection)
                             (memo-id teno.id:id)
                             (text teno.memo:text))
  (insert-into conn "memo_text" '("memo_id" "string" "type")
   (list (list
          (teno.id:to-string memo-id)
          (teno.memo:text-string text)
          (symbol-name (teno.memo:text-type text))))))
          

(defmethod memo-text-delete ((conn connection) (memo-id-list list))
  (delete-from conn "memo_text"
   :where `(:in "memo_id" (:p ,(mapcar #'teno.id:to-string memo-id-list)))))
