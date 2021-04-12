(in-package :teno.db.rdb)

(defmethod teno.memo:load-memo-by-id ((conn connection)
                                      (memo-id teno.id:id))
  (memo-select-by-id conn memo-id))

(defmethod teno.memo:load-memos ((conn connection))
  (memo-select conn))

(defmethod teno.memo:save-memo ((conn connection)
                                (memo teno.memo:memo))
  (memo-insert conn memo)
  (memo-text-insert conn
                    (teno.memo:memo-id memo)
                    (teno.memo:construct-text
                     :type teno.memo::+plain+
                     :string ""))
  (values))
  
(defmethod teno.memo:delete-memo ((conn connection)
                                  (memo-id teno.id:id))
  (let ((id-list (list memo-id)))
    (memo-text-delete conn id-list)
    (memo-delete conn id-list)))

(defmethod teno.memo:load-memo-text-by-id ((conn connection)
                                           (memo-id teno.id:id))
  (memo-text-select conn memo-id))

(defmethod teno.memo:load-memo-head-text-strings-by-ids ((conn connection)
                                                         (ids list))
  (let ((id->string (make-hash-table :test #'equal)))
    (loop for (id-string text-string)
               in (memo-text-head-string-select conn ids)
          do (setf (gethash id-string id->string) text-string))
    (mapcar (lambda (id)
              (gethash (teno.id:to-string id) id->string))
            ids)))

(defmethod teno.memo:update-memo-text ((conn connection)
                                       (memo teno.memo:memo)
                                       (text teno.memo:text))
  (memo-text-update conn (teno.memo:memo-id memo) text))
