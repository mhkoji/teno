(in-package :teno.db.rdb)

(defmethod teno:load-memo-by-id ((conn connection)
                                 (memo-id teno.id:id))
  (memo-select-by-id conn memo-id))

(defmethod teno:load-memos ((conn connection))
  (memo-select conn))

(defmethod teno:save-memo ((conn connection)
                           (memo teno:memo))
  (memo-insert conn memo)
  (memo-text-insert conn (teno:memo-id memo) "")
  (values))
  
(defmethod teno:delete-memo ((conn connection)
                             (memo-id teno.id:id))
  (let ((id-list (list memo-id)))
    (memo-text-delete conn id-list)
    (memo-delete conn id-list)))

(defmethod teno:memo-text ((conn connection)
                           (memo teno:memo))
  (let ((octets (memo-text-select conn (teno:memo-id memo))))
    (when octets
      (babel:octets-to-string octets))))
