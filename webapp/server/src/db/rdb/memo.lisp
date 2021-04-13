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

(defmethod teno.memo:memo-text ((conn connection)
                                (memo teno.memo:memo))
  (memo-text-select conn (teno.memo:memo-id memo)))

(defmethod teno.memo:update-memo-text ((conn connection)
                                       (memo teno.memo:memo)
                                       (text teno.memo:text))
  (memo-text-update conn (teno.memo:memo-id memo) text))

(defmethod teno.memo:load-head-text-memos ((conn connection))
  (let ((memos (teno.memo:load-memos conn)))
    (when memos
      (let ((ids (mapcar #'teno.memo:memo-id memos))
            (id->string (make-hash-table :test #'equal)))
        (loop for (id-string text-string) in (memo-text-head-string-select
                                              conn ids)
              do (setf (gethash id-string id->string) text-string))
        (mapcar (lambda (memo)
                  (let ((id-string (teno.id:to-string
                                    (teno.memo:memo-id memo))))
                    (change-class memo 'teno.memo:head-text-memo
                                  :string (gethash id-string id->string))))
                memos)))))
