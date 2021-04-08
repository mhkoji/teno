(in-package :teno)

(defmethod load-note-by-id ((db teno.db:db) note-id)
  (teno.db:with-connection (conn db)
    (teno.db:note-select-by-id conn note-id)))

(defmethod load-notes ((db teno.db:db))
  (teno.db:with-connection (conn db)
    (teno.db:note-select conn)))

(defmethod save-note ((db teno.db:db) note)
  (teno.db:with-connection (conn db)
    (teno.db:note-insert conn note)
    (teno.db:note-text-insert conn (note-id note) ""))
  (values))
  
(defmethod delete-note ((db teno.db:db) note-id)
  (let ((id-list (list note-id)))
    (teno.db:with-connection (conn db)
      (teno.db:note-text-delete conn id-list)
      (teno.db:note-delete conn id-list))))


(defmethod note-text ((db teno.db:db) note)
  (let ((octets
         (teno.db:with-connection (conn db)
           (teno.db:note-text-select conn (note-id note)))))
    (when octets
      (babel:octets-to-string octets))))

(defmethod update-note-text ((db teno.db:db) note string)
  (teno.db:with-connection (conn db)
    (teno.db:note-text-update conn (note-id note) string)
    (values)))
