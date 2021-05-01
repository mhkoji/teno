(asdf:defsystem :teno
  :serial t
  :pathname "src"

  :components
  ((:file "id")
   (:file "markdown")

   (:file "db")
   (:file "memo")

   (:file "db/rdb")
   (:file "db/rdb/memo")
   (:file "db/rdb/mysql")

   (:module :server
    :pathname "server"
    :components
    ((:file "html")
     (:file "clack/util")
     (:file "clack/middleware")
     (:file "jsown")
     (:file "clack"))))

  :depends-on
  (:alexandria
   :cl-base64
   :cl-markdown
   :local-time
   :myqlo
   :uuid

   :clack
   :cl-who
   :jsown
   :lack-request
   :myway))
