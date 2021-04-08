(asdf:defsystem :teno
  :serial t
  :pathname "src"

  :components
  ((:file "id")
   (:file "markdown")

   (:file "teno")
   (:file "db")
   (:file "persistent")

   (:file "db/rdb")
   (:file "db/mysql")

   (:module :server
    :pathname "server"
    :components
    ((:file "page")
     (:file "clack/util")
     (:file "clack/middleware")
     (:file "jsown")
     (:file "clack"))))

  :depends-on
  (:alexandria
   :cl-base64
   :cl-dbi
   :cl-markdown
   :cl-mysql
   :local-time
   :uuid

   :clack
   :cl-who
   :jsown
   :lack-request
   :myway))
