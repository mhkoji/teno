(asdf:defsystem :teno
  :serial t

  :components
  ((:file "id")
   (:file "markdown")

   (:file "teno")
   (:file "db")
   (:file "persistent")

   (:file "db/rdb")
   (:file "db/mysql"))

  :depends-on
  (:alexandria
   :cl-base64
   :cl-dbi
   :cl-markdown
   :cl-mysql
   :local-time
   :uuid))
