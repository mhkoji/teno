(asdf:defsystem :teno
  :serial t
  :pathname "src/"
  :components ((:file "teno")
               (:file "gui/json")
               (:file "store/in-memory"))
  :depends-on (:alexandria
               :jsown))
