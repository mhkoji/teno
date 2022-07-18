(asdf:defsystem :teno-websocket
  :serial t
  :pathname "src/"
  :components ((:file "websocket"))
  :depends-on (:teno
               :alexandria
               :clack
               :jsown
               :websocket-driver))
