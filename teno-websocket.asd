(asdf:defsystem :teno-websocket
  :serial t
  :pathname "src/"
  :components ((:file "websocket"))
  :depends-on (:teno
               :clack
               :websocket-driver))
