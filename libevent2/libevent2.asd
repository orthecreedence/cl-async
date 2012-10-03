(asdf:defsystem libevent2
  :depends-on (#:cffi)
  :components ((:file "libevent2")
               (:file "wrapper" :depends-on ("libevent2"))
               (:file "bindings" :depends-on ("wrapper"))
               (:file "exports" :depends-on ("bindings"))
               (:file "accessors" :depends-on ("exports"))))
