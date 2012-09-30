(asdf:defsystem cl-async
  :depends-on (#:cffi #:libevent2 #:babel)
  :components
  ((:file "package")
   (:file "common" :depends-on ("package"))
   (:file "timer" :depends-on ("common"))
   (:file "tcp" :depends-on ("common"))
   (:file "http" :depends-on ("common"))
   (:file "signal" :depends-on ("common"))))
