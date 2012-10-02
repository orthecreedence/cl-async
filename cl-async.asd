(asdf:defsystem cl-async
  :depends-on (#:cffi #:libevent2 #:babel #:cl-ppcre)
  :components
  ((:file "package")
   (:file "common" :depends-on ("package"))
   (:file "timer" :depends-on ("common"))
   (:file "dns" :depends-on ("common"))
   (:file "tcp" :depends-on ("dns"))
   (:file "http" :depends-on ("dns"))
   (:file "signal" :depends-on ("common"))))
