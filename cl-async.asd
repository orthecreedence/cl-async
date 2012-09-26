(asdf:defsystem cl-async
  :depends-on (#:cffi #:libevent2 #:babel)
  :components
  ((:file "async")))
