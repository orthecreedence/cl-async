(asdf:defsystem cl-async
  :depends-on (#:cffi #:libevent2 #:flexi-streams)
  :components
  ((:file "async")))
