(asdf:defsystem cl-async-test
  :author "Andrew Danger Lyon <orthecreedence@gmail.com>"
  :license "MIT"
  :version "0.2"
  :description "TESTS FOR Asynchronous operations for Common Lisp."
  :depends-on (#:cffi
               #:cl-async
               #:cl-async-ssl
               #:fiveam
               #:bordeaux-threads
               #:usocket
               #:flexi-streams
               #:ironclad)
  :components
  ((:module test
    :serial t
    :components ((:file "util")
                 (:file "base")
                 (:file "event")
                 (:file "dns")
                 (:file "tcp")
                 (:file "pipe")
                 (:file "async-stream")
                 (:file "threading")
                 (:file "tcp-ssl")
                 (:file "signal")
                 (:file "idle")
                 (:file "poll")
                 (:file "benchmarks")
                 (:file "run")
                 (:file "filesystem")
                 (:file "process")
                 (:file "fsevent")))))
