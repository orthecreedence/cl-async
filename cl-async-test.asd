(asdf:defsystem cl-async-test
  :author "Andrew Danger Lyon <orthecreedence@gmail.com>"
  :license "MIT"
  :version "0.2"
  :description "TESTS FOR Asynchronous operations for Common Lisp."
  :depends-on (#:cffi #:cl-async #:eos #:bordeaux-threads #:usocket)
  :components
  ((:module test
    :serial t
    :components ((:file "util")
                 (:file "base")
                 (:file "event")
                 (:file "dns")
                 (:file "tcp")
                 (:file "tcp-stream")
                 (:file "threading")
				 ;(:file "tcp-ssl")
                 (:file "signal")
				 (:file "benchmarks")
                 (:file "run")))))

