(asdf:defsystem cl-async-repl
  :author "Andrew Danger Lyon <orthecreedence@gmail.com>"
  :license "MIT"
  :version "0.1"
  :description "REPL integration for CL-ASYNC."
  :depends-on (#:cl-async #:bordeaux-threads)
  :components
  ((:file "src/repl")))
