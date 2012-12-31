(in-package :cl-async-test)

(defun run-tests (&key ssl)
  (when ssl
    (unless (find-package :cl-async-ssl)
      (asdf:oos 'asdf:load-op :cl-async-ssl))
    (load (asdf:system-relative-pathname :cl-async #P"test/tcp-ssl")))
  (if ssl
      (run! 'cl-async-test)
      (run! 'cl-async-test-core)))
