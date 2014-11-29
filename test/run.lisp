(in-package :cl-async-test)

(defun run-tests (&key ssl threading)
  (run! 'cl-async-test-core)
  (when ssl
    (unless (find-package :cl-async-ssl)
      (asdf:oos 'asdf:load-op :cl-async-ssl))
    (load (asdf:system-relative-pathname :cl-async #P"test/tcp-ssl"))
    (run! 'cl-async-ssl-test))
  (when threading
    (run! 'cl-async-threading-test)))

