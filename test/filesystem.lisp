(in-package :cl-async-test)
(in-suite cl-async-test-core)

(test mkdtemp
  (with-test-event-loop ()
    (as:mkdtemp (uiop:merge-pathnames*
                 "tstXXXXXX"
                 (uiop:temporary-directory))
                (called-once
                 #'(lambda (path)
                     (let ((temp-p (uiop:subpathp path (uiop:temporary-directory))))
                       (is-true temp-p)
                       (when temp-p
                         (uiop:delete-empty-directory path))))))))

(test mkdtemp-fail
  (signals as:filesystem-enoent
    (with-test-event-loop ()
      (as:mkdtemp (format nil "/whatever-~36r/abcXXXXXX" (random (expt 2 256)))
                  #'never))))
