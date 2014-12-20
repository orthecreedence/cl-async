(in-package :cl-async-test)
(in-suite cl-async-test-core)

(test mkdtemp
  (let ((count 0))
    (as:with-event-loop (:catch-app-errors t)
      (as:mkdtemp (uiop:merge-pathnames*
                   "tstXXXXXX"
                   (uiop:temporary-directory))
                  #'(lambda (path)
                      (incf count)
                      (let ((temp-p (uiop:subpathp path (uiop:temporary-directory))))
                        (is-true temp-p)
                        (when temp-p
                          (uiop:delete-empty-directory path))))))
    (is (= 1 count))))

(test mkdtemp-fail
  (let ((count 0))
    (signals as:filesystem-enoent
      (async-let ((count 0))
        (as:mkdtemp (format nil "/whatever-~36r/abcXXXXXX" (random (expt 2 256)))
                    #'(lambda (path)
                        (declare (ignore path))
                        (incf count)))))
    (is (= 0 count))))
