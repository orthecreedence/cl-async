(in-package :cl-async-test)

(in-suite cl-async-test-core)

(test fs-monitor
  (with-test-event-loop ()
    (test-timeout 3)
    (with-temporary-directory (dir)
      (let ((got-callback-p nil)
            fs-monitor)
        (setf fs-monitor
              (as:fs-watch dir
                           #'(lambda (monitor path rename-p change-p)
                               ;; Unfortunately uv_fs_event stuff is pretty random,
                               ;; so these arguments aren't supported across all
                               ;; the platforms and are pretty random anyway.
                               ;; Also, the callback can be invoked several
                               ;; times for the single change.
                               (declare (ignore path rename-p change-p))
                               (is (eq fs-monitor monitor))
                               (setf got-callback-p t))))
        (macrolet ((expecting-callback (&body body)
                     `(progn
                        (setf got-callback-p nil)
                        ,@body
                        (wait got-callback-p))))
          (as:with-delay ()
            (expecting-callback
             (ensure-directories-exist
              (uiop:merge-pathnames* #p"42/" dir)))
            (expecting-callback
             (alexandria:with-output-to-file (s (uiop:merge-pathnames* "4242" dir))
               (princ 42 s)))
            (expecting-callback
             (uiop:delete-empty-directory
              (uiop:merge-pathnames* #p"42/" dir)))))))))

(test fs-watch-failure ()
  (signals as:filesystem-enoent
    (with-test-event-loop ()
      (with-path-under-tmpdir (path "blabla")
        (as:fs-watch path #'never)
        (as:with-delay (1) nil)))))
