(in-package :cl-async)

(cffi:defcallback signal-cb :void ((signo :int) (event :short) (data-pointer :pointer))
  "All signals come through here."
  (let ((cb (car (get-callbacks data-pointer))))
    (funcall cb signo)))

(defun setup-signal-handler (signo signal-cb)
  "Setup a signal handler for the current event base."
  (check-event-loop-running)
  (let ((data-pointer (create-data-pointer))
        (ev (le:event-new *event-base* signo (logior le:+ev-signal+ le:+ev-persist+) (cffi:callback signal-cb) data-pointer)))
    (le:event-add ev (cffi:null-pointer))
    (save-callbacks data-pointer signal-cb)
    ;; cleanup
    (add-event-loop-exit-callback (lambda ()
                                    (le:event-del ev)
                                    (free-pointer-data data-pointer)))))

