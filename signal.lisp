(in-package :cl-async)

(defun signal-name (signo)
  "Turn a signal number (2, for example) into the symbol 'SIG2"
  (read-from-string (format nil "sig~a" signo)))

(cffi:defcallback signal-cb :void ((signo :int) (event :short) (arg :pointer))
  "All signals come through here."
  (let ((cb (car (get-callbacks (signal-name signo)))))
    (funcall cb signo)))

(defun setup-signal-handler (signo signal-cb)
  "Setup a signal handler for the current event base."
  (check-event-loop-running)
  (let ((ev (le:event-new *event-base* signo (logior le:+ev-signal+ le:+ev-persist+) (cffi:callback signal-cb) (cffi:null-pointer))))
    (le:event-add ev (cffi:null-pointer))
    (save-callbacks (signal-name signo) signal-cb)
    ;; cleanup
    (add-event-loop-exit-callback (lambda ()
                                    (le:event-del ev)
                                    (clear-object-attachments (signal-name signo))))))

