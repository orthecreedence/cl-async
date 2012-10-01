(in-package :cl-async)

(cffi:defcallback timer-cb :void ((fd :pointer) (what :pointer) (data :pointer))
  "Callback used by the async timer system to find and run user-specified
   callbacks on timer events."
  (declare (ignore fd what))
  (let ((cb (car (get-callbacks data))))
    (when cb (funcall cb))))

(defun timer (time-s callback)
  "Run a function, asynchronously, after the specified amount of seconds. An
   event loop must be running for this to work."
  (check-event-loop-running)
  (multiple-value-bind (time-sec time-usec) (split-usec-time time-s)
    (make-foreign-type (time-c (le::cffi-type le::timeval))
                       (('le::tv-sec time-sec)
                        ('le::tv-usec time-usec))
      (let* ((pointer (cffi:foreign-alloc :char :count 0))
             (ev (le:event-new *event-base* -1 0 (cffi:callback timer-cb) pointer)))
        (save-callbacks pointer callback)
        (le:event-add ev time-c)))))

