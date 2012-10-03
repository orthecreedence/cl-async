(in-package :cl-async)

(cffi:defcallback timer-cb :void ((fd :pointer) (what :pointer) (data-pointer :pointer))
  "Callback used by the async timer system to find and run user-specified
   callbacks on timer events."
  (declare (ignore fd what))
  (let* ((ev (deref-data-from-pointer data-pointer))
         (callbacks (get-callbacks data-pointer))
         (cb (getf callbacks :callback))
         (event-cb (getf callbacks :event-cb)))
    (catch-app-errors event-cb
      (unwind-protect
        (when cb (funcall cb))
        (free-pointer-data data-pointer)
        (le:event-free ev)))))

(defun timer (time-s callback &key event-cb)
  "Run a function, asynchronously, after the specified amount of seconds. An
   event loop must be running for this to work."
  (check-event-loop-running)
  (multiple-value-bind (time-sec time-usec) (split-usec-time time-s)
    (make-foreign-type (time-c (le::cffi-type le::timeval))
                       (('le::tv-sec time-sec)
                        ('le::tv-usec time-usec))
      (let* ((data-pointer (create-data-pointer))
             (ev (le:event-new *event-base* -1 0 (cffi:callback timer-cb) data-pointer)))
        (save-callbacks data-pointer (list :callback callback :event-cb event-cb))
        (attach-data-to-pointer data-pointer ev)
        (le:event-add ev time-c)))))

