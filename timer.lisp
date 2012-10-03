(in-package :cl-async)

(cffi:defcallback timer-cb :void ((fd :pointer) (what :pointer) (pointer :pointer))
  "Callback used by the async timer system to find and run user-specified
   callbacks on timer events."
  (declare (ignore fd what))
  (let* ((callbacks (get-callbacks pointer))
         (cb (getf callbacks :callback))
         (event-cb (getf callbacks :event-cb)))
    (catch-app-errors event-cb
      (unwind-protect
        (when cb (funcall cb))
        (clear-object-attachments pointer)
        (cffi:foreign-free pointer)))))

(defun timer (time-s callback &key event-cb)
  "Run a function, asynchronously, after the specified amount of seconds. An
   event loop must be running for this to work."
  (check-event-loop-running)
  (multiple-value-bind (time-sec time-usec) (split-usec-time time-s)
    (make-foreign-type (time-c (le::cffi-type le::timeval))
                       (('le::tv-sec time-sec)
                        ('le::tv-usec time-usec))
      (let* ((pointer (cffi:foreign-alloc :char :count 0))
             (ev (le:event-new *event-base* -1 0 (cffi:callback timer-cb) pointer)))
        (save-callbacks pointer (list :callback callback :event-cb event-cb))
        (le:event-add ev time-c)))))

