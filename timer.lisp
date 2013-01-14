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

(defun delay (callback &key time event-cb)
  "Run a function, asynchronously, after the specified amount of seconds. An
   event loop must be running for this to work.
   
   If time is nil, callback is still called asynchronously, but is queued in the
   event loop with no delay."
  ;; TODO: if time is in fact 0, just create and active a new vanilla event
  ;; instead of a timer event. should save some processing time
  (check-event-loop-running)
  (let* ((data-pointer (create-data-pointer))
         (ev (le:event-new *event-base* -1 0 (cffi:callback timer-cb) data-pointer)))
    (save-callbacks data-pointer (list :callback callback :event-cb event-cb))
    (attach-data-to-pointer data-pointer ev)
    (if (numberp time)
        ;; time is not bogus, make a timeval struct and add the event to the
        ;; loop for delayed processing.
        (with-struct-timeval time-c time
          (le:event-add ev time-c))
        ;; there was no time specified (or it wasn't a number), so fire up the
        ;; event to be processed with no delay
        (le:event-active ev 0 0))))

(defun timer (time-s callback &key event-cb)
  "Deprecated, use delay."
  (delay callback :time time-s :event-cb event-cb))

