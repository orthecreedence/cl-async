(in-package :cl-async)

(cffi:defcallback fd-cb :void ((fd :int) (what :short) (data-pointer :pointer))
  (declare (ignore fd))
  (let* ((ev (deref-data-from-pointer data-pointer))
         (callbacks (get-callbacks data-pointer))
         (timeout-cb (getf callbacks :timeout-cb))
         (read-cb (getf callbacks :read-cb))
         (write-cb (getf callbacks :write-cb))
         (event-cb (getf callbacks :event-cb)))
    (catch-app-errors event-cb
      (when (and (< 0 (logand what le:+ev-read+))
                 read-cb)
         (funcall read-cb))
      (when (and (< 0 (logand what le:+ev-write+))
                 write-cb)
        (funcall write-cb))
      (when (and (< 0 (logand what le:+ev-timeout+))
                 timeout-cb)
         (funcall timeout-cb)))))

(defun fd-add (fd &key event-cb read-cb write-cb timeout-cb timeout)
  "Run a function, asynchronously, when the specified file descriptor is
   ready for write or read operations. An event loop must be running for
   this to work."
  (check-event-loop-running)
  (let* ((data-pointer (create-data-pointer))
         (ev (le:event-new *event-base*
                           fd
                           ;; listen to read/timeout events, and keep listening
                           (logior
                            (if timeout-cb le:+ev-timeout+ 0)
                            (if read-cb le:+ev-read+ 0)
                            (if write-cb le:+ev-write+ 0)
                            le:+ev-persist+)
                           (cffi:callback fd-cb)
                           data-pointer)))
    (save-callbacks data-pointer (list :read-cb read-cb
                                       :write-cb write-cb
                                       :timeout-cb timeout-cb
                                       :event-cb event-cb))
    (attach-data-to-pointer data-pointer ev)
    (if (numberp timeout)
        (with-struct-timeval time-c timeout
          (le:event-add ev time-c))
        (le:event-add ev (cffi:null-pointer)))))
