(in-package :cl-async)

(define-condition event-freed (event-error)
  ((event :initarg :event :accessor event-freed-event :initform nil))
  (:report (lambda (c s) (format s "Freed event being operated on: ~a~%" c)))
  (:documentation "Thrown when a freed event is operated on."))

(defclass event ()
  ((c :accessor event-c :initarg :c :initform (cffi:null-pointer) :type cffi:foreign-pointer)
   (free-callback :accessor event-free-callback :initarg :free-callback :initform nil :type callback)
   (freed :accessor event-freed :reader event-freed-p :initform nil :type boolean))
  (:documentation "Wraps a C libevent event object."))

(declaim (inline check-event-unfreed))
(defun* (check-event-unfreed -> null) ((event (or event cffi:foreign-pointer)))
  "Checks that an event being operated on is not freed."
  (when (subtypep (type-of event) 'event)
    (when (event-freed event)
      (error 'event-freed :event event))))

(defun* (free-event -> null) ((event event))
  "Free a cl-async event object and any resources it uses. It *is* safe to free
   a pending/active event."
  (declare (optimize speed (debug 0) (safety 0)))
  (check-event-unfreed event)
  ;; run the free-callback (if any)
  (let ((free-cb (event-free-callback event)))
    (when free-cb (funcall free-cb event)))
  ;; free the event (also makes it inactive/non-pending before freeing)
  (le:event-free (event-c event))
  (setf (event-freed event) t)
  nil)

(defun* (remove-event -> boolean) ((event event))
  "Remove a pending event from the event loop. Returns t on success, nil on
   failure."
  (declare (optimize speed (debug 0)))
  (check-event-unfreed event)
  (let ((ret (le:event-del (event-c event))))
    (declare (type integer ret))
    (when (eq ret 0)
      t)))

(defun* (add-event -> integer) ((event event) &key ((timeout (or null real)) nil) ((activate boolean) nil))
  "Add an event to its specified event loop (make it pending). If given a
   :timeout (in seconds) the event will fire after that amount of time, unless
   it's removed or freed. If :activate is true and the event has no timeout,
   the event will be activated directly without being added to the event loop,
   and its callback(s) will be fired."
  (declare (optimize speed (debug 0) (safety 0)))
  (check-event-unfreed event)
  (let ((ev (event-c event)))
    (declare (type cffi:foreign-pointer ev))
    (cond ((realp timeout)
           (with-struct-timeval time-c timeout
             (le:event-add ev time-c)))
          (activate
           (le:event-active ev 0 0))
          (t
           (le:event-add ev (cffi:null-pointer))))))

(cffi:defcallback timer-cb :void ((fd :pointer) (what :pointer) (data-pointer :pointer))
  "Callback used by the async timer system to find and run user-specified
   callbacks on timer events."
  (declare (ignore fd what)
           (optimize speed (debug 0) (safety 0)))
  (let* ((event (deref-data-from-pointer data-pointer))
         (callbacks (get-callbacks data-pointer))
         (cb (getf callbacks :callback))
         (event-cb (getf callbacks :event-cb)))
    (declare (type event event)
             (type callback cb event-cb))
    (catch-app-errors event-cb
      (unwind-protect
        (when cb (funcall cb))
        (free-event event)))))

(cffi:defcallback fd-cb :void ((fd :int) (what :short) (data-pointer :pointer))
  "Called when an event watching a file descriptor is triggered."
  (declare (ignore fd))
  (let* (;(event (deref-data-from-pointer data-pointer))
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

(defun* (delay -> event) ((callback callback) &key ((time (or null real)) nil) ((event-cb callback) nil))
  "Run a function, asynchronously, after the specified amount of seconds. An
   event loop must be running for this to work.
   
   If time is nil, callback is still called asynchronously, but is queued in the
   event loop with no delay."
  (declare (optimize speed (debug 0) (safety 0)))
  (check-event-loop-running)
  (let* ((data-pointer (the cffi:foreign-pointer (create-data-pointer)))
         (ev (le:event-new (event-base-c *event-base*)
                           -1 0
                           (cffi:callback timer-cb)
                           data-pointer))
         (event (make-instance 'event
                               :c (the cffi:foreign-pointer ev)
                               :free-callback (lambda (event)
                                                (declare (ignore event))
                                                (free-pointer-data data-pointer)))))
    (declare (type cffi:foreign-pointer data-pointer ev)
             (type event event))
    (save-callbacks data-pointer (list :callback callback :event-cb event-cb))
    (attach-data-to-pointer data-pointer event)
    (add-event event :timeout time :activate t)
    event))

(defun watch-fd (fd &key event-cb read-cb write-cb timeout-cb timeout)
  "Run a function, asynchronously, when the specified file descriptor is
   ready for write or read operations. An event loop must be running for
   this to work."
  (declare (optimize speed (debug 0)))
  (check-event-loop-running)
  (let* ((data-pointer (create-data-pointer))
         (ev (le:event-new (event-base-c *event-base*)
                           fd
                           ;; listen to read/timeout events, and keep listening
                           (logior
                            (if timeout-cb le:+ev-timeout+ 0)
                            (if read-cb le:+ev-read+ 0)
                            (if write-cb le:+ev-write+ 0)
                            le:+ev-persist+)
                           (cffi:callback fd-cb)
                           data-pointer))
         (event (make-instance 'event
                               :c ev
                               :free-callback (lambda (event)
                                                (declare (ignore event))
                                                (free-pointer-data data-pointer)))))
    (save-callbacks data-pointer (list :read-cb read-cb
                                       :write-cb write-cb
                                       :timeout-cb timeout-cb
                                       :event-cb event-cb))
    (attach-data-to-pointer data-pointer event)
    (add-event event :timeout timeout)
    event))

(defun fd-add (fd &key event-cb read-cb write-cb timeout-cb timeout)
  "Deprecated. Backwards compatible function wrapping watch-fd."
  (watch-fd fd
            :event-cb event-cb
            :read-cb read-cb
            :write-cb write-cb
            :timeout-cb timeout-cb
            :timeout timeout))
