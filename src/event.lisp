(in-package :cl-async)

(define-condition event-freed (event-error)
  ((event :initarg :event :accessor event-freed-event :initform nil))
  (:documentation "Thrown when a freed event is operated on."))

(defclass event ()
  ((c :accessor event-c :initarg :c :initform (cffi:null-pointer))
   (freed :accessor event-freed :reader event-freed-p :initform nil))
  (:documentation "Wraps a C libuv event object."))

(defun check-event-unfreed (event)
  "Checks that an event being operated on is not freed."
  (when (event-freed event)
    (error 'event-freed :event event :msg "Freed event being operated on")))

(defun free-event (event)
  "Free a cl-async event object and any resources it uses."
  (check-event-unfreed event)
  (setf (event-freed event) t)
  (let ((timer-c (event-c event)))
    (when (zerop (uv:uv-is-closing timer-c))
      (uv:uv-close timer-c (cffi:callback timer-close-cb)))))

(defun remove-event (event)
  "Remove a pending event from the event loop."
  (check-event-unfreed event)
  (let ((timer-c (event-c event)))
    (uv:uv-timer-stop timer-c))
  t)

(defmethod ref ((handle event))
  (uv:uv-ref (event-c handle)))

(defmethod unref ((handle event))
  (uv:uv-unref (event-c handle)))

(defun add-event (event &key timeout activate)
  "Add an event to its specified event loop (make it pending). If given a
   :timeout (in seconds) the event will fire after that amount of time, unless
   it's removed or freed."
  (declare (ignore activate)) ;; compatibility-only
  (check-event-unfreed event)
  (let ((timer-c (event-c event)))
    (uv:uv-timer-start timer-c (cffi:callback timer-cb) (round (* (or timeout 0) 1000)) 0)))

(define-c-callback timer-cb :void ((timer-c :pointer))
  "Callback used by the async timer system to find and run user-specified
   callbacks on timer events."
  (let* ((event (deref-data-from-pointer timer-c))
         (callbacks (get-callbacks timer-c))
         (cb (getf callbacks :callback))
         (event-cb (getf callbacks :event-cb)))
    (catch-app-errors event-cb
      (unwind-protect
        (when cb (funcall cb))
        (unless (event-freed-p event)
          (free-event event))))))

(define-c-callback timer-close-cb :void ((timer-c :pointer))
  "Called when a timer closes."
  (free-pointer-data timer-c :preserve-pointer t)
  (uv:uv-timer-stop timer-c)
  (uv:free-handle timer-c))

(defun delay (callback &key time event-cb)
  "Run a function, asynchronously, after the specified amount of seconds. An
   event loop must be running for this to work.

   If time is nil, callback is still called asynchronously, but is queued in the
   event loop with no delay."
  (check-event-loop-running)
  (let* ((timer-c (uv:alloc-handle :timer))
         (event (make-instance 'event :c timer-c)))
    (uv:uv-timer-init (event-base-c *event-base*) timer-c)
    (save-callbacks timer-c (list :callback callback :event-cb event-cb))
    (attach-data-to-pointer timer-c event)
    (add-event event :timeout time :activate t)
    event))

(defmacro with-delay ((&optional (seconds 0)) &body body)
  "Nicer syntax for delay function."
  `(delay (lambda () ,@body) :time ,seconds))

(defun interval (callback &key time event-cb)
  "Run a function, asynchronously, every `time` seconds. This function returns a
   function which, when called, cancels the interval."
  ;; TODO: convert to uv-timer w/repeat
  (let (event)
    (labels ((main ()
               (funcall callback)
               (when event
                 (setf event (as:delay #'main :time time :event-cb event-cb)))))
      (setf event (as:delay #'main :time time :event-cb event-cb))
      (lambda ()
        (when event
          (remove-event event)
          (setf event nil))))))

(defmacro with-interval ((seconds) &body body)
  "Nicer syntax for interval function."
  `(interval (lambda () ,@body) :time ,seconds))

(defun remove-interval (interval-fn)
  "Stops an interval from looping."
  (funcall interval-fn))

(defun make-event (callback &key event-cb)
  "Make an arbitrary event, and add it to the event loop. It *must* be triggered
   by (add-event <the event> :activate t) or it will sit, idle, for 100 years.
   Or you can remove/free it instead.

   This is useful for triggering arbitrary events, and can even be triggered
   from a thread outside the libuv loop."
  (delay callback :event-cb event-cb :time (* 100 31536000)))

(defmethod handle-cleanup ((handle-type (eql :timer)) handle)
  (let ((event (deref-data-from-pointer handle)))
    (unless (event-freed-p event)
      (free-event event))))
