(in-package :cl-async)

(defclass poller ()
  ((c :accessor poller-c :initarg :c :initform (cffi:null-pointer))
   (freed :accessor poller-freed :reader poller-freed-p :initform nil))
  (:documentation "Wraps a polling handle."))

(defun free-poller (poller)
  "Stop and free a poller."
  (unless (poller-freed-p poller)
    (setf (poller-freed poller) t)
    (let ((poll-c (poller-c poller)))
      (when poll-c
        (uv:uv-poll-stop poll-c)
        (uv:uv-close poll-c (cffi:callback poll-close-cb))))))

(define-c-callback poll-close-cb :void ((poll-c :pointer))
  "Called when a poller closes."
  (free-pointer-data poll-c :preserve-pointer t)
  (uv:free-handle poll-c))

(define-c-callback poll-cb :void ((poll-c :pointer) (status :int) (events :int))
  "Called when something happens on a polled FD."
  (let* ((callbacks (get-callbacks poll-c))
         (poll-cb (getf callbacks :poll-cb))
         (event-cb (getf callbacks :event-cb)))
    (catch-app-errors event-cb
      (if (< status 0)
          ;; got an error, pass it along
          (event-handler status event-cb)
          ;; kewl, forward the event(s) along
          (let ((events-named nil))
            (when (< 0 (logand events (cffi:foreign-enum-value 'uv:uv-poll-event :readable)))
              (push :readable events-named))
            (when (< 0 (logand events (cffi:foreign-enum-value 'uv:uv-poll-event :writable)))
              (push :writable events-named))
            (funcall poll-cb events-named))))))

(defun poll (fd poll-cb &key event-cb (poll-for '(:readable :writable)) socket)
  "Poll an OS FD. If the FD is a socket, :socket t must be passed."
  (check-event-loop-running)
  (let* ((poll-c (uv:alloc-handle :poll))
         (poller (make-instance 'poller :c poll-c))
         (fn (if socket
                 'uv:uv-poll-init-socket
                 'uv:uv-poll-init)))
    (funcall fn (event-base-c *event-base*) poll-c fd)
    (let ((events 0))
      (dolist (event poll-for)
        (let ((event-val (case event
                           (:readable (cffi:foreign-enum-value 'uv:uv-poll-event :readable))
                           (:writable (cffi:foreign-enum-value 'uv:uv-poll-event :writable)))))
          (setf events (logior events event-val))))
      (save-callbacks poll-c (list :poll-cb poll-cb
                                   :event-cb event-cb))
      (uv:uv-poll-start poll-c events (cffi:callback poll-cb)))
    poller))

