(in-package :cl-async)

(define-condition notifier-freed (event-error)
  ((notifier :initarg :notifier :accessor notifier-freed-notifier :initform nil))
  (:documentation "Thrown when a freed notifier is operated on."))

(defclass notifier ()
  ((c :accessor notifier-c :initarg :c :initform (cffi:null-pointer))
   (freed :accessor notifier-freed :reader notifier-freed-p :initform nil)
   (main-cb :accessor notifier-main-cb :initarg :main-cb :initform nil)
   (event-cb :accessor notifier-event-cb :initarg :event-cb :initform nil))
  (:documentation "Wraps a threading-enabled notifier."))

(defun check-notifier-unfreed (notifier)
  "Checks that an notifier being operated on is not freed."
  (when (notifier-freed notifier)
    (error 'notifier-freed :notifier notifier :msg "Freed notifier being operated on")))

(defun free-notifier (notifier)
  "Free a cl-async notifier object and any resources it uses."
  (check-notifier-unfreed notifier)
  (setf (notifier-freed notifier) t)
  (let ((async-c (notifier-c notifier)))
    (when (zerop (uv:uv-is-closing async-c))
      (uv:uv-close async-c (cffi:callback async-close-cb)))))

(defmethod ref ((handle notifier))
  (uv:uv-ref (notifier-c handle)))

(defmethod unref ((handle notifier))
  (uv:uv-unref (notifier-c handle)))

(define-c-callback async-close-cb :void ((async-c :pointer))
  "Called when an async handle is closed."
  (uv:free-handle async-c))

(define-c-callback async-cb :void ((async-c :pointer))
  "Called when an async notifier is triggered."
  (let* ((notifier (deref-data-from-pointer async-c))
         (callback (notifier-main-cb notifier))
         (event-cb (notifier-event-cb notifier)))
    (catch-app-errors event-cb
      (unwind-protect
        (when callback (funcall callback))
        (unless (notifier-freed-p notifier)
          (free-notifier notifier))))))

(defun make-notifier (callback &key event-cb)
  "Makes a notifier, an object that can trigger a callback from a thread other
   than the event loop thread."
  (check-event-loop-running)
  (let* ((async-c (uv:alloc-handle :async))
         (notifier (make-instance 'notifier :c async-c :main-cb callback :event-cb event-cb)))
    (let ((r (uv:uv-async-init (event-base-c *event-base*) async-c (cffi:callback async-cb))))
      (if (< r 0)
          (event-handler r event-cb :catch-errors t)
          (progn
            (attach-data-to-pointer async-c notifier)
            notifier)))))

(defun trigger-notifier (notifier)
  "Fires the callback attached to a notifier. Can be called from any thread."
  (let ((async-c (notifier-c notifier)))
    (uv:uv-async-send async-c)))

