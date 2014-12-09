(in-package :cl-async)

(defclass idler ()
  ((c :accessor idler-c :initarg :c :initform (cffi:null-pointer))
   (freed :accessor idler-freed :reader idler-freed-p :initform nil))
  (:documentation "Wraps a libuv idle handle."))

(defun free-idler (idler)
  "Stop and free an idler."
  (unless (idler-freed-p idler)
    (setf (idler-freed idler) t)
    (let ((idle-c (idler-c idler)))
      (when idle-c
        (uv:uv-idle-stop idle-c)
        (uv:uv-close idle-c (cffi:callback idle-close-cb))))))

(define-c-callback idle-close-cb :void ((idle-c :pointer))
  "Called when an idler closes."
  (free-pointer-data idle-c :preserve-pointer t)
  (uv:free-handle idle-c))

(define-c-callback idle-cb :void ((handle :pointer))
  "Called when an idle handler fires."
  (let* ((callbacks (get-callbacks handle))
         (idle-cb (getf callbacks :idle-cb))
         (event-cb (getf callbacks :event-cb)))
    (catch-app-errors event-cb
      (when idle-cb (funcall idle-cb)))))

(defun idle (callback &key event-cb)
  "Calls `callback` once per event loop."
  (check-event-loop-running)
  (let* ((idle-c (uv:alloc-handle :idle))
         (idler (make-instance 'idler :c idle-c)))
    (uv:uv-idle-init (event-base-c *event-base*) idle-c)
    (save-callbacks idle-c (list :idle-cb callback
                                 :event-cb event-cb))
    (uv:uv-idle-start idle-c (cffi:callback idle-cb))
    idler))

