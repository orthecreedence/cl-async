(in-package :cl-async)

(defun signal-sym (signo)
  "Does nothing. Can be used in the future to prevent collissions in the data/
   callback space if needed."
  signo)

(defun free-signal-handler (signo)
  "Clear a signal handler and unbind it."
  (when (find signo *signal-handlers*)
    (let* ((signo-sym (signal-sym signo))
           (data-pointer (deref-data-from-pointer signo-sym))
           (sig-data (deref-data-from-pointer data-pointer))
           (ev (getf sig-data :ev))
           (original-lisp-signal-handler (get sig-data :original-handler)))
      (le:event-del ev)
      (cffi:foreign-funcall "signal" :int signo :pointer original-lisp-signal-handler :pointer)
      (free-pointer-data signo-sym)
      (free-pointer-data data-pointer))
    (setf *signal-handlers* (remove signo *signal-handlers*))))

(defun clear-signal-handlers ()
  "Clear all bound signal handlers. Great for cleaning up when exiting an app."
  (dolist (signo (copy-list *signal-handlers*))
    (free-signal-handler signo))
  (setf *signal-handlers* nil))

(cffi:defcallback lisp-signal-cb :void ((signo :int))
  "Generic callback for lisp signal handling."
  (declare (ignore signo))
  (let ((callback (car (get-callbacks (signo-sym signo)))))
    (when (functionp callback)
      (funcall callback))))
     
(defun set-lisp-signal-handler (signo fn)
  "Replace the current handler for the signal number under signo, and return a
   pointer to the handler that is being replaced."
  (save-callbacks (signal-sym signo) fn)
  (cffi:foreign-funcall "signal" :int signo :pointer (cffi:callback lisp-signal-cb) :pointer))

(cffi:defcallback signal-cb :void ((signo :int) (event :short) (data-pointer :pointer))
  "All signals come through here."
  (let* ((callbacks (get-callbacks data-pointer))
         (signal-cb (getf callbacks :signal-cb))
         (event-cb (getf callbacks :event-cb))
         (ev (deref-data-from-pointer data-pointer)))
    (catch-app-errors event-cb
      (funcall signal-cb signo))))

(defun signal-handler (signo signal-cb &key event-cb)
  "Setup a one-time signal handler for the given signo. This also sets up a
   lisp signal handler, so if a signal comes through while lisp is running
   instead of the event loop, it will run the same callback. All signal handlers
   are restored on event loop exit."
  (check-event-loop-running)
  ;; un-bind this signal handler if it is already bound. this ensures we don't
  ;; lose the original lisp signal handler when we overwrite it.
  (free-signal-handler signo)
  (let* ((data-pointer (create-data-pointer))
         (ev (le:event-new *event-base* signo (logior le:+ev-signal+ le:+ev-persist+) (cffi:callback signal-cb) data-pointer))
         (lisp-signal-handler (set-lisp-signal-handler signo (lambda () (le:event-active ev))))
         (signo-sym (signal-sym signo)))
    (le:event-add ev (cffi:null-pointer))
    (save-callbacks data-pointer (list :signal-cb signal-cb :event-cb event-cb))
    (attach-data-to-pointer data-pointer (list :ev ev :original-handler lisp-signal-handler))
    ;; make sure we can find the event/original handler from just the signo
    (attach-data-to-pointer signo-sym data-pointer)
    ;; add this signal to the list of active signals
    (push signo *signal-handlers*)
    (add-event-loop-exit-callback (lambda () (free-signal-handler signo)))))

