(in-package :cl-async)

(defconstant +sighup+ 1)
(defconstant +sigint+ 2)
(defconstant +sigquit+ 3)
(defconstant +sigill+ 4)
(defconstant +sigtrap+ 5)
(defconstant +sigabrt+ 6)
(defconstant +sigemt+ 7)
(defconstant +sigfpe+ 8)
(defconstant +sigkill+ 9)
(defconstant +sigbus+ 10)
(defconstant +sigsegv+ 11)
(defconstant +sigsys+ 12)
(defconstant +sigpipe+ 13)
(defconstant +sigalrm+ 14)
(defconstant +sigterm+ 15)
(defconstant +sigurg+ 16)
(defconstant +sigstop+ 17)
(defconstant +sigtstp+ 18)
(defconstant +sigcont+ 19)
(defconstant +sigchld+ 20)
(defconstant +sigttin+ 21)
(defconstant +sigttou+ 22)
(defconstant +sigio+ 23)
(defconstant +sigxcpu+ 24)
(defconstant +sigxfsz+ 25)
(defconstant +sigvtalrm+ 26)
(defconstant +sigprof+ 27)
(defconstant +sigwinch+ 28)
(defconstant +siginfo+ 29)
(defconstant +sigusr1+ 30)
(defconstant +sigusr2+ 31)

(defun signal-pointer (signo)
  "Creates a pointer from a signal number. Not that this doesn't need to be
   freed since we're not allocating anything...just creating a pointer object
   with a specific address (which can be GCed)."
  (cffi:make-pointer signo))

(defun free-signal-handler (signo)
  "Clear a signal handler and unbind it."
  (when (find signo (event-base-signal-handlers *event-base*))
    (let* ((signo-pt (signal-pointer signo))
           (watcher (deref-data-from-pointer signo-pt)))
      (uv:uv-signal-stop watcher)
      (uv:uv-close watcher (cffi:callback signal-close-cb)))
    (setf (event-base-signal-handlers *event-base*) (remove signo (event-base-signal-handlers *event-base*)))))

(defun clear-signal-handlers ()
  "Clear all bound signal handlers. Great for cleaning up when exiting an app."
  (dolist (signo (copy-list (event-base-signal-handlers *event-base*)))
    (free-signal-handler signo))
  (setf (event-base-signal-handlers *event-base*) nil))

(define-c-callback signal-close-cb :void ((watcher :pointer))
  "Called when a signal handler closes."
  (let* ((sig-data (deref-data-from-pointer watcher))
         (signo (getf sig-data :signo))
         (signo-pt (signal-pointer signo))
         (original-lisp-signal-handler (getf sig-data :original-handler)))
    (free-pointer-data watcher :preserve-pointer t)
    (uv:free-handle watcher)
    (cffi:foreign-funcall "signal" :int signo :pointer original-lisp-signal-handler :pointer)
    (free-pointer-data signo-pt :preserve-pointer t)))

(define-c-callback signal-cb :void ((watcher :pointer) (signo :int))
  "All signals come through here."
  (let* ((callbacks (get-callbacks watcher))
         (signal-cb (getf callbacks :signal-cb))
         (event-cb (getf callbacks :event-cb))
         ;; (sig-data (deref-data-from-pointer watcher))
         ;; (ev (getf sig-data :ev))
         )
    (catch-app-errors event-cb
      (funcall signal-cb signo))))

(define-c-callback lisp-signal-cb :void ((signo :int))
  "Generic callback for lisp signal handling."
  (declare (optimize (speed 3)))
  ;; grab the callback from set-lisp-signal-handler
  (let* ((signo-pt (signal-pointer signo))
         (callback (car (get-callbacks signo-pt))))
    (when (functionp callback)
      ;; trigger the callback to run async
      (as:delay callback))))
     
(defun set-lisp-signal-handler (signo fn)
  "Replace the current handler for the signal number under signo, and return a
   pointer to the handler that is being replaced."
  (save-callbacks (signal-pointer signo) (list fn))
  (cffi:foreign-funcall "signal" :int signo :pointer (cffi:callback lisp-signal-cb) :pointer))

(defun signal-handler (signo signal-cb &key event-cb)
  "Setup a one-time signal handler for the given signo. This also sets up a
   lisp signal handler, so if a signal comes through while lisp is running
   instead of the event loop, it will run the same callback. All signal handlers
   are restored on event loop exit."
  (check-event-loop-running)
  ;; un-bind this signal handler if it is already bound. this ensures we don't
  ;; lose the original lisp signal handler when we overwrite it.
  (free-signal-handler signo)
  (let* ((watcher (uv:alloc-handle :signal)) 
         (lisp-signal-handler (set-lisp-signal-handler signo (lambda () (signal-cb watcher signo))))
         (signo-pt (signal-pointer signo)))
    (let ((res (uv:uv-signal-init (event-base-c *event-base*) watcher)))
      (unless (zerop res)
        (uv:free-handle watcher)
        (event-handler res event-cb :throw t)
        (return-from signal-handler)))
    (let ((res (uv:uv-signal-start watcher (cffi:callback signal-cb) signo)))
      (unless (zerop res)
        (uv:free-handle watcher)
        (event-handler res event-cb :throw t)
        (return-from signal-handler)))
    (save-callbacks watcher (list :signal-cb signal-cb
                                  :event-cb event-cb))
    (attach-data-to-pointer watcher (list :signo signo :original-handler lisp-signal-handler))
    ;; make sure we can find the event/original handler from just the signo
    (attach-data-to-pointer signo-pt watcher)
    ;; add this signal to the list of active signals
    (push signo (event-base-signal-handlers *event-base*))
    (add-event-loop-exit-callback (lambda () (free-signal-handler signo)))))

