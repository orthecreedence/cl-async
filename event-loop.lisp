(in-package :cl-async)

(cffi:defcallback fatal-cb :void ((err :int))
  "Used to handle fatal libevent errors."
  (let ((fatal-cb (getf (get-callbacks (event-base-c *event-base*)) :fatal-cb)))
    (when fatal-cb
      (funcall fatal-cb err))))

(cffi:defcallback logger-cb :void ((severity :int) (msg :string))
  "Used to catch log messages (if setup in start-event-loop)."
  (let ((logger-cb (getf (get-callbacks (event-base-c *event-base*)) :logger-cb)))
    (when logger-cb
      (funcall logger-cb severity msg))))

(cffi:defcallback event-debug-cb :void ((severity :int) (msg :string))
  (let ((sev-str (cond
                   ((= severity le:+event-log-debug+) "debug")
                   ((= severity le:+event-log-msg+) "msg")
                   ((= severity le:+event-log-warn+) "warn")
                   ((= severity le:+event-log-err+) "err"))))
    (format t "EVDBG(~a): ~a~%" sev-str msg)
    (force-output)))

(defun enable-debug-mode ()
  "Enable debug mode. As far as I can tell, this is undoable, so you may have to
   restart your CL implementation after calling if you want to disable it.

   NOTE: This appears to make libevent exit suddenly after creating a second
   event loop for some reason. I recommend *not* using this function until I
   figure out debug omde a bit more."
  (if (event-base-c *event-base*)
      (error "Debug mode must be enabled *before* an event loop has started.")
      (progn
        (le:event-enable-debug-mode)
        (le:event-set-log-callback (cffi:callback event-debug-cb)))))

(defun enable-threading-support ()
  "Enable threading support in libevent. This attempts to guess which threading
   function of libevent's to use and call it.

   Experimental, only supports pthreads or Windows."
  (let ((use-pthreads (cffi:foreign-symbol-pointer "evthread_use_pthreads"))
        (use-win-threads (cffi:foreign-symbol-pointer "evthread_use_windows_threads")))
    (cond
      (use-pthreads
        (cffi:foreign-funcall-pointer use-pthreads () :void))
      (use-win-threads
        (cffi:foreign-funcall-pointer use-win-threads () :void)))))

(defun add-event-loop-exit-callback (fn)
  "Add a function to be run when the event loop exits."
  (push fn (event-base-exit-functions *event-base*)))

(defun process-event-loop-exit-callbacks ()
  "run and clear out all event loop exit functions."
  (dolist (fn (event-base-exit-functions *event-base*))
    (funcall fn))
  (setf (event-base-exit-functions *event-base*) nil))

(defun check-event-loop-running ()
  (unless (and *event-base* (event-base-c *event-base*))
    (error "Event loop not running. Start with function start-event-loop.")))

(defun stats ()
  "Return statistics about the current event loop."
  (list :open-dns-queries (event-base-dns-ref-count *event-base*)
        :fn-registry-count (if (hash-table-p (event-base-function-registry *event-base*))
                               (hash-table-count (event-base-function-registry *event-base*))
                               0)
        :data-registry-count (if (hash-table-p (event-base-data-registry *event-base*))
                                 (hash-table-count (event-base-data-registry *event-base*))
                                 0)
        :incoming-tcp-connections (event-base-num-connections-in *event-base*)
        :outgoing-tcp-connections (event-base-num-connections-out *event-base*)))

(defun dump-event-loop-status (file &key (return-as-string t))
  "Dump the status of the event loop to a file. Good for debugging.
   
   If return-as-string is T, the file is read/deleted and the contents returned
   as a string. Note that this is the default behavior."
  (check-event-loop-running)
  (let ((fp (cffi:foreign-funcall "fopen" :string (namestring file) :string "w+" :pointer)))
    (unwind-protect
        (le:event-base-dump-events (event-base-c *event-base*) fp)
      (cffi:foreign-funcall "fclose" :pointer fp)))
  (when (and return-as-string
             (probe-file file))
    (unwind-protect
        (with-open-file (s file)
          (let* ((len (file-length s))
                 (data (make-string len)))
            (values data (read-sequence data s))))
      (delete-file file))))
  
(defun start-event-loop (start-fn &key fatal-cb logger-cb default-event-cb (catch-app-errors nil catch-app-errors-supplied-p))
  "Simple wrapper function that starts an event loop which runs the given
   callback, most likely to init your server/client.

   Supports setting up a callback for fatal errors. In case you don't want
   libevent to just exit your app for you.

   Supports setting up a logging callback for your application.

   *PLEASE NOTE* Using libevent functions from within the logging callback can
   lead to strange bugs and problems. Don't do it."
  (when *event-base*
    (error "Event loop already started. Please wait for it to exit."))
  ;; note the binding of these variable via (let), which means they are thread-
  ;; local... so this function can be called in different threads, and the bound
  ;; variables won't interfere with each other.
  (let ((*event-base* (apply #'make-instance
                             (append
                               (list 'event-base
                                     :c (le:event-base-new)
                                     :id *event-base-next-id*)
                               (when catch-app-errors-supplied-p
                                 (list :catch-app-errors catch-app-errors))
                               (when (functionp default-event-cb)
                                 (list :default-event-handler default-event-cb)))))
        (*timeval-cache* nil)
        (*socket-buffer-c* (cffi:foreign-alloc :unsigned-char :count *buffer-size*))
        (*socket-buffer-lisp* (make-array *buffer-size* :element-type '(unsigned-byte 8)))
        (callbacks nil))
    (incf *event-base-next-id*)
    ;; set up a callback for dealing with fatal errors
    (when fatal-cb
      (setf callbacks (append callbacks (list :fatal-cb fatal-cb)))
      (le:event-set-fatal-callback (cffi:callback fatal-cb)))
    ;; set up the logging callback if we were passed one
    (when logger-cb
      (setf callbacks (append callbacks (list :logger-cb logger-cb)))
      (le:event-set-log-callback (cffi:callback logger-cb)))
    (delay start-fn)
    ;; this is the once instance where we assign callbacks to a libevent object
    ;; instead of a data-pointer since the callbacks don't take any void* args,
    ;; meaning we have to dereference from the global (event-base-c *event-base*) object.
    (save-callbacks (event-base-c *event-base*) callbacks)
    (unwind-protect
      (progn
        ;; this will block until all events are processed
        (le:event-base-dispatch (event-base-c *event-base*)))
      ;; cleanup
      (process-event-loop-exit-callbacks)
      (cffi:foreign-free *socket-buffer-c*)
      (free-pointer-data (event-base-c *event-base*) :preserve-pointer t)
      (le:event-base-free (event-base-c *event-base*))
      (free-foreign-cached-collection *timeval-cache*)
      (setf *event-base* nil))))

(defun exit-event-loop ()
  "Exit the event loop if running."
  (if (event-base-c *event-base*)
      (le:event-base-loopexit (event-base-c *event-base*) (cffi:null-pointer))
      nil))


