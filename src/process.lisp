(in-package :cl-async)

;; TBD: separate tcp-stream from async-stream.
;; Also, need to rename (?) socket in tcp.lisp
;; TBD: support inheritance of custom fds
;; TBD: utf-8 pipe support
;; TBD: check process handles during walk using generic function
;; TBD: custom env
;; TBD: custom process name
;; TBD: custom cwd
;; TBD: custom string encoding
;; TBD: process flags (detached etc.)

;; TBD: use common superclass for libuv handle wrappers

(defclass process ()
  ((c :accessor process-c :initarg :c)
   (input :accessor process-input :initarg :input)
   (output :accessor process-output :initarg :output)
   (error-output :accessor process-error-output :initarg :error-output)))

(defmethod initialize-instance :after ((process process) &key exit-cb event-cb &allow-other-keys)
  (attach-data-to-pointer (process-c process) process)
  (save-callbacks (process-c process)
                  (list :exit-cb exit-cb :event-cb event-cb)))

(define-c-callback process-close-cb :void ((process-handle :pointer))
  "Called when a process closes."
  ;; FIXME: same as streamish-close-cb
  (free-pointer-data process-handle :preserve-pointer t)
  (uv:free-handle process-handle))

(defun process-close (process-handle)
  (uv:uv-close process-handle (cffi:callback process-close-cb)))

(define-c-callback process-exit-cb :void ((process-handle :pointer)
                                          (exit-status :int64)
                                          (term-signal :int))
  (let* ((process (deref-data-from-pointer process-handle))
         (callbacks (get-callbacks process-handle))
         (event-cb (getf callbacks :event-cb))
         (exit-cb (getf callbacks :exit-cb)))
    (catch-app-errors event-cb
      (when exit-cb
        (funcall exit-cb process exit-status term-signal))
      (process-close process-handle)
      (setf (process-c process) nil))))

(defun init-stdio-container (container out-p type fd pipe-args)
  (setf (uv-a:uv-stdio-container-t-flags container)
        (flet ((v (name)
                 (cffi:foreign-enum-value 'uv:uv-stdio-flags name)))
          (ecase type
            (:ignore (v :ignore))
            (:inherit (v :inherit-fd))
            ((:pipe :stream)
             (logior (v :create-pipe)
                     (if out-p
                         (v :writable-pipe)
                         (v :readable-pipe)))))))
  (case type
    (:inherit
     (setf (cffi:foreign-slot-value
            (cffi:foreign-slot-pointer container '(:struct uv:uv-stdio-container-t) 'uv::data)
            '(:union uv:uv-stdio-container-s-data)
            'uv::fd)
           fd)
     nil)
    ((:stream :pipe)
     (let ((pipe-or-stream (apply #'init-client-socket 'pipe
                                  :stream (eq :stream type)
                                  :allow-other-keys t pipe-args)))
       (setf (uv-a:uv-stdio-container-t-data container)
             (streamish-c (streamish pipe-or-stream)))
       pipe-or-stream))))

(defun spawn (path args &key exit-cb
                          (event-cb #'error)
                          (input :ignore)
                          (output :ignore)
                          (error-output :ignore))
  "Run the program specified by PATH with specified ARGS.
  ARGS don't include the executable path (argv[0]).  Return process
  object and pipes or streams for input, output and error output file
  descriptors of the child process (NIL for file descriptors that
  aren't redirected via :PIPE or :STREAM, see below).

  EXIT-CB specifies the callback that should be called when the
  program terminates. It should be a function taking three arguments:
  process object, exit status and signal number that caused program
  termination (0 if the program wasn't terminated by signal).

  EVENT-CB specifies error handler to be used.

  INPUT, OUTPUT and ERROR-OUTPUT specify process input/output/error
  output redirection. For each of these, the following values are
  supported:

  :IGNORE the corresponding file descriptor isn't used
  :INHERIT inherit file descriptor from this process
  (:PIPE [:READ-CB ...] ...) use pipe-based redirection of the
    corresponding file descriptor (see PIPE-CONNECT for the set
    of supported keyword arguments).
  (:STREAM [:READ-CB ...] ...) same as PIPE, but uses async
    stream instead of a pipe."
  (check-event-loop-running)
  (let ((handle (uv:alloc-handle :process)))
    (cffi:with-foreign-objects ((stdio '(:struct uv:uv-stdio-container-t) 3)
                                (c-args :pointer (+ 2 (length args))))
      (cffi:with-foreign-strings (#++ (cwd "/tmp")
                                  (file (namestring path)))
        (let ((stdios
                (loop for fd from 0 below 3
                      for (type . other-args) in (mapcar #'alexandria:ensure-list
                                                         (list input output error-output))
                      for out-p = nil then t
                      collect
                      (init-stdio-container
                       (cffi:mem-aptr stdio '(:struct uv:uv-stdio-container-t) fd)
                       out-p type fd other-args))))
          (setf (cffi:mem-aref c-args :pointer) file
                (cffi:mem-aref c-args :pointer (1+ (length args))) (cffi:null-pointer))
          (loop for i from 1
                for arg in args
                do (setf (cffi:mem-aref c-args :pointer i)
                         (cffi:foreign-string-alloc arg)))
          (with-foreign-object* (options uv:uv-process-options-t)
                                ((uv-a:uv-process-options-t-exit-cb (cffi:callback process-exit-cb))
                                 (uv-a:uv-process-options-t-file file)
                                 (uv-a:uv-process-options-t-args c-args)
                                 #++
                                 (uv-a:uv-process-options-t-env (cffi:null-pointer))
                                 #++
                                 (uv-a:uv-process-options-t-cwd (cffi:null-pointer))
                                 #++
                                 (uv-a:uv-process-options-t-flags 0)
                                 (uv-a:uv-process-options-t-stdio-count 3)
                                 (uv-a:uv-process-options-t-stdio stdio)
                                 #++
                                 (uv-a:uv-process-options-t-uid 0)
                                 #++
                                 (uv-a:uv-process-options-t-gid 0))
            (let ((res (uv:uv-spawn (event-base-c *event-base*) handle options)))
              (cond ((zerop res)
                     (loop for i from 1 upto (length args)
                           do (cffi:foreign-string-free (cffi:mem-aref c-args :pointer i)))
                     (loop for pipe-or-stream in stdios
                           for in-p = t then nil
                           when pipe-or-stream
                             do (let ((pipe (streamish pipe-or-stream)))
                                  (when (or in-p (streamish-read-start
                                                  (streamish pipe)))
                                    (setf (socket-connected (streamish pipe)) t)
                                (write-pending-socket-data pipe))))
                     (apply #'values
                            (make-instance 'process
                                           :c handle
                                           :exit-cb exit-cb
                                           :event-cb event-cb
                                           :input (first stdios)
                                           :output (second stdios)
                                           :error-output (third stdios))
                            stdios))
                    (t
                     ;; destroying the handle immediately causes assertion failure
                     ;; (FIXME: why? seems like it shouldn't be so, looking
                     ;; at libuv tests)
                     (as:with-delay ()
                       (process-close handle))
                     (event-handler res event-cb :throw t))))))))))

(defmethod handle-cleanup ((handle-type (eql :process)) handle)
  (let ((process (deref-data-from-pointer handle)))
    (process-close handle)
    (when process
      (setf (process-c process) nil))))

(defun process-kill (process signal &key (event-cb #'error))
  "If PROCESS is active, send the specified signal (an integer) to it and return true.
   If PROCESS is not active or an error occurs (and EVENT-CB doesn't
   signal an error), return false.  If EVENT-CB is specified, use it
   to handle errors, otherwise signal them via ERROR."
  (alexandria:when-let ((handle (process-c process)))
    (let ((res (uv:uv-process-kill handle signal)))
      (cond ((zerop res) t)
            (t
             (event-handler res event-cb :throw t)
             nil)))))
