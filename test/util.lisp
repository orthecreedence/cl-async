(defpackage :cl-async-test
  (:use :cl :fiveam :cl-async-base :cl-async-util)
  (:export #:run-tests
           #:benchmark-server
           #:benchmark-client))
(in-package :cl-async-test)

;; Exclude 5am check failures from normal cl-async error handling.
;; This makes it possible to use (is ...) and related operations
;; in cl-async callbacks inside tests.
(pushnew '5am::check-failure cl-async-util:*passthrough-errors*)

;; TODO: test all functions in util package

(defmacro async-let ((&rest bindings) &body body)
  "Wrap an async op inside of a let/start-event-loop block to mimick a blocking
   action. Bindings must be set from withing the block via setf."
  `(let ,bindings
     (as:with-event-loop (:catch-app-errors nil)
       ,@body)
     (values ,@(loop for (binding . nil) in bindings collect binding))))

(defun test-timeout (seconds)
  "Make sure a test times out after the given num seconds. An event loop can't
   do this itself without skewing results. Uses event base IDs to make sure it
   doesn't cancel an event loop that test-timeout wasn't called inside of."
  (let ((cancel nil)
        (notifier (as:make-notifier (lambda () (error "test timeout")))))
    (as:unref notifier)
    ;; if the event loop exits naturally, cancel the break
    (as:add-event-loop-exit-callback
      (lambda ()
        (setf cancel t)
        (unless (as:notifier-freed-p notifier)
          (as:free-notifier notifier))))
    ;; spawn the thread to kill the event loop
    (handler-case
      (bt:make-thread (lambda ()
                        (sleep seconds)
                        (when (not cancel)
                          (as:trigger-notifier notifier))))
      (bt::bordeaux-mp-condition ()
        nil))))

(defun concat (&rest args)
  "Shortens string concatenation because I'm lazy and really who the hell wants
   to type out (concatenate 'string ...) seriously, I mispell concatentate like
   90% of the time I type it out."
  (apply #'concatenate 'string args))

(defun id (val)
  "Returns val. Yes, yes. I know that the identity function exists, but
   seriously I'm not going to waste precious time out of my life typing identity
   when I can just type id. The idea is the same, and everybody KNOWS what I'm
   trying to express.

   Oh one more thing: the only reason I NEED identi...err, id is because Eos
   can't use its `is` macro around another macro. So I need a function to wrap
   it. Lame. BUT such is life."
  val)

(defun call-with-temporary-directory (thunk)
  (as:mkdtemp (uiop:merge-pathnames*
               "as-tst-XXXXXX"
               (uiop:temporary-directory))
              #'(lambda (dir)
                  (as:add-event-loop-exit-callback
                   #'(lambda ()
                       (uiop:delete-directory-tree
                             dir
                             :validate #'(lambda (path)
                                           (uiop:subpathp path (uiop:temporary-directory))))))
                  (funcall thunk dir))))

(defmacro with-temporary-directory ((dir) &body body)
  `(call-with-temporary-directory #'(lambda (,dir) ,@body)))

(defmacro with-path-under-tmpdir ((path-var subpath) &body body)
  (alexandria:with-gensyms (dir)
    `(with-temporary-directory (,dir)
       (let ((,path-var (uiop:merge-pathnames* ,subpath ,dir)))
         ,@body))))

(defvar *later-list*)

(defun later (function)
  "Execute the specified FUNCTION after event loop of WITH-TEST-EVENT-LOOP
   terminates."
  ;; using as:add-event-loop-exit-callback depends on event loop
  ;; actually calling it, which may not be a good assumption
  ;; for tests
  (push function *later-list*))

(defmacro with-test-event-loop ((&key (catch-app-errors nil)) &body body)
  "Run BODY within event loop, executing functions specified via LATER
   after it terminates. Can be used with CALLED-ONCE and NEVER."
  (alexandria:with-gensyms (fn)
    `(let ((*later-list* '()))
       (as:with-event-loop (:catch-app-errors ,catch-app-errors)
         ,@body)
       (dolist (,fn (nreverse *later-list*))
         (funcall ,fn)))))

(defun called-once (function)
  "Wrap FUNCTION to make sure that it's called exactly once during
   execution of WITH-TEST-EVENT-LOOP body."
  (let ((count 0))
    (later #'(lambda ()
               (is (= 1 count)
                   "callback is expected to be called once, ~
                        but it was called ~d time~:p" count)))
    #'(lambda (&rest args)
        (incf count)
        (apply function args))))

(defun never (&rest args)
  "NEVER function is for use as a callback that should be never
   called. Use in the dynamic context of WITH-TEST-EVENT-LOOP."
  (declare (ignore args))
  (fail "'never' function called (unexpected callbacl)"))

(defparameter *wait-interval* 0.05)

(defun %wait (pred)
  (let ((wait-successful-p nil))
    (labels ((wait-for-it ()
               (if (funcall pred)
                   (setf wait-successful-p t)
                   (as:delay #'wait-for-it :time *wait-interval*))))
      (wait-for-it)
      (unless wait-successful-p
        (later #'(lambda ()
                   (is-true wait-successful-p "WAIT failed")))))))

(defmacro wait (expr)
  `(%wait #'(lambda () ,expr)))

;; define the test suite
(def-suite cl-async-test :description "cl-async test suite")
(def-suite cl-async-test-core :in cl-async-test :description "cl-async test suite")
(in-suite cl-async-test-core)

(test data-pointers
  "Make sure data pointers are #'eql"
  (let* ((raw-pointer (cl-async-util:create-data-pointer))
         (pointer1 (make-pointer-eql-able raw-pointer))
         (pointer2 (make-pointer-eql-able raw-pointer)))
    (is (eql pointer1 pointer2))))

(test ipv4-address
  "Test IPV4 address formatting"
  (is (ipv4-address-p "127.0.0.1"))
  (is (ipv4-address-p "192.168.0.1"))
  (is (ipv4-address-p "74.125.224.104"))
  (is (ipv4-address-p "255.255.255.0"))
  (is (not (ipv4-address-p "hai")))
  (is (not (ipv4-address-p "i'm larry")))
  (is (not (ipv4-address-p "127.0.4.")))
  (is (not (ipv4-address-p "80.80.80.257")))
  (is (not (ipv4-address-p "")))
  (is (not (ipv4-address-p "1.2.3.4.5"))))

(test ipv6-address
  "Test IPV6 address formatting"
  (is (ipv6-address-p "fe80:0000:0000:0000:0204:61ff:fe9d:f156"))
  (is (ipv6-address-p "fe80:0:0:0:204:61ff:fe9d:f156"))
  (is (ipv6-address-p "fe80::204:61ff:fe9d:f156"))
  ;; no ipv4 dotted at end for now.
  ;(is (ipv6-address-p "fe80:0000:0000:0000:0204:61ff:254.157.241.86"))
  ;(is (ipv6-address-p "fe80:0:0:0:0204:61ff:254.157.241.86"))
  ;(is (ipv6-address-p "fe80::204:61ff:254.157.241.86"))
  (is (ipv6-address-p "::1"))
  (is (ipv6-address-p "fe80::"))
  (is (ipv6-address-p "2001::"))
  (is (not (ipv6-address-p "hai::")))
  (is (not (ipv6-address-p "omg::lol:wtf::")))
  (is (not (ipv6-address-p "sexrobot.sexrobot")))
  (is (not (ipv6-address-p "ge80::")))
  (is (not (ipv6-address-p "fe80:0000:0000:0000:02046:61ff:fe9d:f156")))
  (is (not (ipv6-address-p "f:80:0000:0000:0000:0204:61ff:fe9d:f156"))))

(test pointer-callbacks
  "Test that pointer callbacks are assigned and also cleared correctly"
  (let* ((*event-base* (make-instance 'event-base))
         (*data-registry* (event-base-data-registry *event-base*))
         (*function-registry* (event-base-function-registry *event-base*))
         (fn (lambda (x) (1+ x)))
         (fn-size (if (event-base-function-registry *event-base*)
                      (hash-table-count (event-base-function-registry *event-base*))
                      0))
         (data-pointer (create-data-pointer)))
    (save-callbacks data-pointer (list :test fn))
    (is (= (funcall (getf (get-callbacks data-pointer) :test) 6) 7))
    (free-pointer-data data-pointer)
    (is (null (get-callbacks data-pointer)))
    (is (= fn-size (hash-table-count (event-base-function-registry *event-base*))))))

(test pointer-data
  "Test that pointer data is assigned and also cleared correctly"
  (let* ((*event-base* (make-instance 'event-base))
         (*data-registry* (event-base-data-registry *event-base*))
         (*function-registry* (event-base-function-registry *event-base*))
         (my-data (make-hash-table :size 5))
         (data-size (if (event-base-data-registry *event-base*)
                        (hash-table-count (event-base-data-registry *event-base*))
                        0))
         (data-pointer (create-data-pointer)))
    (attach-data-to-pointer data-pointer my-data)
    (is (equal my-data (deref-data-from-pointer data-pointer)))
    (free-pointer-data data-pointer)
    (is (null (deref-data-from-pointer data-pointer)))
    (is (= data-size (hash-table-count (event-base-data-registry *event-base*))))))
