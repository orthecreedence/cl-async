(defpackage :cl-async-test
  (:use :cl :eos :cl-async-util :cl-async-future)
  (:export #:run-tests))
(in-package :cl-async-test)

;; TODO: test all functions in util package

(defmacro async-let ((&rest bindings) &body body)
  "Wrap an async op inside of a let/start-event-loop block to mimick a blocking
   action. Bindings must be set from withing the block via setf."
  `(let ,bindings
     (as:start-event-loop
       (lambda ()
         ,@body)
       :catch-app-errors t)
     (values ,@(loop for (binding . nil) in bindings collect binding))))

(defun test-timeout (seconds)
  "Make sure a test times out after the given num seconds. An event loop can't
   do this itself without skewing results. Uses event base IDs to make sure it
   doesn't cancel an event loop that test-timeout wasn't called inside of."
  (let ((event-base cl-async-util::*event-base*)
        (base-id cl-async-util::*event-base-id*))
    (let ((cancel nil))
      ;; if the event loop exits naturally, cancel the break
      (as:add-event-loop-exit-callback
        (lambda () (setf cancel t)))
      ;; spawn the thread to kill the event loop
      (handler-case
        (bt:make-thread (lambda ()
                         (sleep seconds)
                         (when (and (eql cl-async-util::*event-base-id* base-id)
                                    (not cancel))
                           (le:event-base-loopexit event-base (cffi:null-pointer)))))
        (bt::bordeaux-mp-condition ()
          nil)))))

(defun concat (&rest args)
  "Shortens string concatenation because I'm lazy and really who the hell wants
   to type out (concatenate 'string ...) seriously, I mispell concatentate like
   90% of the time I type it out."
  (apply #'concatenate (append '(string) args)))

(defun id (val)
  "Returns val. Yes, yes. I know that the identity function exists, but
   seriously I'm not going to waste precious time out of my life typing identity
   when I can just type id. The idea is the same, and everybody KNOWS what I'm
   trying to express.
   
   Oh one more thing: the only reason I NEED identi...err, id is because Eos
   can't use its `is` macro around another macro. So I need a function to wrap
   it. Lame. BUT such is life."
  val)
  
;; define the test suite
(def-suite cl-async-test :description "cl-async test suite")
(def-suite cl-async-test-core :in cl-async-test
                              :description "cl-async test suite")
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

(test append-array
  "Test appending of arrays"
  (let ((arr1 "well i hope you leave enough room for my fist, because i'm going to RAM IT INTO YOUR STOMACH!!! ")
        (arr2 "i will, bye."))
    (let ((str (babel:octets-to-string
                 (append-array (babel:string-to-octets arr1)
                               (babel:string-to-octets arr2)))))
      (is (string= str "well i hope you leave enough room for my fist, because i'm going to RAM IT INTO YOUR STOMACH!!! i will, bye.")))))

(test usec-time
  "Test correct splitting of a decimal second value in seconds and useconds"
  (let ((seconds1 6.3)
        (seconds2 10)
        (seconds3 .0002))
    (let ((res1 (multiple-value-list (split-usec-time seconds1)))
          (res2 (multiple-value-list (split-usec-time seconds2)))
          (res3 (multiple-value-list (split-usec-time seconds3))))
      (is (= (car res1) 6))
      (is (= (cadr res1) 300000))
      (is (= (car res2) 10))
      (is (= (cadr res2) 0))
      (is (= (car res3) 0))
      (is (= (cadr res3) 200)))))

(test pointer-callbacks
  "Test that pointer callbacks are assigned and also cleared correctly"
  (let ((fn (lambda (x) (1+ x)))
        (fn-size (if *fn-registry* (hash-table-count *fn-registry*) 0))
        (data-pointer (create-data-pointer)))
    (save-callbacks data-pointer (list :test fn))
    (is (= (funcall (getf (get-callbacks data-pointer) :test) 6) 7))
    (free-pointer-data data-pointer)
    (is (null (get-callbacks data-pointer)))
    (is (= fn-size (hash-table-count *fn-registry*)))))

(test pointer-data
  "Test that pointer data is assigned and also cleared correctly"
  (let ((my-data (make-hash-table :size 5))
        (data-size (if *data-registry* (hash-table-count *data-registry*) 0))
        (data-pointer (create-data-pointer)))
    (attach-data-to-pointer data-pointer my-data)
    (is (equal my-data (deref-data-from-pointer data-pointer)))
    (free-pointer-data data-pointer)
    (is (null (deref-data-from-pointer data-pointer)))
    (is (= data-size (hash-table-count *data-registry*)))))

