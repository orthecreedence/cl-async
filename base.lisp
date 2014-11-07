(defpackage :cl-async-base
  (:use :cl)
  (:export #:event-info
           #:event-error
           #:event-errcode
           #:event-errmsg

           #:*event-base*
           #:*event-base-next-id*
           #:*enable-threading*
           #:event-base
           #:event-base-c
           #:event-base-id
           #:event-base-function-registry
           #:event-base-data-registry
           #:event-base-exit-functions
           #:event-base-signal-handlers
           #:event-base-dns-base
           #:event-base-dns-ref-count
           #:event-base-catch-app-errors
           #:event-base-default-event-handler
           #:event-base-lock
           #:event-base-num-connections-in
           #:event-base-num-connections-out

           #:*buffer-size*
           #:*output-buffer*
           #:*input-buffer*
           #:*data-registry*
           #:*function-registry*))

(in-package :cl-async-base)

(define-condition event-info () ()
  (:report (lambda (c s)
             (print-unreadable-object (c s :type t :identity t)
               (format s ""))))
  (:documentation "Describes the base event for any action in cl-async."))

(define-condition event-error (event-info error)
  ((code :initarg :code :reader event-errcode :initform 0)
   (msg :initarg :msg :reader event-errmsg :initform nil))
  (:report (lambda (c s)
             (print-unreadable-object (c s :type t :identity t)
               (format s "~a: ~a" (event-errcode c) (event-errmsg c)))))
  (:documentation "Describes a general error event."))

(defvar *event-base* nil
  "THE event base object used to process all async operations.")
(defvar *event-base-next-id* 0
  "The numeric identifier assigned to each new event base.")
(defclass event-base ()
  ((c :accessor event-base-c :initarg :c :initform nil
     :documentation "Holds the C object pointing to the libevent event base.")
   (id :accessor event-base-id :initarg :id :initform nil
     :documentation "Holds this event loop's numeric id.")
   (function-registry :accessor event-base-function-registry :initarg :function-registry :initform (make-hash-table :test #'eql)
     :documentation "Holds all callbacks attached to this event loop.")
   (data-registry :accessor event-base-data-registry :initarg :data-registry :initform (make-hash-table :test #'eql)
     :documentation "Holds all callback data attached to this event loop.")
   (exit-functions :accessor event-base-exit-functions :initarg :exit-functions :initform nil
     :documentation "Holds functions to be run when the event loop exist (cleanly or otherwise).")
   (signal-handlers :accessor event-base-signal-handlers :initarg :signal-handlers :initform nil
     :documentation "Holds all signal handlers.")
   (dns-base :accessor event-base-dns-base :initarg :dns-base :initform nil
     :documentation "Holds the DNS base object used for DNS lookups.")
   (dns-ref-count :accessor event-base-dns-ref-count :initarg :dns-ref-count :initform 0
     :documentation "Tracks how many open requests are pending on the dns base.")
   ;; error handling
   (catch-app-errors :accessor event-base-catch-app-errors :initarg :catch-app-errors :initform nil
     :documentation "If true, attemps to trap all errors produced in the event loop and process them internally")
   (default-event-handler :accessor event-base-default-event-handler
                          :initarg :default-event-handler
                          :initform (lambda (err)
                                      ;; throw the error so we can wrap it in a handler-case
                                      (handler-case (error err)
                                        ;; got a connection error, throw it (must
                                        ;; do this explicitely since event-error
                                        ;; extends event-info)
                                        (event-error () (error err))

                                        ;; this is just info, let it slide
                                        (event-info () nil)))
     :documentation "Used as the default event handler if one is not specified.")
   (lock :accessor event-base-lock :initarg :lock :initform (bt:make-lock)
     :documentation "Holds *the* lock for this event base.")
   ;; stats
   (num-connections-in :accessor event-base-num-connections-in :initform 0)
   (num-connections-out :accessor event-base-num-connections-out :initform 0))
  (:documentation
    "A class that holds an event loop and all of the state that it manages.
     
     One object to rule them all, one object to find them.
     One object to bring them in and in the darkness bind them."))
  
(defparameter *buffer-size* 65535
  "The amount of data we'll pull from the evbuffers when doing reading/writing.")
(defvar *output-buffer* nil
  "A buffer that lives in both C and lisp worlds (static-vector) that lets us
   write to sockets.")
(defvar *input-buffer* nil
  "A buffer that lives in both C and lisp worlds (static-vector) that lets us
   read from sockets.")

(defvar *data-registry* nil
  "A hash table holding C pointer -> lisp data mappings.")
(defvar *function-registry* nil
  "A hash table holding C pointer -> lisp function(s) mappings.")

;; threading/locking state. i had an internal debate whether or note to include
;; these inside the event-base class itself, but i'd honestly rather not muddy
;; it up with threading stuff.
(defvar *enable-threading* nil
  "If true, various pieces of the cl-async internals will lock their restecpive
   structures before operating to ensure thread safety.")

