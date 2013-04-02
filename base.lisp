(defpackage :cl-async-base
  (:use :cl)
  (:export #:event-info
           #:event-error
           #:event-errcode
           #:event-errmsg

           #:*event-base*
           #:*event-base-next-id*
           #:event-base
           #:event-base-c
           #:event-base-id
           #:event-base-function-registry
           #:event-base-data-registry
           #:event-base-exit-functions
           #:event-base-signal-handlers
           #:event-base-timeval-cache
           #:event-base-dns-base
           #:event-base-dns-ref-count
           #:event-base-catch-app-errors
           #:event-base-default-event-handler
           #:event-base-num-connections-in
           #:event-base-num-connections-out

           #:*buffer-size*
           #:*socket-buffer-c*
           #:*socket-buffer-lisp*))

(in-package :cl-async-base)

(define-condition event-info () ()
  (:documentation "Describes the base event for any action in cl-async.")
  (:report (lambda (c s) (format s "Info event: ~a" c))))

(define-condition event-error (event-info)
  ((code :initarg :code :reader event-errcode :initform 0)
   (msg :initarg :msg :reader event-errmsg :initform nil))
  (:report (lambda (c s) (format s "Error event: ~a: ~a" (event-errcode c) (event-errmsg c))))
  (:documentation "Describes a general error event."))

(defvar *event-base* nil
  "THE event base object used to process all async operations.")
(defvar *event-base-next-id* 0
  "The numeric identifier assigned to each new event base.")

(defclass event-base ()
  ((c :accessor event-base-c :initarg :c :initform nil :type (or null cffi:foreign-pointer)
     :documentation "Holds the C object pointing to the libevent event base.")
   (id :accessor event-base-id :initarg :id :initform -1 :type integer
     :documentation "Holds this event loop's numeric id.")
   (function-registry :accessor event-base-function-registry :initarg :function-registry :initform (make-hash-table :test #'eq) :type hash-table
     :documentation "Holds all callbacks attached to this event loop.")
   (data-registry :accessor event-base-data-registry :initarg :data-registry :initform (make-hash-table :test #'eq) :type hash-table
     :documentation "Holds all callback data attached to this event loop.")
   (exit-functions :accessor event-base-exit-functions :initarg :exit-functions :initform nil :type list
     :documentation "Holds functions to be run when the event loop exist (cleanly or otherwise).")
   (signal-handlers :accessor event-base-signal-handlers :initarg :signal-handlers :initform nil :type list
     :documentation "Holds all signal handlers.")
   (timeval-cache :accessor event-base-timeval-cache :initarg :timeval-cache :initform nil :type list
     :documentation "Holds a list of cached C timeval structs, freed on event loop exit.")
   (dns-base :accessor event-base-dns-base :initarg :dns-base :initform nil :type (or null cffi:foreign-pointer)
     :documentation "Holds the DNS base object used for DNS lookups.")
   (dns-ref-count :accessor event-base-dns-ref-count :initarg :dns-ref-count :initform 0 :type fixnum
     :documentation "Tracks how many open requests are pending on the dns base.")
   ;; error handling
   (catch-app-errors :accessor event-base-catch-app-errors :initarg :catch-app-errors :initform nil :type boolean
     :documentation "If true, attemps to trap all errors produced in the event loop and process them internally")
   (default-event-handler :accessor event-base-default-event-handler
                          :initarg :default-event-handler
                          :type function
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
   ;; stats
   (num-connections-in :accessor event-base-num-connections-in :initform 0 :type fixnum)
   (num-connections-out :accessor event-base-num-connections-out :initform 0 :type fixnum))
  (:documentation
    "A class that holds an event loop and all of the state that it manages.
     
     One object to rule them all, one object to find them.
     One object to bring them in and in the darkness bind them."))
  
(defparameter *buffer-size* 16384
  "The amount of data we'll pull from the evbuffers when doing reading/writing.")
(defvar *socket-buffer-c* nil
  "A pointer to the buffer in C land that reads from sockets.")
(defvar *socket-buffer-lisp* nil
  "An array in lisp land that holds data copied from a socket.")

