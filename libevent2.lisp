(defpackage #:libevent2
  (:use :cl :cffi)
  (:nicknames :le))

(defpackage #:libevent2.accessors
  (:use :cl :cffi :libevent2)
  (:nicknames :le-a))

(in-package :libevent2)

(eval-when (:load-toplevel)
  (define-foreign-library libevent2
    (:unix (:or "libevent"))
    (t (:default "libevent")))
  (unless (foreign-library-loaded-p 'libevent2)
    (use-foreign-library libevent2)))

