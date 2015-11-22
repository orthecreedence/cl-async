;;; This file contains a simple proxy. It binds to a local port and connects to
;;; a remote host/port, proxying all data received locally or remotely. It uses
;;; the "socket data" accessor to reference the local and remote socket to each
;;; other, allowing one to be closed if the other gets an EOF.
;;;
;;; It has the ability to print out all data sent/received, and print out
;;; reports on connection statistics.
;;;
;;; Usage:
;;;
;;;   (simple-proxy:start local-bind local-port
;;;                       remote-host remote-port
;;;                       &key stats debug ascii verbose)
;;;
;;; Not only does this offer a good example of a more advanced usage of cl-async
;;; but is actually really useful for peeking into plaintext TCP protocols. For
;;; instance, you could use it to debug HTTP requests or learn what a driver is
;;; sending to a server (which is what I built it for).

(ql:quickload :cl-async)

(defpackage :simple-proxy
  (:use :cl)
  (:export #:start))
(in-package :simple-proxy)

(defparameter *debug* nil "If T, will echo all data coming through the proxy")
(defparameter *verbose* nil "If T (and *debug* is T), will spit out every byte that passes through")
(defparameter *ascii* nil "If T, will echo all non-UTF8 data as a string of ASCII bytes instead of a vector")

(defun to-ascii (data)
  "Print data to an ASCII string byte by byte (if enabled)."
  (if *ascii*
      (progn
        (loop for byte-code across data do
          (format t "~c" (code-char byte-code)))
        (format t "~%"))
      data))

(defun output-data (location &optional data)
  "Outputs the given data using the specified location as the header."
  (when *debug*
    (if data
        (progn
          (format t "---~a(~a)---~%" location (length data))
          (when *verbose*
            (handler-case (format t "~a~%" (babel:octets-to-string data :encoding :utf-8))
              (error () (format t "~a~%" (to-ascii data))))))
        (format t "---~a---~%" location))))

(defun socketp (socket)
  "Test if given object is an as:socket."
  (subtypep (type-of socket) 'as:socket))

(defun pair-sockets (sock1 sock2)
  "Pair two sockets to each other."
  (when (and (socketp sock1)
             (socketp sock2))
    (setf (as:socket-data sock1) sock2
          (as:socket-data sock2) sock1)))

(defun close-paired-socket (socket)
  "Given a socket, close the paired socket (if it exists)."
  (when (socketp socket)
    (let ((paired-socket (as:socket-data socket)))
      (when (and (socketp paired-socket)
                 (not (as:socket-closed-p paired-socket)))
        (output-data "close")
        (as:close-socket paired-socket)
        ;; deref them
        (setf (as:socket-data socket) nil
              (as:socket-data paired-socket) nil)))))

(defun proxy-event-handler (ev)
  "Handle all proxy events."
  (handler-case
    (error ev)
    ;; if a socket times out, close the paired socket
    (as:tcp-timeout ()
      (close-paired-socket (as:tcp-socket ev)))
    ;; if we get a socket eof, close the paired socket, but delay it so that any
    ;; data being sent out before closing has a chance to "escape."
    (as:tcp-error ()
      (as:delay (lambda () (close-paired-socket (as:tcp-socket ev)))))
    (as:tcp-eof ()
      (as:delay (lambda () (close-paired-socket (as:tcp-socket ev)))))
    ;; just echo the event
    (error ()
      (when *debug*
        (format t "ev: ~a (~a)~%" (type-of ev) ev)))))

(defun proxy-remote-response (sock-remote data)
  "Send data received on the remote socket into the local socket."
  (output-data "remote" data)
  (let ((sock-local (as:socket-data sock-remote)))
    (if (as:socket-closed-p sock-local)
        (close-paired-socket sock-local)
        (as:write-socket-data sock-local data))))

(defun proxy-local-data (sock-local data)
  "Send data received on the local socket into the remote socket."
  (output-data "local" data)
  (let ((sock-remote (as:socket-data sock-local)))
    (if (as:socket-closed-p sock-remote)
        (close-paired-socket sock-remote)
        (as:write-socket-data sock-remote data))))

(defun start (local-bind local-port remote-host remote-port &key stats debug ascii verbose)
  "Start a proxy on a local port and proxy to a remote host. If :stats is T,
   connection stats are printed every 2 seconds. If :debug is T, all data
   passing through the proxy is echoed to STDOUT."
  (let ((server nil)
        (quit nil)
        (*debug* debug)
        (*verbose* verbose)
        (*ascii* ascii))
    (as:with-event-loop (:catch-app-errors t)
      (format t "Starting proxy.~%")
      (setf server (as:tcp-server
                     local-bind local-port
                     (lambda (sock-local data)
                       (proxy-local-data sock-local data))
                     :event-cb #'proxy-event-handler
                     :connect-cb (lambda (sock-local)
                                   (output-data "connection")
                                   ;; on local connect, establish the remote connection
                                   (let ((sock-remote (as:tcp-connect remote-host remote-port
                                                                      #'proxy-remote-response
                                                                      :event-cb #'proxy-event-handler)))
                                     ;; pair the local and remote sockets. if
                                     ;; one closes, so does the other.
                                     (pair-sockets sock-local sock-remote)))
                     :backlog -1))

      ;; SIGINT will *cleanly* close the proxy (doesn't accept any new
      ;; connections, but lets current ones run free until they close).
      (as:signal-handler as:+sigint+
        (lambda (sig)
          (declare (ignore sig))
          (format t "Closing proxy...~%")
          (setf quit t)
          (as:close-tcp-server server)
          (as:free-signal-handler as:+sigint+)))

      ;; if :stats is T, print connections statistics every few seconds
      (when stats
        (labels ((print-stats ()
                   (let* ((stats (as:stats))
                          (incoming (getf stats :incoming-tcp-connections))
                          (outgoing (getf stats :outgoing-tcp-connections)))
                     (format t "incoming: ~a~%outgoing: ~a~%~%" incoming outgoing))
                   (unless quit
                     (as:delay #'print-stats :time 2))))
          (print-stats)))))
  (format t "Closed.~%"))
