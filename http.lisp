(in-package :cl-async)

(define-condition http-info (event-info) ()
  (:documentation "Base HTTP condition. Holds the socket object."))

(define-condition http-error (event-error http-info) ()
  (:report (lambda (c s) (format s "HTTP connection error: ~a: ~a" (event-errcode c) (event-errmsg c))))
  (:documentation "Describes a general HTTP connection error."))

(define-condition http-timeout (http-error) ()
  (:report (lambda (c s) (format s "HTTP connection timeout: ~a: ~a" (event-errcode c) (event-errmsg c))))
  (:documentation "Passed to an event callback when an HTTP connection times out."))

(define-condition http-refused (http-error) ()
  (:report (lambda (c s) (format s "HTTP connection refused: ~a: ~a" (event-errcode c) (event-errmsg c))))
  (:documentation "Passed to an event callback when an HTTP connection is refused."))

(defclass http-request ()
  ((req-obj :accessor http-request-c :initarg :c :initform nil :documentation
     "Holds the libevent evhttp request foreign pointer. Can be used to get more
      information from the request if needed via the libevent evhttp_* functions
      if needed, but for the most part it is just used to send the response back
      to the client asynchronously after the app is done processing the request.")
   (server :accessor http-request-server :initarg :server :initform nil :documentation
     "Holds the http-server class that this request was accepted from. Private.")
   (method :accessor http-request-method :initarg :method :initform nil :documentation
     "Hold the HTTP request method (GET, POST, PUT, DELETE, etc etc etc)")
   (uri :accessor http-request-uri :initarg :uri :initform nil :documentation
     "Holds the entire URI from the client request (decoded)")
   (resource :accessor http-request-resouce :initarg :resource :initform nil :documentation
     "The (string) resource the request is accessing. So if the client asked for
          /documents/doc-18?pretty-print=1&format=json
      the resource would be
          /documents/doc-18
      Note the lack of querystring. The resource is derived from uri, so it is
      decoded.")
   (querystring :accessor http-request-querystring :initarg :querystring :initform nil :documentation
     "Holds the (string) entire querystring from the request, unparsed. It is
      derived from uri, so it's decoded.")
   (headers :accessor http-request-headers :initarg :headers :initform nil :documentation
     "An alist of headers:
          '((\"Content-Type\" . \"text/html\")
            (\"Accept\" . \"*/*\"))
      Headers are passed directly from libevent to the app without any extra
      processing")
   (body :accessor http-request-body :initarg :body :initform nil :documentation
     "The body grabbed from the request. This can be form data, a posted file,
      etc. It is not processed in any way and the application must be able to
      deal with the different request body types that come through."))
  (:documentation
    "When any request comes in to the HTTP listener, it is parsed into the slots
     of this class, which is then handed directly to the application's request
     callback."))

(defmethod print-object ((request http-request) s)
  (format s "HTTP request~%------------~%")
  (format s "  method: ~s~%" (http-request-method request))
  (format s "  uri: ~s~%" (http-request-uri request))
  (format s "  resource: ~s~%" (http-request-resouce request))
  (format s "  querystring: ~s~%" (http-request-querystring request))
  (format s "  headers:~%")
  (dolist (header (http-request-headers request))
    (format s "    ~a: ~a~%" (car header) (cdr header)))
  (format s "  body-length: ~s~%" (length (http-request-body request)))
  (format s "------------~%"))

(defclass http-server ()
  ((c :accessor http-server-c :initarg :c :initform (cffi:null-pointer))
   (socket :accessor http-server-sock :initarg :sock :initform (cffi:null-pointer))
   (closed :accessor http-server-closed :initarg :closed :initform nil))
  (:documentation "Wraps around a libevent HTTP listener."))

(defun free-http-server (http-server)
  "Frees an HTTP server (once)."
  (let ((evhttp (http-server-c http-server)))
    ;; only free if we haven't already freed, and there are no pending responses
    (unless (or (cffi:null-pointer-p evhttp)
                (< 0 *incoming-http-count*))
      (le:evhttp-free evhttp)
      (setf (http-server-c http-server) (cffi:null-pointer)))))

(defun close-http-server (http-server)
  "Closes an HTTP server. If already closed, does nothing."
  (unless (http-server-closed http-server)
    (let ((evhttp (http-server-c http-server))
          (socket (http-server-sock http-server)))
      (unless (or (cffi:null-pointer-p evhttp)
                  (cffi:null-pointer-p socket))
        (le:evhttp-del-accept-socket evhttp socket)))
    ;(le:evhttp-free (http-server-c http-server))
    (setf (http-server-closed http-server) t))
  ;; free it if we can.
  (free-http-server http-server))

(defun get-method (enum)
  "Given a libevent EVHTTP_REQ enum key word, return the appropriate method
   string."
  (case enum
    (:+evhttp-req-get+ :GET)
    (:+evhttp-req-post+ :POST)
    (:+evhttp-req-head+ :HEAD)
    (:+evhttp-req-put+ :PUT)
    (:+evhttp-req-delete+ :DELETE)
    (:+evhttp-req-options+ :OPTIONS)
    (:+evhttp-req-trace+ :TRACE)
    (:+evhttp-req-connect+ :CONNECT)
    (:+evhttp-req-patch+ :PATCH)))

(defun get-method-reverse (method)
  "Given a method string, return the appropriate enum value for the libevent
   type."
  (case (cond
          ((stringp method) (read-from-string method))
          ((symbolp method) method))
    ((GET :GET) :+evhttp-req-get+)
    ((POST :POST) :+evhttp-req-post+)
    ((HEAD :HEAD) :+evhttp-req-head+)
    ((PUT :PUT) :+evhttp-req-put+)
    ((DELETE :DELETE) :+evhttp-req-delete+)
    ((OPTIONS :OPTIONS) :+evhttp-req-options+)
    ((TRACE :TRACE) :+evhttp-req-trace+)
    ((CONNECT :CONNECT) :+evhttp-req-connect+)
    ((PATCH :PATCH) :+evhttp-req-patch+)
    (t :+evhttp-req-get+)))

(defun http-get-headers (http-headers)
  "Pull the headers out of an HTTP request as a plist."
  (let ((header-ptr (le-a:evkeyvalq-thq-first http-headers))
        (headers nil))
    (loop until (cffi:null-pointer-p header-ptr) do
      (push (cons (le-a:evkeyval-key header-ptr)
                  (le-a:evkeyval-value header-ptr)) headers)
      (setf header-ptr (le-a:evkeyval-next header-ptr)))
    (nreverse headers)))

(cffi:defcallback http-client-close-cb :void ((connection :pointer) (data-pointer :pointer))
  "Called when an HTTP client connection is closed."
  (declare (ignore connection))
  (decf *outgoing-http-count*)
  (free-pointer-data data-pointer))

(cffi:defcallback http-request-cb :void ((request :pointer) (data-pointer :pointer))
  "ALL HTTP requests come through here. They are processed into the http-request
   class and sent off to the appropriate callback."
  ;; TODO: process request body
  (let* ((http-server (deref-data-from-pointer data-pointer))
         (callbacks (get-callbacks data-pointer))
         (request-cb (getf callbacks :request-cb))
         (event-cb (getf callbacks :event-cb)))
    ;; NOTE: this is inaccurate. more than one request could come in on the same
    ;; connection, so it would be better to track the connection when it's
    ;; opened. not sure if libevent allows that on evhttp though, so for now
    ;; this is the next best thing.
    (incf *incoming-http-count*)
    (catch-app-errors event-cb
      (let* ((method (get-method (le:evhttp-request-get-command request)))
             (uri (le:evhttp-request-get-uri request))
             (uri (let ((str-pt (cffi:foreign-funcall "evhttp_uridecode" :string uri :int 1 :pointer (cffi:null-pointer) :pointer)))
                    (unwind-protect
                      (cffi:foreign-string-to-lisp str-pt)
                      (cffi:foreign-string-free str-pt))))
             ;(uri (le:evhttp-uridecode uri 1 (cffi:null-pointer)))
             (find-q (position #\? uri))
             (resource (if find-q (subseq uri 0 find-q)))
             (querystring (if find-q (subseq uri (1+ find-q)) ""))
             (body (drain-evbuffer (le:evhttp-request-get-input-buffer request)))
             (headers (http-get-headers (le:evhttp-request-get-input-headers request))))
        ;; grab the headers and build the final request object, then pass it into
        ;; the callback
        (let ((http-request (make-instance 'http-request
                                           :c request
                                           :server http-server
                                           :method method
                                           :uri uri
                                           :resource resource
                                           :querystring querystring
                                           :headers headers
                                           :body body)))
          (funcall request-cb http-request))))))

(cffi:defcallback http-client-cb :void ((request :pointer) (data-pointer :pointer))
  "HTTP client callback. All client HTTP requests come through here, get
   processed, and send off to either the event-cb (in case of connection error)
   or to the request-cb for everything else (even HTTP errors are sent through)."
  (let* ((pointer-data (deref-data-from-pointer data-pointer))
         (connection (getf pointer-data :connection))
         (dns-base (getf pointer-data :dns-base))
         (callbacks (get-callbacks connection))
         (request-cb (getf callbacks :request-cb))
         (event-cb (getf callbacks :event-cb)))
    (catch-app-errors event-cb
      (unwind-protect
        (cond
          ;; timeout
          ((cffi:null-pointer-p request)
           (run-event-cb event-cb (make-instance 'http-timeout :code -1 :msg "HTTP connection timed out.")))
          ;; connection refused
          ((eq (le:evhttp-request-get-response-code request) 0)
           (run-event-cb event-cb (make-instance 'http-refused :code -1 :msg "HTTP connection refused.")))
          ;; got response back, parse and send off to request-cb
          (t
           (let ((status (le:evhttp-request-get-response-code request))
                 (body (drain-evbuffer (le:evhttp-request-get-input-buffer request)))
                 (headers (http-get-headers (le:evhttp-request-get-input-headers request))))
             ;; free request if we own it
             (when (eq (le:evhttp-request-is-owned request) 1)
               (le:evhttp-request-free request))
             (funcall request-cb status headers body))))
        (unless (cffi:null-pointer-p dns-base)
          (release-dns-base))
        ;; free the connection if it exists
        (unless (cffi:null-pointer-p connection)
          (le:evhttp-connection-free connection))
        (free-pointer-data data-pointer)))))

(defun lookup-status-text (status-code)
  "Get the HTTP standard text that goes along with a status code."
  (case status-code
    (100 "Continue")
    (101 "Switching Protocols")
    (102 "Processing")

    (200 "OK")
    (201 "Created")
    (202 "Accepted")
    (203 "Non-Authoratative Information")
    (204 "No Content")
    (205 "Reset Content")
    (206 "Partial Content")
    (207 "Multi-Status")
    (208 "Already Reported")
    (226 "IM Used")

    (300 "Multiple Choices")
    (301 "Moved Permanently")
    (302 "Found")
    (303 "See Other")
    (304 "Not Modified")
    (305 "Use Proxy")
    (306 "Switch Proxy")
    (307 "Temporary Redirect")
    (308 "Permanent Redirect")

    (400 "Bad Request")
    (401 "Unauthorized")
    (402 "Payment Required")
    (403 "Forbidden")
    (404 "Not Found")
    (405 "Method Not Allowed")
    (406 "Not Acceptable")
    (407 "Proxy Authentication Required")
    (408 "Request Timeout")
    (409 "Conflict")
    (410 "Gone")
    (411 "Length Required")
    (412 "Precondition Failed")
    (413 "Request Entity Too Large")
    (414 "Request-URI Too Long")
    (415 "Unsupported Media Type")
    (416 "Requested Range Not Satisfiable")
    (417 "Expectation Failed")
    (418 "I'm a teapot")
    (420 "Enhance Your Calm")
    (422 "Unprocessable Entity")
    (423 "Locked")
    (424 "Failed Dependency")
    (425 "Unordered Collection")
    (426 "Upgrade Required")
    (428 "Precondition Required")
    (429 "Too Many Requests")
    (431 "Request Header Fields Too Large")
    (444 "No Response")
    (449 "Retry With")
    (450 "Blocked by Windows Parental Controls")
    (451 "Unavailable For Legal Reasons")
    (494 "Request Header Too Large")
    (495 "Cert Error")
    (496 "No Cert")
    (497 "HTTP to HTTPS")
    (499 "Client Closed Request")

    (500 "Internal Server Error")
    (501 "Not Implemented")
    (502 "Bad Gateway")
    (503 "Service Unavailable")
    (504 "Gateway Timeout")
    (505 "HTTP Version Not Supported")
    (506 "Variant Also Negotiates")
    (507 "Insufficient Storage")
    (508 "Loop Detected")
    (509 "Bandwidth Limit Exceeded")
    (510 "Not Extended")
    (511 "Network Authentication Required")
    (598 "Network Read Timeout Error")
    (599 "Network Connect Timeout Error")
    (t "Unknown Status")))

(defun http-client (uri request-cb event-cb &key (method :GET) headers body timeout)
  "Asynchronously contact an HTTP server. Allows passing of method, headers, and
   body. If host is not present in the headers, it is set from the hostname (if
   given) in the URI. Also supports setting a timeout."
  (check-event-loop-running)
  (let* ((data-pointer (create-data-pointer))
         (parsed-uri (puri:parse-uri uri))
         (host (puri:uri-host parsed-uri))
         (port (or (puri:uri-port parsed-uri) 80))
         (resource (or (puri:uri-path parsed-uri) "/"))
         (resource (if (puri:uri-query parsed-uri)
                       (concatenate 'string resource "?" (puri:uri-query parsed-uri))
                       resource))
         (dns-base (if (ip-address-p host)
                       (cffi:null-pointer)
                       (get-dns-base)))
         (connection (le:evhttp-connection-base-new *event-base*
                                                    dns-base
                                                    host
                                                    port))
         (request (le:evhttp-request-new (cffi:callback http-client-cb) data-pointer)))
    ;; track when the connection closes
    (le:evhttp-connection-set-closecb connection (cffi:callback http-client-close-cb) (cffi:null-pointer))
    (save-callbacks connection (list :request-cb request-cb :event-cb event-cb))
    (attach-data-to-pointer data-pointer (list :connection connection :dns-base dns-base))
    (when (numberp timeout)
      (le:evhttp-connection-set-timeout connection timeout))
    (let ((host-set nil)
          (header-ptr (le:evhttp-request-get-output-headers request)))
      (dolist (header headers)
        (unless (string= (car header) "Connection")
          (le:evhttp-add-header header-ptr (car header) (cdr header))
          (when (string= (string-downcase (car header)) "host")
            (setf host-set t))))
      (unless host-set
        (le:evhttp-add-header header-ptr "Host" host))
      ;; always close
      (le:evhttp-add-header header-ptr "Connection" "close"))
    (when body
      (write-to-evbuffer (le:evhttp-request-get-output-buffer request) body))
    (le:evhttp-make-request connection request (get-method-reverse method) resource)
    (incf *outgoing-http-count*)))

(defun http-server (bind port request-cb event-cb)
  "Start an HTTP server. If `bind` is nil, it bind to 0.0.0.0. Returns a wrapper
   around the HTTP server that allows the app to close it via close-http-server."
  (check-event-loop-running)
  (let* ((data-pointer (create-data-pointer))
         (http-base (le:evhttp-new *event-base*))
         (server-class (make-instance 'http-server :c http-base)))
    (when (eq http-base 0)
      (error "Error creating HTTP listener"))
    (le:evhttp-set-gencb http-base (cffi:callback http-request-cb) data-pointer)
    (save-callbacks data-pointer (list :request-cb request-cb :event-cb event-cb))
    (attach-data-to-pointer data-pointer server-class)
    (let* ((bind (if bind bind "0.0.0.0"))
           (socket (le:evhttp-bind-socket-with-handle http-base bind port)))
      (when (eq socket 0)
        (error "Couldn't bind HTTP listener to ~a:~a" bind port))
      (setf (http-server-sock server-class) socket)
      (add-event-loop-exit-callback (lambda ()
                                      (close-http-server server-class)
                                      (free-http-server server-class))))
    server-class))

(defun http-response (http-request &key (status 200) headers (body ""))
  "Called by the server when a request has been finished and a response is ready
   to be sent to the client.

   Takes the original http-request object passed into the request callback and
   optional keywords:

   :status   The HTTP status code to send back in the response
   :body     The content body to send back to the client (byte array or string)
   :headers  A list if (key . val) pairs to send back as headers:
      '((\"Content-Type\" . \"text/html\") ...)

   This function also does the cleanup needed for the request. So basically,
   every HTTP request should end with this function."
  (let* ((evbuffer (le:evbuffer-new))
         (req-c (http-request-c http-request))
         (output-headers (le:evhttp-request-get-output-headers req-c))
         (http-server (http-request-server http-request)))
    (write-to-evbuffer evbuffer body)
    (dolist (header headers)
      (le:evhttp-add-header output-headers (car header) (cdr header)))
    (if (<= 200 status 299)
        (le:evhttp-send-reply req-c status (lookup-status-text status) evbuffer)
        (le:evhttp-send-error req-c status (lookup-status-text status)))
    (le:evbuffer-free evbuffer)
    (decf *incoming-http-count*)
    (when (and (zerop *incoming-http-count*) (cffi:null-pointer-p (http-server-sock http-server)))
      (free-http-server http-server))))


