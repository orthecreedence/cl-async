;;; This file wraps around a lot of the libevent2 evhttp_* functions, providing
;;; an asynchronous HTTP server that you can build your application on top of.
;;; Each request is put into an `http-request` class, which is sent off to your
;;; app.
;;;
;;; Once the app is finished processing the request, it passes the request
;;; object into the http-response function and the response will be send and all
;;; data cleaned up.

(in-package :cl-async)

(defclass http-request ()
  ((req-obj :accessor http-request-c :initarg :c :initform nil :documentation
     "Holds the libevent evhttp request foreign pointer. Can be used to get more
      information from the request if needed via the libevent evhttp_* functions
      if needed, but for the most part it is just used to send the response back
      to the client asynchronously after the app is done processing the request.")
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

(defun free-http-base (http-base)
  "Free an HTTP server object and clear any callbacks associated with it."
  (le:evhttp-free http-base)
  (free-pointer-data http-base :preserve-pointer t))

(defun get-method (enum)
  "Given a libevent EVHTTP_REQ enum key word, return the appropriate method
   string."
  (case enum
    (:+evhttp-req-get+ 'GET)
    (:+evhttp-req-post+ 'POST)
    (:+evhttp-req-head+ 'HEAD)
    (:+evhttp-req-put+ 'PUT)
    (:+evhttp-req-delete+ 'DELETE)
    (:+evhttp-req-options+ 'OPTIONS)
    (:+evhttp-req-trace+ 'TRACE)
    (:+evhttp-req-connect+ 'CONNECT)
    (:+evhttp-req-patch+ 'PATCH)))

(defun get-method-reverse (method)
  "Given a method string, return the appropriate enum value for the libevent
   type."
  (case (cond
          ((stringp method) (read-from-string method))
          ((symbolp method) method))
    (GET :+evhttp-req-get+)
    (POST :+evhttp-req-post+)
    (HEAD :+evhttp-req-head+)
    (PUT :+evhttp-req-put+)
    (DELETE :+evhttp-req-delete+)
    (OPTIONS :+evhttp-req-options+)
    (TRACE :+evhttp-req-trace+)
    (CONNECT :+evhttp-req-connect+)
    (PATCH :+evhttp-req-patch+)
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

(cffi:defcallback http-request-cb :void ((request :pointer) (data-pointer :pointer))
  "ALL HTTP requests come through here. They are processed into the http-request
   class and sent off to the appropriate callback."
  ;; TODO: process request body
  (let* ((callbacks (get-callbacks data-pointer))
         (request-cb (getf callbacks :request-cb))
         (event-cb (getf callbacks :event-cb)))
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
         (callbacks (get-callbacks connection))
         (dns-base (getf pointer-data :dns-base))
         (request-cb (getf callbacks :request-cb))
         (event-cb (getf callbacks :event-cb)))
    (catch-app-errors event-cb
      (unwind-protect
        (cond
          ;; timeout
          ((cffi:null-pointer-p request)
           (funcall event-cb (make-instance 'connection-timeout :connection connection)))
          ;; connection refused
          ((eq (le:evhttp-request-get-response-code request) 0)
           (funcall event-cb (make-instance 'connection-refused :connection connection)))
          ;; got response back, parse and send off to request-cb
          (t
           (let ((status (le:evhttp-request-get-response-code request))
                 (body (drain-evbuffer (le:evhttp-request-get-input-buffer request)))
                 (headers (http-get-headers (le:evhttp-request-get-input-headers request))))
             ;; This segfaults *sometimes* so are we not supposed to call this?
             ;(le:evhttp-request-free request)
             (funcall request-cb status headers body))))
        (free-dns-base dns-base)
        (free-pointer-data data-pointer)
        ;; free the connection if it exists
        (unless (cffi:null-pointer-p connection)
          (le:evhttp-connection-free connection))))))

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

(defparameter *url-scanner*
  (cl-ppcre:create-scanner
    "^([a-z]+)://(([\\w-]+):([\\w-]+)@)?([^/:\?]+)(:([0-9]+))?((/|\\?).*)?$"
    :case-insensitive-mode t)
  "Scanner that splits URLs into their multiple parts. A bit sloppy and general,
   but works great for it's purpose.")

(defun parse-uri (uri)
  "Given a full URI:
     http://andrew:mypass@myhost.lol.com:9000/resource?query=string
   Parse it into a plist:
     '(:protocol \"http\"
       :user \"andrew\"
       :password \"mypass\"
       :host \"myhost.lol.com\"
       :port 9000
       :resource \"/resource?query=string\")"
  (let ((parts (cadr (multiple-value-list (cl-ppcre:scan-to-strings *url-scanner* uri)))))
    (when (and parts
               (not (stringp parts)))
      (let ((protocol (aref parts 0))
            (user (aref parts 2))
            (pass (aref parts 3))
            (host (aref parts 4))
            (port (aref parts 6))
            (resource (aref parts 7)))
        (list :protocol protocol
              :user user
              :password pass
              :host host
              :port (if port (read-from-string port) 80)
              :resource (if resource resource "/"))))))

(defun http-client (uri request-cb event-cb &key (method 'GET) headers body timeout)
  "Asynchronously contact an HTTP server. Allows passing of method, headers, and
   body. If host is not present in the headers, it is set from the hostname (if
   given) in the URI. Also supports setting a timeout."
  (check-event-loop-running)
  (let* ((data-pointer (create-data-pointer))
         (parsed-uri (parse-uri uri))
         (host (getf parsed-uri :host))
         (dns-base (if (ip-address-p host)
                       (cffi:null-pointer)
                       (get-dns-base)))
         (connection (le:evhttp-connection-base-new *event-base*
                                                    dns-base
                                                    host
                                                    (getf parsed-uri :port 80)))
         (request (le:evhttp-request-new (cffi:callback http-client-cb) data-pointer)))
    (save-callbacks connection (list :request-cb request-cb :event-cb event-cb))
    (attach-data-to-pointer data-pointer (list :connection connection :dns-base dns-base))
    (when (numberp timeout)
      (le:evhttp-connection-set-timeout connection timeout))
    (let ((host-set nil)
          (header-ptr (le:evhttp-request-get-output-headers request)))
      (dolist (header headers)
        (le:evhttp-add-header header-ptr (car header) (cdr header))
        (when (string= (string-downcase (car header)) "host")
          (setf host-set t)))
      (unless host-set
        (le:evhttp-add-header header-ptr "Host" host)))
    (when body
      (write-socket-data (le:evhttp-request-get-output-buffer request) body :socket-is-evbuffer t))
    (le:evhttp-make-request connection request (get-method-reverse method) (getf parsed-uri :resource))))

(defun http-server (bind port request-cb event-cb)
  "Start an HTTP server. If `bind` is nil, it bind to 0.0.0.0"
  (check-event-loop-running)
  (let ((data-pointer (create-data-pointer))
        (http-base (le:evhttp-new *event-base*)))
    (when (eq http-base 0)
      (error "Error creating HTTP listener"))
    (add-event-loop-exit-callback (lambda ()
                                    (free-http-base http-base)
                                    (free-pointer-data data-pointer)))
    (le:evhttp-set-gencb http-base (cffi:callback http-request-cb) data-pointer)
    (save-callbacks data-pointer (list :request-cb request-cb :event-cb event-cb))
    (let* ((bind (if bind bind "0.0.0.0"))
           (handle (le:evhttp-bind-socket-with-handle http-base bind port)))
      (when (eq handle 0)
        (error "Couldn't bind HTTP listener to ~a:~a" bind port)))))

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
         (output-headers (le:evhttp-request-get-output-headers req-c)))
    (write-socket-data evbuffer body :socket-is-evbuffer t)
    (dolist (header headers)
      (le:evhttp-add-header output-headers (car header) (cdr header)))
    (if (<= 200 status 299)
        (le:evhttp-send-reply req-c status (lookup-status-text status) evbuffer)
        (le:evhttp-send-error req-c status (lookup-status-text status)))
    (le:evbuffer-free evbuffer)))

