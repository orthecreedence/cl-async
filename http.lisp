(in-package :cl-async)

(defclass http-request ()
  ((req-obj :accessor http-request-c :initarg :c :initform nil)
   (method :accessor http-request-method :initarg :method :initform nil)
   (uri :accessor http-request-uri :initarg :uri :initform nil)
   (resource :accessor http-request-resouce :initarg :resource :initform nil)
   (querystring :accessor http-request-querystring :initarg :querystring :initform nil)
   (headers :accessor http-request-headers :initarg :headers :initform nil)
   (body :accessor http-request-body :initarg :body :initform nil)))

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
  (clear-callbacks http-base))

(defun get-method (enum)
  (case enum
    (:+evhttp-req-get+ "GET")
    (:+evhttp-req-post+ "POST")
    (:+evhttp-req-head+ "HEAD")
    (:+evhttp-req-put+ "PUT")
    (:+evhttp-req-delete+ "DELETE")
    (:+evhttp-req-options "OPTIONS")
    (:+evhttp-req-trace+ "TRACE")
    (:+evhttp-req-connect+ "CONNECT")
    (:+evhttp-req-patch+ "PATCH")))

(cffi:defcallback http-request-cb :void ((request :pointer) (http-base :pointer))
  "ALL HTTP requests come through here. They are processed into the http-request
   class and sent off to the appropriate callback."
  ;; TODO: process request body
  (let* ((callbacks (get-callbacks http-base))
         (request-cb (getf callbacks :request-cb))
         (fail-cb (getf callbacks :fail-cb)))
    (let* ((method (get-method (le:evhttp-request-get-command request)))
           (uri (le:evhttp-request-get-uri request))
           (uri (le:evhttp-uridecode uri 1 (cffi:null-pointer)))
           (find-q (position #\? uri))
           (resource (if find-q (subseq uri 0 find-q)))
           (querystring (if find-q (subseq uri (1+ find-q)) ""))
           (body (make-array 0 :element-type '(unsigned-byte 8))))

      ;; populate the body array. reads all input data from the HTTP stream,
      ;; and this byte array is set directly into the request class. the app can
      ;; turn it into a string if it wants, but we don't make any assumptions
      ;; here.
      (read-socket-data (le:evhttp-request-get-input-buffer request)
                        (lambda (data)
                          (setf body (append-array body data :element-type '(unsigned-byte 8))))
                        :socket-is-evbuffer t)

      ;; parse the headers
      (let ((header-ptr (le-a:evkeyvalq-thq-first (le:evhttp-request-get-input-headers request)))
            (headers nil))
        (loop while (not (cffi:null-pointer-p header-ptr)) do
          (let ((key (le-a:evkeyval-key header-ptr))
                (val (le-a:evkeyval-value header-ptr)))
            (push (cons key val) headers)
            (setf header-ptr (le-a:evkeyval-next header-ptr))))

        ;; build the final request object and send it into the request callback
        (let ((http-request (make-instance 'http-request
                                           :c request
                                           :method method
                                           :uri uri
                                           :resource resource
                                           :querystring querystring
                                           :headers headers
                                           :body body)))
          (funcall request-cb http-request))))))

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
      (let ((key (car header))
            (val (cdr header)))
        (le:evhttp-add-header output-headers key val)))
    (if (<= 200 status 299)
        (le:evhttp-send-reply req-c status (lookup-status-text status) evbuffer)
        (le:evhttp-send-error req-c status (lookup-status-text status)))
    (le:evbuffer-free evbuffer)))

(defun http-client (method uri &key data headers))

(defun http-server (bind port request-cb fail-cb)
  "Start an HTTP server. If `bind` is nil, it bind to 0.0.0.0."
  (check-event-loop-running)
  (let ((http-base (le:evhttp-new *event-base*)))
    (when (eq http-base 0)
      (error "Error creating HTTP listener"))
    (add-event-loop-exit-callback (lambda () (free-http-base http-base)))
    (le:evhttp-set-gencb http-base (cffi:callback http-request-cb) http-base)
    (save-callbacks http-base (list :request-cb request-cb :fail-cb fail-cb))
    (let* ((bind (if bind bind "0.0.0.0"))
           (handle (le:evhttp-bind-socket-with-handle http-base bind port)))
      (when (eq handle 0)
        (error "Couldn't bind HTTP listener to ~a:~a" bind port)))))

