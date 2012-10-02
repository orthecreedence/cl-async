cl-async - Asynchronous operations for Common Lisp
==================================================
So after trying out various non-blocking libraries and frameworks for CL, I was
a bit unsatisfied. [USOCKET](https://github.com/mcna/usocket) is probably the
best thing out there for non-blocking TCP, and it works well on Linux, but in
my opinion is poorly documented.

I decided to write a wrapper around [Libevent2](http://libevent.org/) that is,
above all, easy to use and understand. The concepts are similar to other libs,
but work much like how I'd want an asynchronous I/O library to work.

*Please note that at the moment, I consider this library ALPHA and very likely
to change...a lot. Please rely on it at your own risk, until things solidify a
bit.*

Usage
-----
You can use cl-async with the prefixes `cl-async:` or `as:`.

### start-event-loop  `function`
Start the event loop, giving a function that will be run inside the event loop
once started. This function blocks the main thread until the event loop returns,
which doesn't happen until the loop is empty *or* `(event-loop-exit)` is called
inside the loop.

    ;; definition:
    (start-event-loop start-fn)
    
    ;; example:
    (start-event-loop (lambda () (format t "Event loop started.~%")))

_The following functions assume an event loop is started and running._

### event-loop-exit
Exit the event loop. This will free up all resources internally and close down
the event loop.

    ;; definition
    (event-loop-exit)

### timer
Run a function after a specified amount of time (in seconds, decimals OK):

    ;; definition:
    (timer time-s callback)
    
    ;; example:
    (timer 3.2 (lambda () (format t "I ran! (3.2 seconds later)~%")))

### tcp-send
Open an asynchronous TCP connection to a host (IP or hostname) and port, once
connected send the given data (byte array or string) and process any response
with the given read callback. Also supports timing out after no data is read /
written in (in seconds). If a socket is specified via :socket in the args, then
the provided data will be written to the given socket instead of opening a new
one, and the given callbacks will be set as the new callbacks for the socket.
This can be useful if you need to set up a new request/response on an existing
socket.

Note that the `host` can be an IP address *or* a hostname, the hostname will
be looked up asynchronously via libevent.

    ;; definition:
    (tcp-send host port data read-cb fail-cb &key read-timeout write-timeout)
    
    ;; example:
    (tcp-send "www.google.com" 80
              (format nil "GET /~c~c" #\return #\newline)
              (lambda (socket data)
                (when (pretend-http-package:process-http-stream data) 
                  (close-socket socket)))  ; close the socket if done processing
              #'my-app-error-handler)

The callbacks are as follows:

    ;; read callback:
    (lambda (socket data-recieved) ...)
    
    ;; error callback
    (lambda (socket errors) ...)

### tcp-server
Bind an asynchronous listener to the given bind address/port and start accepting
connections on it. It takes read and failure callbacks (like [tcp-send](#tcp-send)).
If `nil` is passed into the bind address, it effectively binds the listener to
"0.0.0.0" (listens from any address).

    ;; definition
    (tcp-server bind-address port read-cb fail-cb)
    
    ;; example
    (tcp-server "127.0.0.1" 8080
                #'myapp:process-client-data
                #'myapp:error-handler)

Read/failure callbacks take the same arguments as the [tcp-send](#tcp-send) function.

### write-socket-data
Write data to an existing socket (such as one passed into a read-cb). Data can
be a byte array or string (converted to a byte array via babel).

    ;; definition
    (write-socket-data socket data)
    
    ;; example
    (defun read-cb (socket data)
      ...
      (write-socket-data socket "thxlol"))

### set-socket-timeouts
Set the read/write timeouts (in seconds) on a socket. If nil, the timeout is
cleared, otherwise if a number, the timeout is set into the socket such that
when the socket is active and hasn't been read from/written to in the specified
amount of time, it is closed.

    ;; definition
    (set-socket-timeouts socket read-sec write-sec)
    
    ;; example
    (set-socket-timeouts socket 10.5 nil)

### close-socket
Close a socket and free its callbacks.

    ;; definition
    (close-socket socket)

### http-client
Asynchronously communicates with an HTTP server. Allows setting the method,
headers, and body in the request which should be enough to make just about any
HTTP request. This functionality wraps the libevent HTTP client.

If a "Host" header isn't passed in, it is automatically set with whatever host
is pulled out of the `uri`.

The `timeout` arg is in seconds.

    ;; definition
    (http-client uri request-cb fail-cb &key (method 'GET) headers body timeout)

    ;; example
    (http-client "http://musio.com/"
                 (lambda (status headers body)
                   (format t "Result: ~s~%" (list status headers (babel:octets-to-string body :encoding :utf-8))))
                 (lambda (err)
                   (format t "ERROR!!!!: ~a~%" err))
                 :method 'GET
                 :headers '(("Accept" . "text/html"))
                 :timeout 5)

### http-server
Start a server that asynchronously processes HTTP requests. It takes data out of
the request and populates the [http-request](#http-request) with it, which is
passed into the request callback.

Once the application is done processing the request, it must respond by calling
the [http-response](#http-response) function.

If `nil` is passed in into the `bind` arg, the server is bound to "0.0.0.0"

    ;; definition
    (http-server bind port request-cb fail-cb)

    ;; example
    (http-server "192.168.0.1" 8090
                 (lambda (req)
                   (format t "Request: ~a~%" req)
                   (http-response req :body "hai")))

### http-response
This is the function called by the application using an [http-server](#http-server)
after it is done processing a request. It takes the [http-request](#http-request)
object passed into the request callback, along with some information about the
response we're sending.

    ;; definition
    (http-response http-request &key (status 200) headers (body ""))

    ;; example
    (http-server nil 80
                 (lambda (req)
                   (http-response req
                                  :status 200
                                  :headers '(("Content-Type" . "application/json"))
                                  :body "{\"name\":\"larry\"}")))

### http-request
This is the class passed to an HTTP request callback after a request comes in
from [http-server](#http-server). It must also be passed into
[http-response](#http-response) when the request is finished, since it holds the
pointer to the socket the request came in on.

### http-request accessors
This details the accessors in `http-request`.

#### http-request-c
Pulls out the pointer to the libevent request object. This is included just in
case extra processing is needed on the request that the library doesn't handle
for you. In other words, ignore this accessor unless you know the libevent evhttp
internals and are comfortable using the libevent CFFI wrapper included with
cl-async.

#### http-request-method
Pull out the request method. This is a symbol, and will be one of

    '(GET POST HEAD PUT DELETE OPTIONS TRACE CONNECT PATCH)

#### http-request-uri
This is the full request URI in the request. For instance, if the request was

    GET /documents/45?format=json

Then this will be the string "GET /documents/45?format=json"

#### http-request-resource
This is a string of the request resource (path). A request of

    GET /mysite/index?page=4

The resource will be "/mysite/index"

#### http-request-querystring
The querystring from the request (string). Everything after (and not including)
the "?"

#### http-request-headers
All headers given in the request as an alist:

    '(("Host" . "musio.com")
      ("Accept" . "text/html"))

#### http-request-body
Get the body out of the request. Since we don't make any assumptions about the
data that's being passed around, it is a byte array. Convert it to a string in
your app via `babel:octets-to-string` if needed.

It's important to note that at this time, multipart form data, posted files, etc
are *not* decoded by `http-server`. As such, it is currently up to your app to
do this. *This may change in the future* and if so, I will do my best to make the
change backwards compatible.

Examples
--------
Some limited examples are outlined above, but I learn by example, not reading
function definitions and specifications. So here's some more to get you going.

### An echo server

    (defun my-echo-server ()
      (format t "Starting server.~%")
      (as:tcp-server nil 9003  ; nil is "0.0.0.0"
                     (lambda (socket data)
                       "our read-cb, called when data is received from the client"
                       ;; convert the data into a string
                       (let ((str (babel:octets-to-string data :encoding :utf-8)))
                         (when (search "QUIT" str)
                           ;; sent "QUIT" so close the socket and exit
                           (as:close-socket socket)
                           (as:event-loop-exit)))
                       ;; echo the data back into the socket
                       (as:write-socket-data socket data))
                     (lambda () nil)))  ; error handler that does nothing
    (as:start-event-loop #'my-echo-server)

This echos anything back to the client that was sent, until "QUIT" is recieved,
which closes the socket and ends the event loop, returning to the main thread.

Implementation notes
--------------------
### TODO
 - Error handling. Need to catch errors in user code and fire appropriate
 cleanup functions and/or call appropriate "fail" callbacks. Right now this
 isn't really happening. More on this below.
 - Tests/benchmarks

### Error handling
One thing that is *not* solidified yet is error handling. The arguments for
error/failure callbacks will most likely stay the same, but the data passed into
them will most likely change in the near future.

Also, I need to figure out when it makes sense to auto-close connections (for
uncaught exceptions and such) and when to call failure callbacks. Right now,
everything works great until something goes wrong.

As I build a few applications with this library, except this to solidify, but
for now it's just a stab in the dark until I figure out how it should work. Be
warned!

### Libevent
The bindings for libevent are auto-generated. I'm not proud of the bindings
themselves, but because I planned to completely wrap them all along, didn't put
too much work into making them pretty and useful. They will most likely stay
as-is.

Libevent was chosen for a few reasons:
 - It provides a socket API. The USOCKET library was too undocumented for me
 to figure out. Plus things like delayed functions/timers were not clear to me.
 - It wraps the socket implementation and buffering in a simple and wonderful
 API. You say bloated, I say "thank god I didn't have to fucking wrap that in
 CFFI."
 - It has a great API, as far as wrapping in CFFI goes. Values are passed by
 reference instead of on the stack. Wonderful.
 - It works with windows. This is big for me, since I do a ton of development
 on windows. Libraries that assume "Y WOULD U PROGRAM ON WINDOWS?!?!LOL" have
 their place, but most people who say this probably use Ubuntu anyway (sorry,
 it just slipped out). Libevent can be compiled on windows just fine, and
 with a bit of work, I'm assuming this wrapper could be programmed to use the
 IOCP parts of libevent (I think for now it uses select())
 - It comes with asynchronous HTTP client/server implementations. These are not
 trivial, and if libevent makes it easier to have an asynchronous CL webserver
 or client, then hell let's use it.

Drivers
-------
I plan on building and releasing a number of drivers on top of this library. The
first one is going to be an [asynchronous beanstalkd driver](https://github.com/orthecreedence/beanstalk-async).
Then a MongoDB driver. And then every driver ever.

The biggest problem with asynchronous IO in lisp is that there are no drivers
that use asynchronous IO. I'm hoping to get enough *drive* (heh heh) behind this
to make asynchronous programming (ala Node.js) in lisp a viable option.

License
-------
As always, my code is MIT licenced. Do whatever the hell you want with it.
Enjoy!
