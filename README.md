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

The documentation is split into a few main sections.

- [__Function and class documentation__](#functions-and-classes)
- [__Event callbacks and error handling__](#event-callbacks-and-error-handling-in-general)
- [__Examples__](#examples)
- [__Implementation notes__](#implementation-notes)
- [__Drivers__](#drivers)
- [__License__](#license)

Functions and classes
----------------------
You can use cl-async with the prefixes `cl-async:` or `as:`. Throughout the
functions documentded below, you will see a lot of `event-cb` callback
arguments. Since any callback labelled `event-cb` has the same specification,
they are not documented here. Please refer to the
[section on error handling](#event-callbacks-and-error-handling-in-general)
for more information on these callbacks (and error handling in general).

- [start-event-loop](#start-event-loop) _function_
- [event-loop-exit](#event-loop-exit) _function_
- [timer](#timer) _function_
- [dns-lookup](#dns-lookup) _function_
- [tcp-send](#tcp-send) _function_
- [tcp-server](#tcp-server) _function_
- [write-socket-data](#write-socket-data) _function_
- [set-socket-timeouts](#set-socket-timeouts) _function_
- [close-socket](#close-socket) _function_
- [http-client](#http-client) _function_
- [http-server](#http-server) _function_
- [http-response](#http-response) _function_
- [http-request](#http-request) _class_
  - [http-request-c](#http-request-c) _accessor_
  - [http-request-method](#http-request-method) _accessor_
  - [http-request-uri](#http-request-uri) _accessor_
  - [http-request-resource](#http-request-resource) _accessor_
  - [http-request-querystring](#http-request-querystring) _accessor_
  - [http-request-headers](#http-request-headers) _accessor_
  - [http-request-body](#http-request-body) _accessor_

### start-event-loop
Start the event loop, giving a function that will be run inside the event loop
once started. This function blocks the main thread until the event loop returns,
which doesn't happen until the loop is empty *or* `(event-loop-exit)` is called
inside the loop.

This function must be called before any other operations in the library are
allowed. Don't worry, if you try to do an asyn operation without an event loop
running, it will throw an error.

It allows specifying callbacks for the fatal errors in libevent (called when
libevent would normall exit, taking your app with it), logging, and default
application error handling.

    ;; definition:
    (start-event-loop start-fn &key fatal-cb logger-cb default-event-cb catch-app-errors)
    
    ;; example:
    (start-event-loop (lambda () (format t "Event loop started.~%")))

##### fatal-cb definition

    (lambda (errcode) ...)

##### logger-cb definition

    (lambda (loglevel msg) ...)

`loglevel` corresponds to syslog levels.

##### default-event-cb and catch-app-errors
Please see the [Application error handling](#application-error-handling) section
for complete information on these. They correspond 1 to 1 with
[\*default-event-handler\*](#default-event-handler) and
[\*catch-application-errors\*](#catch-application-errors). Setting them when
calling `start-event-loop` is really just a convenience to cut down on `setf`s.

### event-loop-exit
Exit the event loop. This will free up all resources internally and close down
the event loop.

    ;; definition
    (event-loop-exit)

### timer
Run a function after a specified amount of time (in seconds, decimals OK). It
optionally takes an `event-cb` which can be used to catch application errors
should they occur while running `callback`.

    ;; definition:
    (timer time-s callback &key event-cb)
    
    ;; example:
    (timer 3.2 (lambda () (format t "I ran! (3.2 seconds later)~%")))

### dns-lookup
Asynchronously lookup an IP address given a hostname. If the hostname is an IP
address already, the mechanics are the same although the callback is called
synchronously.

Please note that at this time, IPV6 is not supported. Libevent has support for
it, but I don't feel like wrapping up the necessary classes just yet. I'd rather
get IPV4 going and then focus on IPV6 when everything's working. While the
`resolve-cb` supports a family parameter, it will always be `AF_INET` until this
is implemented.

    ;; definition
    (dns-lookup host resolve-cb event-cb)

    ;; example
    (dns-lookup "www.google.com"
                (lambda (host family)
                  (format t "Address: ~a~%" host))
                (lambda (err) (format t "err: ~a~%" err)))

##### resolve-cb definition

    (lambda (ip-address-string ip-address-family) ...)

As mentioned, until IPV6 is implemented, `ip-address-family` will *always* be
`AF_INET`. To test this, you can use the included libevent2 package's definition
of `libevent2:+af-inet+` or `libevent2:+af-inet-6+` (`le:` for short).

### tcp-send
Open an asynchronous TCP connection to a host (IP or hostname) and port, once
connected send the given data (byte array or string) and process any response
with the given read callback. Also supports timing out after no data is read /
written in (in seconds). If a socket is specified via :socket in the args, then
the provided data will be written to the given socket instead of opening a new
one, and the given callbacks will be set as the new callbacks for the socket.
This can be useful if you need to set up a new request/response on an existing
socket.

If you want to write data to an existing socket without modifying the callbacks,
please see [write-socket-data](#write-socket-data).

Note that the `host` can be an IP address *or* a hostname, the hostname will
be looked up asynchronously via libevent.

    ;; definition:
    (tcp-send host port data read-cb event-cb &key read-timeout write-timeout)
    
    ;; example:
    (tcp-send "www.google.com" 80
              (format nil "GET /~c~c" #\return #\newline)
              (lambda (socket data)
                (when (pretend-http-package:process-http-stream data) 
                  (close-socket socket)))  ; close the socket if done processing
              #'my-app-error-handler)

##### read-cb definition

    (lambda (socket byte-array) ...)

`socket` should never be dealt with directly as it may change in the future,
however it *can* be passed to other cl-async functions that take a `socket` arg.

### tcp-server
Bind an asynchronous listener to the given bind address/port and start accepting
connections on it. It takes read and event callbacks (like [tcp-send](#tcp-send)).
If `nil` is passed into the bind address, it effectively binds the listener to
"0.0.0.0" (listens from any address).

    ;; definition
    (tcp-server bind-address port read-cb event-cb)
    
    ;; example
    (tcp-server "127.0.0.1" 8080
                (lambda (socket data) (myapp:process-client-data socket data))
                nil)  ;; use *default-event-handler* as the event handler for this operation

##### read-cb definition

    (lambda (socket byte-array) ...)

`socket` should never be dealt with directly as it may change in the future,
however it *can* be passed to other cl-async functions that take a `socket` arg.

### write-socket-data
Write data to an existing socket (such as one passed into a read-cb). Data can
be a byte array or string (converted to a byte array via babel). This is useful
if you want to write data to an existing socket without modifying its callbacks,
which is what [tcp-send](#tcp-send) would do.

    ;; definition
    (write-socket-data socket data)
    
### set-socket-timeouts
Set the read/write timeouts (in seconds) on a socket. If nil, the timeout is
cleared, otherwise if a number, the timeout is set into the socket such that
when the socket is active and hasn't been read from/written to in the specified
amount of time, it is closed.

`nil` for a timeout value unsets the timeout.

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
    (http-client uri request-cb event-cb &key (method 'GET) headers body timeout)

    ;; example
    (http-client "http://musio.com/"
                 (lambda (status headers body)
                   (format t "Result: ~s~%" (list status headers (babel:octets-to-string body :encoding :utf-8))))
                 (lambda (err)
                   (format t "ERROR!!!!: ~a~%" err))
                 :method 'GET
                 :headers '(("Accept" . "text/html"))
                 :timeout 5)

##### request-cb definition

    (lambda (http-status http-headers body-byte-array) ...)

- `http-status` is an integer corresponding to the HTTP status code returned.
- `http-headers` is an alist, as such: '(("Content-Type" . "text/html") ...)
- `body-byte-array` is pretty self-explanatory. Convert to string w/ babel if
needed.

### http-server
Start a server that asynchronously processes HTTP requests. It takes data out of
the request and populates the [http-request](#http-request) with it, which is
passed into the request callback.

Once the application is done processing the request, it must respond by calling
the [http-response](#http-response) function.

If `nil` is passed in into the `bind` arg, the server is bound to "0.0.0.0"

    ;; definition
    (http-server bind port request-cb event-cb)

    ;; example
    (http-server "192.168.0.1" 8090
                 (lambda (req)
                   (format t "Request: ~a~%" req)
                   (http-response req :body "hai")))

##### request-cb definition

    (lambda (http-request) ... )

`http-request` is a on object of type [http-request](#http-request).

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

`http-request` has a pretty-print method associated with it, so if you do
something like `(format t "~a~%" http-request)`, you'll get a nice, detailed
overview of the request (method, uri, headers, content body length (in bytes),
etc).

### http-request accessors
This details the accessors in `http-request`.

##### http-request-c
Pulls out the pointer to the libevent request object. This is included just in
case extra processing is needed on the request that the library doesn't handle
for you. In other words, ignore this accessor unless you know the libevent evhttp
internals and are comfortable using the libevent CFFI wrapper included with
cl-async.

##### http-request-method
Pull out the request method. This is a symbol, and will be one of

    '(GET POST HEAD PUT DELETE OPTIONS TRACE CONNECT PATCH)

##### http-request-uri
This is the full request URI in the request. For instance, if the request was

    GET /documents/45?format=json

Then this will be the string "GET /documents/45?format=json"

##### http-request-resource
This is a string of the request resource (path). A request of

    GET /mysite/index?page=4

The resource will be "/mysite/index"

##### http-request-querystring
The querystring from the request (string). Everything after (and not including)
the "?"

##### http-request-headers
All headers given in the request as an alist:

    '(("Host" . "musio.com")
      ("Accept" . "text/html"))

##### http-request-body
Get the body out of the request. Since we don't make any assumptions about the
data that's being passed around, it is a byte array. Convert it to a string in
your app via `babel:octets-to-string` if needed.

It's important to note that at this time, multipart form data, posted files, etc
are *not* decoded by `http-server`. As such, it is currently up to your app to
do this. *This may change in the future* and if so, I will do my best to make the
change backwards compatible.

Event callbacks (and error handling in general)
-----------------------------------------------
Any parameter labelled `event-cb` is what's known as an "event callback." Event
callbacks have one argument: a condition describing the event that caused them
to be invoked. Originally event callbacks were failure callbacks, but since
non-failure conditions are sometimes useful to an app, it made sense to make it
more generic.

The event conditions generally match conditions in libevent, although they try
to be as informative as possible. Note that conditions are not actually thrown,
but rather instantiated via `make-instance` and passed directly to the event
callback.

- [Application error handling](#application-error-handling)
- [General cl-async conditions](#general-cl-async-conditions)

### Application error handling
cl-async can be set up to catch errors in your application and pass them to
your `event-cb`. This makes for seamless error handling, and keeps a rouge
condition from exiting the event loop (assuming you have an `event-cb` set for
the operation that generated the condition).

Note that the following variables are also controllable on a per-event-loop
basis via the [start-event-loop](#start-event-loop) keyword arguments
`:catch-app-errors` and `:default-event-cb`.

##### \*catch-application-errors\*
_default: `nil`_

By setting this to true, you allow cl-async to catch conditions in your app and
pass them to the event callback associated with the procedure that triggered the
condition.

If this is left as `nil`, triggered conditions will make their way to the top
level and cause the event loop to exit, cancelling any pending events.

##### \*default-event-handler\*
When [\*catch-application-errors\*](#catch-application-errors) is set to `t`
and an `event-cb` is not specified for an operation, the function assigned to
this variable will be used as the `event-cb`. The default:

    (lambda (err)
      ;; throw the error so we can wrap it in a handler-case
      (handler-case (error err)
        ;; this is just info, let it slide
        (connection-info () nil)
        ;; this an actual error. throw it back to toplevel (will exit the
        ;; event loop and cancel any pending events)
        (t () (error err))))

This can be changed by your application if different behavior is desired.

### General cl-async conditions

- [connection-info](#connection-info) _condition_
  - [conn-fd](#conn-fd) _accessor_
- [connection-error](#connection-error) _condition_
  - [conn-errcode](#conn-errcode) _accessor_
  - [conn-errmsg](#conn-errmsg) _accessor_
- [connection-eof](#connection-eof) _condition_
- [connection-timeout](#connection-timeout) _condition_
- [connection-refused](#connection-refused) _condition_
- [connection-dns-error](#connection-dns-error) _condition_

### connection-info
This is the base condition for any connection event. Any other connection
condition extends it.

##### conn-fd
Pulls the connection file descriptor out of a connection-info condition. This is
not necessarily useful to an application, but may be used internally for the
tracking and cleaning of verious object. Exposed to the API since there are
instances where it could be useful.

### connection-error
_extends [connection-info](#connection-info)_

Base connection error condition. Anything considered an error happening on a
connection will extend this condition. Most of the time, will contain an error
code and error message. 

##### conn-errcode
The error code this socket error was triggered with. The code generally refers
to what the C `errno` code is (`WSAGetLastError` on Windows).

_NOTE:_ In cases where the error is triggered by cl-async, the error code will
generally be -1. This may change in the future to support cl-async specific
error codes.

##### conn-errmsg
The error string corresponding to the code in `conn-errcode`. If the error code
is -1, then this string will be a cl-async description of the error instead of
a system-supplied description.

### connection-eof
_extends [connection-info](#connection-info)_

Describes the condition when the peer closes a connection.

### connection-timeout
_extends [connection-error](#connection-error)_

Describes the condition when a connection has timed out either connecting or
waiting for a read/write.

### connection-refused
_extends [connection-error](#connection-error)_

Describes the condition when a connection has been refused by the peer.

### connection-dns-error
_extends [connection-error](#connection-error)_

Describes the condition when a DNS lookup has failed.

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
 - More [examples](#examples)!!
 - Wrap sockets/fds in their own classes, since right now there are CFFI
 pointers flying around willy nilly. While this works fine if people follow
 the API, it can potentially be disastrous as well. Wrapping pointers in
 classes would also allow methods to be used, which would cut down a good
 number of segfaults, I'm guessing.
 - Signal handling. Libevent supports signal handling events, but they aren't
 really caught in lisp land. Will probably end up implementing something like
 [cl-signal-handler](https://github.com/orthecreedence/cl-signal-handler) with
 app-fired, custom events in conjuction with libevent's signal handling.
 Attacking the problem from both sides should get it working.
 - Tests/benchmarks. I have benchmarked a bit, but nothing official. So far,
 the results are somewhat favorable (enough so that building this library wasn't
 a complete waste of time, anyway).

### Libevent
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

The bindings for libevent are auto-generated. I'm not proud of the bindings
themselves, but because I planned to completely wrap them all along, didn't put
too much work into making them pretty and useful. They will most likely stay
as-is.

Drivers
-------
*Once cl-async is more stable, this section will list the drivers built on top
of it to make it easy for people to find them.*

I plan on building and releasing a number of drivers on top of this library. The
first one is going to be an [asynchronous beanstalkd driver](https://github.com/orthecreedence/beanstalk-async).
Then a MongoDB driver (most likely built on top of [cl-mongo](https://github.com/fons/cl-mongo)).
And then every driver ever.

The biggest problem with asynchronous IO in lisp is that there are lots of
libraries that provide it, but no drivers built on top of the libraries. Nobody
wants to sit around all day programming database drivers. I think if I get
enough traction behind cl-async by providing drivers for enough services, it
could stand to be the first viable asynchronous programming library for Common
Lisp users.

That's the goal, anyway...

License
-------
As always, my code is MIT licenced. Do whatever the hell you want with it.
Enjoy!
