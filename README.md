cl-async - Asynchronous operations for Common Lisp
==================================================
So after trying out various non-blocking libraries and frameworks for CL, I was
a bit unsatisfied. [IOLib](http://common-lisp.net/project/iolib/) is probably the
best thing out there for non-blocking TCP, but I had a hard time with the 
interface and using it without beta versions of CFFI. I decided to write a
library that has very simple to understand concepts, an easy to use interface,
and is portable across Linux and Windows. It uses [Libevent2](http://libevent.org/)
as the async backend, which is a fast, stable, portable library for asynchronous
IO (see my [notes on choosing Libevent](#libevent)).

The main goal is to provide an experience that's close to javascript in how it
handles asynchronous operations with ease, but with the speed and power of
lisp. Portability and ease of use are favored over raw speed.

*Please note that at the moment I consider this library BETA. I'm doing my best
to solidify the API and eliminate any bugs. cl-async will most likely get a lot
more fixes and changes once it's put into production, which should hopefully not
be too far off. Stay tuned.*

The documentation is split into a few main sections.

- [__Function and class documentation__](#functions-and-classes)
- [__Conditions and events__](#conditions-and-events)
- [__Event callbacks and error handling__](#event-callbacks-and-error-handling-in-general)
- [__Examples__](#examples)
- [__Benchmarks__](#benchmarks)
- [__Implementation notes__](#implementation-notes)
- [__Drivers__](#drivers)
- [__License__](#license)

See the [TODO list](https://github.com/orthecreedence/cl-async/issues).

Functions and classes
----------------------
You can use cl-async with the prefixes `cl-async:` or `as:`. Throughout the
functions documented below, you will see a lot of `event-cb` callback arguments.
Since any callback labelled `event-cb` has the same specification, they are not
documented here. Please refer to the
[section on error handling](#event-callbacks-and-error-handling-in-general)
for more information on these callbacks (and error handling in general).

- [start-event-loop](#start-event-loop) _function_
- [exit-event-loop](#exit-event-loop) _function_
- [delay](#delay) _function_
- [timer](#timer) _function (deprecated)_
- [signal-handler](#signal-handler) _function_
- [free-signal-handler](#free-signal-handler)
- [clear-signal-handlers](#clear-signal-handlers)
- [dns-lookup](#dns-lookup) _function_
- [tcp-send](#tcp-send) _function_
- [tcp-server](#tcp-server) _function_
- [close-tcp-server](#close-tcp-server)
- [write-socket-data](#write-socket-data) _function_
- [set-socket-timeouts](#set-socket-timeouts) _function_
- [enable-socket](#enable-socket) _function_
- [disable-socket](#disable-socket) _function_
- [socket-closed-p](#socket-closed-p) _function_
- [close-socket](#close-socket) _function_
- [http-client](#http-client) _function_
- [http-server](#http-server) _function_
- [close-http-server](#close-http-server) _function_
- [http-response](#http-response) _function_
- [http-request](#http-request) _class_
  - [http-request-c](#http-request-c) _accessor_
  - [http-request-method](#http-request-method) _accessor_
  - [http-request-uri](#http-request-uri) _accessor_
  - [http-request-resource](#http-request-resource) _accessor_
  - [http-request-querystring](#http-request-querystring) _accessor_
  - [http-request-headers](#http-request-headers) _accessor_
  - [http-request-body](#http-request-body) _accessor_
- [stats](#stats) _function_

### start-event-loop
Start the event loop, giving a function that will be run inside the event loop
once started. `start-event-loop` blocks the main thread until the event loop
returns, which doesn't happen until the loop is empty *or*
[exit-event-loop](#exit-event-loop) is called inside the loop.

This function must be called before any other operations in the library are
allowed. If you try to do an async operation without an event loop running, it
will throw an error.

It allows specifying callbacks for the fatal errors in libevent (called when
libevent would normally exit, taking your app with it), logging, and default
application error handling.

```common-lisp
;; definition:
(start-event-loop start-fn &key fatal-cb logger-cb default-event-cb catch-app-errors)

;; example:
(start-event-loop (lambda () (format t "Event loop started.~%")))
```

##### fatal-cb definition

```common-lisp
(lambda (errcode) ...)
```

##### logger-cb definition

```common-lisp
(lambda (loglevel msg) ...)
```

`loglevel` corresponds to syslog levels.

##### default-event-cb and catch-app-errors
Please see the [application error handling](#application-error-handling) section
for complete information on these. They correspond 1 to 1 with
[\*default-event-handler\*](#default-event-handler) and
[\*catch-application-errors\*](#catch-application-errors). Setting them when
calling `start-event-loop` not only cuts down on `setf`s you have to do when
starting your evented app, but also uses thread-local versions of the vars,
meaning you can start multiple event loops in multiple threads wiithout using
the same values for each thread.

### exit-event-loop
Exit the event loop. This will free up all resources internally and close down
the event loop.

Note that this doesn't let queued events process, and is the equivelent of
doing a force close. Unless you really need to do this and return control to
lisp, try to let your event loop exit of "natural causes" (ie, no events left to
process). You can do this by freeing your signal handlers, servers, etc. This
has the added benefit of letting any connected clients finish their requests
(without accepting new ones) without completely cutting them off.

```common-lisp
;; definition
(exit-event-loop)
```

### delay
Run a function asynchronously. Takes two optional parameters: `time`, the number
of seconds to wait before running the given function (run with no delay if
`nil`), and `event-cb` which can be used to catch application errors should they
occur while running `callback`.

```common-lisp
;; definition:
(delay callback &key time event-cb)

;; example:
(delay (lambda () (format t "Run me immediately after control is given to the event loop.~%")))
(delay (lambda () (format t "I will run 3.2 seconds after calling (delay).~%")) :time 3.2)
```

### timer
_Deprecated_

Yes, even in its infancy, this library has a deprecated function. Use
[delay](#delay) for running functions asynchronously!

### signal-handler
Create a signal handler. This listens for the given `signo` not only in the 
event loop, but also in the lisp app as well. It replaces the current lisp
signal handler by calling C's `signal` function. When a signal handler is freed
via [free-signal-handler](#free-signal-handler), the original lisp signal
handler is restored as it was before binding the signal handler.

Note that signals that aren't freed via [free-signal-handler](#free-signal-handler)
or [clear-signal-handlers](#clear-signal-handlers) will linger on even after all
other events are out of the event loop, which prevents it from exiting. If you
want your event loop to exit naturally, you must free your signals when you're
done with them.

```common-lisp
;; definition
(signal-handler signo signal-cb &key event-cb)

;; example
(signal-handler 2 (lambda (sig) (format t "got SIGINT: ~a~%" sig))
                  (lambda (err) (foramt t "error processing signal callback: ~a~%" err)))
```

The `signo` arg is the POSIX integer signal you want to handle.

In the case of `signal-handler`, `event-cb` will *only* be called when an error
occurs in the signal callback. There are no cl-async events that occur during
signal processing.

##### signal-cb definition
```common-lisp
(lambda (signo) ...)
```

### free-signal-handler
Unbinds a signal handler. This deletes the libevent signal listener event and
also restores the lisp signal handler that existed before calling
[signal-handler](#signal-handler).

```common-lisp
;; definition
(free-signal-handler signo)

;; example
(signal-handler 2 (lambda (sig)
                    (close-server *my-app-server*)
                    (free-signal-handler 2)))
```

### clear-signal-handlers
Clear all cl-async bound signal handlers. This deletes the libevent event
listeners and restores the original lisp signal handlers for each bound signal.

This is useful if you don't want to track all the signals you've bound and
[free](#free-signal-handler) them manually, but don't want to [exit the event
loop forcibly](#exit-event-loop).

```common-lisp
;; definition
(clear-signal-handlers)
```

### dns-lookup
__Note: this is [broken in 64-bit](https://github.com/orthecreedence/cl-async/issues/15).
Help fixing would be appreciated, as I can't seem to figure it out.__

Asynchronously lookup an IP address given a hostname. If the hostname is an IP
address already, the mechanics are the same although the callback is called
synchronously.

Please note that at this time, IPV6 is not supported. Libevent has support for
it, but I don't feel like wrapping up the necessary classes just yet. I'd rather
get IPV4 going and then focus on IPV6 when everything's working. While the
`resolve-cb` supports a family parameter, it will always be `AF_INET` until this
is implemented.

```common-lisp
;; definition
(dns-lookup host resolve-cb event-cb)

;; example
(dns-lookup "www.google.com"
            (lambda (host family)
              (format t "Address: ~a~%" host))
            (lambda (err) (format t "err: ~a~%" err)))
```

##### resolve-cb definition

```common-lisp
(lambda (ip-address-string ip-address-family) ...)
```

As mentioned, until IPV6 is implemented, `ip-address-family` will *always* be
`AF_INET`. To test this, you can use the included libevent2 package's definition
of `libevent2:+af-inet+` or `libevent2:+af-inet-6+` (`le:` for short).

### tcp-send
Open an asynchronous TCP connection to a host (IP or hostname) and port, once
connected send the given data (byte array or string) and process any response
with the given read callback. Also supports timing out after no data is read /
written in (in seconds). 

Note that `tcp-send` always opens a new connection. If you want to send data on
and existing connection (and also be able to set new read/write/event callbacks
on it), check out [write-socket-data](#write-socket-data).

Note that the `host` can be an IP address *or* a hostname, the hostname will
be looked up asynchronously via libevent's DNS implementation. Also note that
the DNS lookup does __not__ use [dns-lookup](#dns-lookup), but directly calls
into the libevent DNS functions, so [IPV6](https://github.com/orthecreedence/cl-async/issues/7) 
and [x86-64](https://github.com/orthecreedence/cl-async/issues/15) are both
supported when using DNS.

```common-lisp
;; definition:
(tcp-send host port data read-cb event-cb &key read-timeout write-timeout)

;; example:
(tcp-send "www.google.com" 80
          (format nil "GET /~c~c" #\return #\newline)
          (lambda (socket data)
            (when (pretend-http-package:process-http-stream data) 
              (close-socket socket)))  ; close the socket if done processing
          #'my-app-error-handler)
```

##### read-cb definition

```common-lisp
(lambda (socket byte-array) ...)
```

`socket` should never be dealt with directly as it may change in the future,
however it *can* be passed to other cl-async functions that take a `socket` arg.

##### write-cb definition

```common-lisp
(lambda (socket) ...)
```

The `write-cb` will be called after data written to the socket's buffer is
flushed out to the socket.

### tcp-server
Bind an asynchronous listener to the given bind address/port and start accepting
connections on it. It takes read and event callbacks (like [tcp-send](#tcp-send)).
If `nil` is passed into the bind address, it effectively binds the listener to
"0.0.0.0" (listens from any address). A connection backlog can be specified when
creating the server via `:backlog`, which defaults to -1.

This function returns a `tcp-server` class, which allows you to close the
server via [close-tcp-server](#close-tcp-server).

```common-lisp
;; definition
(tcp-server bind-address port read-cb event-cb &key (backlog -1))  =>  tcp-server

;; example
(tcp-server "127.0.0.1" 8080
            (lambda (socket data)
              (format t "data: ~a~%" data)
              (write-socket-data socket "i noticed you have brathes. i have brathes too. uhhhhuhuhuh."
                                 :write-cb (lambda (socket)
                                             (close-socket socket))))
            nil)  ;; use *default-event-handler* as the event handler for this operation
```

##### read-cb definition

```common-lisp
(lambda (socket byte-array) ...)
```

`socket` should never be dealt with directly as it may change in the future,
however it *can* be passed to other cl-async functions that take a `socket` arg.

### close-tcp-server
Takes a `tcp-server` class, created by [tcp-server](#tcp-server) and closes the
server it wraps. This can be useful if you want to shut down a TCP server
without forcibly closing all its connections.

If the given server is already closed, this function returns without doing
anything.

```common-lisp
;; definition
(close-tcp-server tcp-server)
```

### write-socket-data
Write data to an existing socket (such as one passed into a `tcp-send` read-cb).
Data can be a byte array or string (converted to a byte array via babel).
Supports resetting the callbacks on the given socket. The `write-cb` is useful
if you want to close the connection after sending data on the socket but want to
make sure the data sent before closing.

Note that if you call this using a socket that has been closed already, it will
throw a [socket-closed](#socket-closed) condition.

```common-lisp
;; definition
(write-socket-data socket data &key read-cb write-cb event-cb)

;; examples
(write-socket-data socket "thanks for connecting. how are you? (good|bad)"
                   :read-cb (lambda (socket data)
                              (my-app:continue-conversation socket data))
                   :event-cb (lambda (err)
                               (format t "condition while having convo: ~a~%" err)))

(write-socket-data socket "invalid command, closing connection"
                   :write-cb (lambda (socket) (close-socket socket)))
```

If you were to close the socket right after sending the data to the buffer,
there's no guarantee it would be sent out. Setting a `write-cb` guarantees that
the data is sent when called.

##### write-cb definition

```common-lisp
(lambda (socket) ...)
```

### set-socket-timeouts
Set the read/write timeouts (in seconds) on a socket. If nil, the timeout is
cleared, otherwise if a number, the timeout is set into the socket such that
when the socket is active and hasn't been read from/written to in the specified
amount of time, it is closed.

`nil` for a timeout value unsets the timeout.

Note that if you call this using a socket that has been closed already, it will
throw a [socket-closed](#socket-closed) condition.

```common-lisp
;; definition
(set-socket-timeouts socket read-sec write-sec)

;; example
(set-socket-timeouts socket 10.5 nil)
```

### enable-socket
Enable read/write monitoring on a socket. This is done automatically by
[tcp-send](#tcp-send) and [write-socket-data](#write-socket-data) so you
probably don't need to worry too much about when to use it. On the other hand,
[disable-socket](#disable-socket) will probably be a bit more useful.

```common-lisp
;; definition
(enable-socket socket &key read write)

;;example
(enable-socket socket :read t :write t)  ; enable read and write monitoring on this socket
```

### disable-socket
Disable read/write monitoring on a socket. This is useful if you get the data
you need from a socket, but while you're processing the data, you don't want the
socket's read timeout to fire. This will both disable the timeouts and callbacks
associated with the socket until enabled again.

```common-lisp
;; definition
(disable-socket socket &key read write)
```

### socket-closed-p
Determines if a socket has been closed already.

```common-lisp
;; definition
(socket-closed-p socket)
```

### close-socket
Close a socket and free its callbacks.

Note that if you call this using a socket that has been closed already, it will
throw a [socket-closed](#socket-closed) condition.

```common-lisp
;; definition
(close-socket socket)
```

### http-client
Asynchronously communicates with an HTTP server. Allows setting the method,
headers, and body in the request which should be enough to make just about any
HTTP request. This functionality wraps the libevent HTTP client.

If a "Host" header isn't passed in, it is automatically set with whatever host
is pulled out of the `uri`. Also, any "Connection" header passed in will be
ignored...for right now, every request is sent out with `Connection: close`,
but this [will probably change to be customizable soon](https://github.com/orthecreedence/cl-async/issues/19).

The `timeout` arg is in seconds.

```common-lisp
;; definition
(http-client uri request-cb event-cb &key (method 'GET) headers body timeout)

;; example
(http-client "http://musio.com/"
             (lambda (status headers body)
               (format t "Result: ~s~%" (list status headers (babel:octets-to-string body :encoding :utf-8))))
             (lambda (err)
               (format t "http event: ~a~%" err))
             :method 'GET
             :headers '(("Accept" . "text/html"))
             :timeout 5)
```

##### request-cb definition

```common-lisp
(lambda (http-status http-headers body-byte-array) ...)
```

- `http-status` is an integer corresponding to the HTTP status code returned.
- `http-headers` is an alist, as such: '(("Content-Type" . "text/html") ...)
- `body-byte-array` is pretty self-explanatory. Convert to string w/ babel if
needed.

### http-server
Start a server that asynchronously processes HTTP requests. It takes data out of
the request and populates the [http-request](#http-request) with it, which is
passed into the request callback.

This function returns an `http-server` class, which allows you to close the
server via [close-http-server](#close-http-server).

Once the application is done processing the request, it must respond by calling
the [http-response](#http-response) function.

If `nil` is passed in into the `bind` arg, the server is bound to "0.0.0.0"

```common-lisp
;; definition
(http-server bind port request-cb event-cb)  =>  http-server

;; example
(http-server "192.168.0.1" 8090
             (lambda (req)
               (format t "Request: ~a~%" req)
               (http-response req :body "hai")))
```

##### request-cb definition

```common-lisp
(lambda (http-request) ... )
```

`http-request` is a on object of type [http-request](#http-request).

### close-http-server
Takes an `http-server` class, created by [http-server](#http-server) and closes
the server it wraps. This can be useful if you want to shut down a HTTP server
without forcibly closing all its connections.

If the given server is already closed, this function returns without doing
anything.

Note: This function closes the listener for new HTTP client requests. Once the
current requests are finished processing, it frees all resources associated with
the server. In other words, a graceful exit.

```common-lisp
;; definition
(close-http-server http-server)

;; example
(let ((server (http-server "127.0.0.1" 80 ...)))
  (signal-handler 2 (lambda (sig)
                      (declare (ignore sig))
                      ;; close the server when we get SIGINT
                      (close-http-server server))))
```

### http-response
This is the function called by the application using an [http-server](#http-server)
after it is done processing a request. It takes the [http-request](#http-request)
object passed into the request callback, along with some information about the
response we're sending.

```common-lisp
;; definition
(http-response http-request &key (status 200) headers (body ""))

;; example
(http-server nil 80
             (lambda (req)
               (http-response req
                              :status 200
                              :headers '(("Content-Type" . "application/json"))
                              :body "{\"name\":\"larry\"}")))
```

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

```common-lisp
'(GET POST HEAD PUT DELETE OPTIONS TRACE CONNECT PATCH)
```

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

```common-lisp
'(("Host" . "musio.com")
  ("Accept" . "text/html"))
```

##### http-request-body
Get the body out of the request. Since we don't make any assumptions about the
data that's being passed around, it is a byte array. Convert it to a string in
your app via `babel:octets-to-string` if needed.

It's important to note that at this time, multipart form data, posted files, etc
are *not* decoded by `http-server`. As such, it is currently up to your app to
do this. *This may change in the future* and if so, I will do my best to make the
change backwards compatible.

### stats
This function returns data on the current state of the cl-async internals. How
many incoming/outgoing connections, how many registered callbacks, how many
registered data objects, how many open DNS requests, etc.

Data is a plist. Stats might change in the near future.

```common-lisp
;; definition
(stats)
```

Conditions and events
---------------------
When something unexpected happens, cl-async will _instantiate_ (not throw) a
condition that explains what happened and pass it into the given 
[event callback](#event-callbacks-and-error-handling-in-general).
This can happen when an HTTP connection is refused, a TCP socket gets an EOF,
etc. Sometimes these conditions won't necessarily be errors, but rather pieces
of information your application might find useful.

The one condition that is _thrown_ by cl-async is [socket-closed](#socket-closed),
which happens when a closed socket is being operated on by the app.

This goes over the conditions you can expect to see when using cl-async. For
information about how these conditions are handled once created, see the
[section explaining event callbacks and error handling](#event-callbacks-and-error-handling-in-general).

- [connection-info](#connection-info) _condition_
- [connection-error](#connection-error) _condition_
  - [conn-errcode](#conn-errcode) _accessor_
  - [conn-errmsg](#conn-errmsg) _accessor_
- [dns-error](#dns-error) _condition_
- [tcp-info](#tcp-info) _condition_
  - [tcp-socket](#tcp-socket) _accessor_
- [tcp-error](#tcp-error) _condition_
- [tcp-eof](#tcp-eof) _condition_
- [tcp-timeout](#tcp-timeout) _condition_
- [tcp-refused](#tcp-refused) _condition_
- [tcp-accept-error](#tcp-accept-error) _condition_
  - [tcp-accept-error-listener](#tcp-accept-error-listener) _accessor_
  - [tcp-accept-error-tcp-server](#tcp-accept-error-tcp-server) _accessor_
- [socket-closed](#socket-closed) _condition_
- [http-info](#http-info) _condition_
- [http-error](#http-error) _condition_
- [http-timeout](#http-timeout) _condition_
- [http-refused](#http-refused) _condition_

### connection-info
Base connection condition. Signals that "something" happened on a connection.
Meant to be extended.

### connection-error
_extends [connection-info](#connection-info)_

Base connection error. Signals that "something bad" happened on a connection.

##### conn-errcode
The error code associated with the connection error. This is generally retrieved
from the underlying OS, but sometimes cl-async will generate its own error
conditions, in which case errcode will be -1.

##### conn-errmsg
Much like `conn-errcode`, this is generally a system message explaining a
connection error. If it is a cl-async generated error, it will have a string
value explaining what happened.

### dns-error
_extends [connection-error](#connection-error)_

This explains a DNS error (for instance if a DNS lookup fails).

### tcp-info
_extends [connection-info](#connection-info)_

Base TCP condition, says "something" happened on a TCP connection.

##### tcp-socket
Holds the TCP socket class. Can be used to write to the socket or close it.

### tcp-error
_extends [connection-error](#connection-error) and [tcp-info](#tcp-info)_

Describes a general error on a TCP connection. If this is triggered, the socket
will generally be closed by cl-async, and the app doesn't need to worry about
doing this. If the app *does* want to close the socket, it can do so by getting
it from the [tcp-socket](#tcp-socket) accessor on the condition and using
[close-socket](#close-socket).

### tcp-eof
_extends [tcp-info](#tcp-info)_

Triggered when the peer on a TCP connection closes the socket.

### tcp-timeout
_extends [tcp-error](#tcp-error)_

Triggered when a TCP connection times out.

### tcp-refused
_extends [tcp-error](#tcp-error)_

Triggered when a TCP connection is refused by the peer.

### tcp-accept-error
_extends [tcp-error](#tcp-error)_

Passed to a [tcp-server](#tcp-server)'s `event-cb` when there is an error
accepting a client connection.

##### tcp-accept-error-listener
The libevent listener c object. Provided in case your app needs to process it in
some way.

##### tcp-accept-error-tcp-server
The `tcp-server` object that the accept error happened on.

### socket-closed
_extends [tcp-error](#tcp-error)_

This exception is thrown by cl-async when the app tries to perform an operation
on a socket that has already been closed via [close-socket](#close-socket).

### http-info
_extends [connection-info](#connection-info)_

Base HTTP condition.

### http-error
_extends [connection-error](#connection-error) and [http-info](#http-info)_

Base HTTP error condition.

### http-timeout
_extends [http-error](#http-error)_

Triggered when an HTTP connection times out.

### http-refused
_extends [http-error](#http-error)_

Triggered when an HTTP connection is refused by the peer.


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

### Application error handling
cl-async can be set up to catch errors in your application and pass them to
your `event-cb`. This makes for seamless error handling, and keeps a rogue
condition from exiting the event loop (assuming you have an `event-cb` set for
the operation that generated the condition, or a default event handler that
deals with the condition).

Note that the following variables are also controllable on a per-event-loop
basis via the [start-event-loop](#start-event-loop) keyword arguments
`:catch-app-errors` and `:default-event-cb`. It might actually be favorable to
use [start-event-loop](#start-event-loop) since it creates thread-local versions
of these variables when instantiating, which can be useful if running event
loops in multiple threads.

##### \*catch-application-errors\*
_default: `nil`_

By setting this to true, you allow cl-async to catch conditions in your app and
pass them to the event callback associated with the procedure that triggered the
condition.

If this is left as `nil`, triggered conditions will make their way to the top
level and cause the event loop to exit, cancelling any pending events (unless
you have restarts implemented in your app).

##### \*default-event-handler\*
When [\*catch-application-errors\*](#catch-application-errors) is set to `t`
and an `event-cb` is not specified for an operation, the function assigned to
this variable will be used as the `event-cb`. The default:

```common-lisp
(lambda (err)
  ;; throw the error so we can wrap it in a handler-case
  (handler-case (error err)
    ;; got a connection error, throw it (must do this explicitely since
    ;; connection-error extends connection-info)
    (connection-error () (error err))

    ;; this is just info, let it slide
    (connection-info () nil)

    ;; this an actual error. throw it back to toplevel
    (t () (error err))))
```

This can be changed by your application if different behavior is desired.

Examples
--------
Some limited examples are outlined above, but I learn by example, not reading
function definitions and specifications. So here's some more to get you going.

### An echo server

```common-lisp
(defun my-echo-server ()
  (format t "Starting server.~%")
  (as:tcp-server nil 9003  ; nil is "0.0.0.0"
                 (lambda (socket data)
                   ;; echo the data back into the socket
                   (as:write-socket-data socket data))
                 (lambda (err) (format t "listener event: ~a~%" err)))
  ;; catch sigint
  (as:signal-handler 2 (lambda (sig)
                         (declare (ignore sig))
                         (as:exit-event-loop))))

(as:start-event-loop #'my-echo-server)
```

This echos anything back to the client that was sent, until SIGINT is recieved,
which forcibly closes the event loop and returns to the main thread.

Benchmarks
----------
So far, benchmarks are favorable. From my intial profiling, it seems most of the
time is spent in CFFI when on Windows, but in linux (of course) CFFI is a minor
speed bump, and the actual cl-async:* functions are the main slowdown (which is
good). Because of this, I really recommend running any production server on
linux. This isn't so much because Windows sucks, but because I feel like most
lisp implementations focus on linux performance a lot more than Windows.

On my (already crowded) Linode 512, cl-async (for both [tcp-server](#tcp-server)
and [http-server](#http-server)) was able to process about 40K concurrent
requests with this example before running out of memory:

```common-lisp
(defparameter *http-response*
  (babel:string-to-octets
    (with-output-to-string (s)
      (format s "HTTP/1.1 200 OK~c~c" #\return #\newline)
      (format s "Date: Wed, 03 Oct 2012 23:43:10 GMT~c~c" #\return #\newline)
      (format s "Content-Type: text/plain~c~c" #\return #\newline)
      (format s "Content-Length: 9~c~c" #\return #\newline)
      (format s "~c~c" #\return #\newline)
      (format s "omglolwtf"))))

(defun tcp-server-test (&key stats)
  (as:start-event-loop
    (lambda ()
      (format t "Starting TCP server.~%")
      (let ((listener nil)
            (quit nil)
            (finished-requests 0)
            (last-finished 0)
            (last-time 0))
        (setf listener
              (as:tcp-server nil 9009
                             (lambda (socket data)
                               (declare (ignore data))
                               (as:delay (lambda ()
                                           (unless (as:socket-closed-p socket)
                                             (as:write-socket-data
                                               socket *http-response*
                                               :write-cb (lambda (socket)
                                                           (as:close-socket socket)
                                                           (incf finished-requests)))))
                                         :time 5))
                             (lambda (err)
                               (format t "tcp server event: ~a~%" err))))
        (as:signal-handler 2 (lambda (sig)
                               (declare (ignore sig))
                               (setf quit t)
                               (as:free-signal-handler 2)
                               (as:close-tcp-server listener)))
        (labels ((show-stats ()
                   (let* ((stats (as:stats))
                          (incoming (getf stats :incoming-tcp-connections))
                          (outgoing (getf stats :outgoing-tcp-connections))
                          (now (get-internal-real-time))
                          (sec (/ (- now last-time) internal-time-units-per-second))
                          (rate (/ (- finished-requests last-finished) sec)))
                     (setf last-finished finished-requests
                           last-time now)
                     (format t "incoming: ~a~%outgoing: ~a~%finished: ~a~%rate: ~f req/s~%~%" incoming outgoing finished-requests rate))
                   (unless quit
                     (as:delay #'show-stats :time 1))))
          (when stats (show-stats)))))
    :catch-app-errors t)
  (format t "TCP server exited.~%"))

;; run it
(tcp-server-test :stats t)
```

What's happening here is that the server gets a request, delays 5 seconds, then
responds on the same socket. This allows connections to build up for 5 seconds
before they start getting released, which is a good way to test how many
connections it can handle.

On another neighboring Linode, I ran
```shell
httperf --server=1.2.3.4 --port=9009 --num-conns=40000 --num-calls=10 --hog --rate=6000
```

In the `stats` output, I was getting:

    incoming: 12645
    outgoing: 0
    finished: 7330
    rate: 6026.183 req/s
    
So I was getting ~6000k req/s, and in some tests (longer delay value) I was able
to get the "incoming" connections to 40K. 6000/s seems to be the limit of the
machine `httperf` was running on, not the server, but I can't confirm this yet.
From the tests I ran, memory seems to be the number one constraining factor in
scalability of number of connections. The more memory, the more connections can
be handled.

Implementation notes
--------------------
### Libevent
Libevent was chosen for a few reasons:
 - It provides a socket API. The IOLib library was too undocumented for me
 to figure out. Plus things like delayed functions/timers were not clear to me.
 - It wraps the socket implementation and buffering in a [simple and wonderful
 API](http://www.wangafu.net/~nickm/libevent-book/Ref6_bufferevent.html).
 - It was very easy to generate bindings for and wrap in CFFI.
 - It works with Windows. This is big for me, since I do a ton of development
 on Windows. Libraries that assume "Y WOULD U PROGRAM ON WINDOWS?!?!LOL" have
 their place, but most people who say this probably use Ubuntu anyway (sorry,
 it just slipped out). Libevent can be compiled on Windows just fine, and
 with a bit of work, I'm assuming this wrapper could be programmed to use the
 IOCP parts of libevent (I think for now it uses select())
 - It comes with asynchronous HTTP client/server implementations. These are not
 trivial, and if libevent makes it easier to have an asynchronous CL webserver
 or client, then hell let's use it.

The bindings for libevent are auto-generated. I'm not proud of the bindings
themselves, but because I planned to completely wrap them all along, didn't put
too much work into making them pretty and useful. They will most likely stay
as-is (and undocumented).

### HTTP server
The [http-server](#http-server) is a simple way to get a quick HTTP interface
for your app. However, as someone who does a lot of ops as well as dev, I must
warn you that **I would not trust this to be public-facing**. This is not
because I am a terrible programmer, but because I don't think libevent's HTTP
implementation takes into account a lot of things that other HTTP servers have
been battle tested with.

In other words, put [HAProxy](http://haproxy.1wt.eu/) or [NginX](http://nginx.org/)
(or similar) in front of it. Let someone else bear the brunt of dealing with the
security flaws of the open web so you can focus on building a solid application.

### Internals
cl-async tracks anonymous callbacks and libevent objects using what are called
data pointers. A data pointer is just a CFFI pointer that can be passed around
to libevent callbacks, and can also be used to pull data out of a hash table.
So while CFFI callbacks cannot be anonymous, we can fake it by creating a data
pointer, assigning the app-supplied anonymous callbacks to the data pointer in
a hash table lookup (pointer => callbacks), and sending the pointer (in what
would be a void\* argument in C) to the libevent callback. Once the generic
CFFI callback is fired, it can pull out the anonymous callbacks (as well as any
assigned libevent objects) using the data pointer and do what it needs to with
them. Data pointers (and the data attached to them in the function/data hash
tables) are freed once no longer needed. This is managed completely by cl-async.

### TODO
Please see the [Issues list](https://github.com/orthecreedence/cl-async/issues)
for the complete list of what needs to be done.

Drivers
-------
I plan on building and releasing a number of drivers on top of cl-async:

- [beanstalkd](https://github.com/orthecreedence/beanstalk-async)
- MongoDB
- Drakma (async port, would essentially replace [http-client](#http-client))
- Amazon S3/Cloudfront
- SMTP
- Redis

Note that these are libraries I use every day, so am in a good position to test
them in a production environment. Also, even though cl-async includes a simple
HTTP client, [Drakma](http://weitz.de/drakma/) is a lot more badass and has a
ton more features. Porting it to be asynchronous would be very valuable, and
also would make porting other drivers that work over HTTP to cl-async easier.

The biggest problem with asynchronous IO in lisp is that there are lots of
libraries that provide it, but no drivers built on top of the libraries. Nobody
wants to sit around all day programming database drivers. I think if I get
enough traction behind cl-async by providing drivers for enough services, it
could stand to be the first viable asynchronous programming library for Common
Lisp users.

So all I need is critical mass. WHO'S WITH ME?!?!

License
-------
As always, my code is MIT licenced. Do whatever the hell you want with it.
Enjoy!

