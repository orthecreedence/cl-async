---
title: TCP | Documentation
layout: default
---

TCP
===
This section details sending and receving data over TCP, along with how to deal
with cl-async sockets. It also goes over conditions/events one might run into
while using the TCP system.

- [tcp-connect](#tcp-connect) _function_
  - [tcp-send](#tcp-send) _function (deprecated)_
- [tcp-server](#tcp-server) _function_
- [close-tcp-server](#close-tcp-server)
- [socket](#socket) _class_
  - [socket-c](#socket-c) _accessor_
  - [socket-data](#socket-data) _accessor_
- [write-socket-data](#write-socket-data) _function_
- [set-socket-timeouts](#set-socket-timeouts) _function_
- [enable-socket](#enable-socket) _function_
- [disable-socket](#disable-socket) _function_
- [socket-closed-p](#socket-closed-p) _function_
- [close-socket](#close-socket) _function_
- [Conditions](#conditions)
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

<a id="tcp-connect"></a>
### tcp-connect
{% highlight cl %}
(defun tcp-connect (host port read-cb event-cb
                    &key data stream
                         connect-cb write-cb
                         (read-timeout -1) (write-timeout -1)
                  (dont-drain-read-buffer nil dont-drain-read-buffer-supplied-p))
  => socket/stream
{% endhighlight %}

Open an asynchronous TCP connection to a host (IP or hostname) and port. You can
specify data to be sent once the connection is established via the `:data`
keyword. All incoming data will be sent to the [read-cb](#tcp-connect-read-cb),
and any events will be sent to the [event-cb](/cl-async/event-handling).

`tcp-connect` returns a [socket](#socket) class, which wraps the libevent socket
implementation and also allows storing arbitrary data with the socket.

`tcp-connect` can also return a stream of type [async-io-stream](/cl-async/tcp-stream#async-io-stream)
when the keyword argument `:stream` is `T`. This allows [normal stream
operations on top of a non-blocking socket](/cl-async/tcp-stream).

Note that `tcp-connect` always opens a new connection. If you want to send data
on and existing connection (and also be able to set new read/write/event
callbacks on it), check out [write-socket-data](#write-socket-data), or in the
case of a stream, you can use `write-sequence` to send new data on the stream.

Note that the `host` can be an IP address *or* a hostname. The hostname will
be looked up asynchronously via libevent's DNS implementation.

{% highlight cl %}
;; example:
(tcp-connect "www.google.com" 80
             (lambda (socket data)
               (when (pretend-http-package:process-http-stream data) 
                 (close-socket socket)))  ; close the socket if done processing
             #'my-app-error-handler
             :data (format nil "GET /~c~c" #\return #\newline))
{% endhighlight %}

See the [tcp-stream page](/cl-async/tcp-stream) for some examples on stream
usage.

<a id="tcp-connect-read-cb"></a>
##### read-cb definition (default)

{% highlight cl %}
(lambda (socket byte-array) ...)
{% endhighlight %}

<a id="tcp-connect-read-cb-stream"></a>
##### read-cb definition (when tcp-connect's :stream is t)

{% highlight cl %}
(lambda (socket stream) ...)
{% endhighlight %}

Note that in this case, `stream` replaces the data byte array's position. Also,
when calling `:stream t` in `tcp-connect`, the read buffer for the socket is not
drained and is only done so by [reading from the stream](/cl-async/tcp-stream).

`stream` is always the same object returned from `tcp-connect` with `:stream t`.
It wraps the `socket` object.

<a id="tcp-connect-connect-cb"></a>
##### connect-cb definition
{% highlight cl %}
(lambda (socket ...))
{% endhighlight %}

The `connect-cb` will be fired when the connection from `tcp-connect` has been
established. Since sending data over the socket is somewhat transparent (either
via `:data` or [write-socket-data](#write-socket-data)), you don't really have
to know when a socket is ready to be written to. In some instances though, it
may be useful to know when the connection has been established, which is why
`:connect-cb` is exposed.

<a id="tcp-connect-write-cb"></a>
##### write-cb definition

{% highlight cl %}
(lambda (socket) ...)
{% endhighlight %}

The `write-cb` will be called after data written to the socket's buffer is
flushed out to the socket. If you want to send a command to a server and
immediately disconnect once you know the data was sent, you could close the
connection in your `write-cb`.

<a id="tcp-send"></a>
### tcp-send  (_deprecated_)
This function is a deprecated version of [tcp-connect](#tcp-connect). Use
`tcp-connect` instead, as `tcp-send` may be removed in later versions.

<a id="tcp-server"></a>
### tcp-server
{% highlight cl %}
(defun tcp-server (bind-address port read-cb event-cb &key connect-cb (backlog -1)))
  => tcp-server
{% endhighlight %}

Bind an asynchronous listener to the given bind address/port and start accepting
connections on it. It takes read and event callbacks (like [tcp-connect](#tcp-connect)).
If `nil` is passed into the bind address, it effectively binds the listener to
"0.0.0.0" (listens from any address). A connection backlog can be specified when
creating the server via `:backlog`, which defaults to -1. A `connect-cb` can
be passed in as a keyword arg, which sets a callback to be called whenever a new
connection comes in.

This function returns a `tcp-server` class, which allows you to close the
server via [close-tcp-server](#close-tcp-server).

{% highlight cl %}
;; example
(tcp-server "127.0.0.1" 8080
            (lambda (socket data)
              (format t "data: ~a~%" data)
              (write-socket-data socket "i noticed you have brathes. i have brathes too. uhhhhuhuhuh."
                                 :write-cb (lambda (socket)
                                             (close-socket socket))))
            nil)  ;; use *default-event-handler* as the event handler for this operation
{% endhighlight %}

<a id="tcp-server-read-cb"></a>
##### read-cb definition

{% highlight cl %}
(lambda (socket byte-array) ...)
{% endhighlight %}

`socket` should never be dealt with directly as it may change in the future,
however it *can* be passed to other cl-async functions that take a `socket` arg.

<a id="tcp-server-connect-cb"></a>
##### connect-cb definition

{% highlight cl %}
(lambda (socket) ...)
{% endhighlight %}

<a id="close-tcp-server"></a>
### close-tcp-server
{% highlight cl %}
(defun close-tcp-server (tcp-server))
{% endhighlight %}

Takes a `tcp-server` class, created by [tcp-server](#tcp-server) and closes the
server it wraps. This can be useful if you want to shut down a TCP server
without forcibly closing all its connections.

If the given server is already closed, this function returns without doing
anything.

<a id="socket"></a>
### socket
This class is a wrapper around the libevent socket class. It is passed to tcp
callback functions, and allows you to perform certain actions on the socket
(such as [closing it](#close-socket), [setting read/write timeouts](#set-socket-timeouts),
[writing data to it](#write-socket-data), etc).

It also exposes an accessor, [socket-data](#socket-data), which allows you to
store arbitrary, app-specific data in the socket.

<a id="socket-c"></a>
##### socket-c
This accessor lets you access the underlying [libevent bufferevent](http://www.wangafu.net/~nickm/libevent-book/Ref6_bufferevent.html)
object for the socket. While this is not immediately useful for any cl-async
related purpose (and manipulating it outside of cl-async may make your worst
nightmares come true if you aren't careful), it can be very useful to do your
own bufferevent operations through the [libevent bindings](https://github.com/orthecreedence/cl-libevent2).

TL;DR: Don't touch this accessor unless you are fairly familiar with libevent's
API and how it interacts with cl-async.

<a id="socket-data"></a>
##### socket-data
This accessor allows you to set arbitrary data into the [socket](#socket) class,
which can be useful if your app needs to match specific data to a socket (for
instance if you are proxying, you could use `socket-data` to store a reference
to the outgoing socket inside the incoming socket).

<a id="write-socket-data"></a>
### write-socket-data
{% highlight cl %}
(defun write-socket-data (socket data &key read-cb write-cb event-cb))
  => nil
{% endhighlight %}

Write data to an existing socket (such as one passed into a `tcp-connect` read-cb).
Data can be a byte array or string (converted to a byte array via babel).
Supports resetting the callbacks on the given socket. The `write-cb` is useful
if you want to close the connection after sending data on the socket but want to
make sure the data sent before closing.

Note that if you call this using a socket that has been closed already, it will
throw a [socket-closed](#socket-closed) condition.

{% highlight cl %}
;; examples
(write-socket-data socket "thanks for connecting. how are you? (good|bad)"
                   :read-cb (lambda (socket data)
                              (my-app:continue-conversation socket data))
                   :event-cb (lambda (err)
                               (format t "condition while having convo: ~a~%" err)))

(write-socket-data socket "invalid command, closing connection"
                   :write-cb (lambda (socket) (close-socket socket)))
{% endhighlight %}

If you were to close the socket right after sending the data to the buffer,
there's no guarantee it would be sent out. Setting a `write-cb` guarantees that
the data is sent when called.

Note that `write-socket-data`'s callbacks are identical to [tcp-connect](#tcp-connect)'s
and if specified, will override those set by [tcp-connect](#tcp-connect).

<a id="set-socket-timeouts"></a>
### set-socket-timeouts
{% highlight cl %}
(defun set-socket-timeouts (socket read-sec write-sec))
  => nil
{% endhighlight %}

Set the read/write timeouts (in seconds) on a socket. If nil, the timeout is
cleared, otherwise if a number, the timeout is set into the socket such that
when the socket is active and hasn't been read from/written to in the specified
amount of time, it is closed.

`nil` for a timeout value unsets the timeout.

Note that if you call this using a socket that has been closed already, it will
throw a [socket-closed](#socket-closed) condition.

{% highlight cl %}
;; example
(set-socket-timeouts socket 10.5 nil)
{% endhighlight %}

<a id="enable-socket"></a>
### enable-socket
{% highlight cl %}
(defun enable-socket (socket &key read write))
  => nil
{% endhighlight %}

Enable read/write monitoring on a socket. This is done automatically by
[tcp-connect](#tcp-connect) and [write-socket-data](#write-socket-data) so you
probably don't need to worry too much about when to use it. On the other hand,
[disable-socket](#disable-socket) will probably be a bit more useful.

{% highlight cl %}
;;example
(enable-socket socket :read t :write t)  ; enable read and write monitoring on this socket
{% endhighlight %}

<a id="disable-socket"></a>
### disable-socket
{% highlight cl %}
(defun disable-socket (socket &key read write))
  => nil
{% endhighlight %}

Disable read/write monitoring on a socket. This is useful if you get the data
you need from a socket, but while you're processing the data, you don't want the
socket's read timeout to fire. This will both disable the timeouts and callbacks
associated with the socket until enabled again.

<a id="socket-closed-p"></a>
### socket-closed-p
{% highlight cl %}
(defun socket-closed-p (socket))
  => t/nil
{% endhighlight %}

Determines if a socket has been closed already.

<a id="close-socket"></a>
### close-socket
{% highlight cl %}
(defun close-socket (socket))
  => nil
{% endhighlight %}

Close a socket and free its callbacks.

Note that if you call this using a socket that has been closed already, it will
throw a [socket-closed](#socket-closed) condition.

<a id="conditions"></a>
Conditions
----------
These are the conditions the TCP system can signal in [event callbacks](/cl-async/event-handling).

<a id="tcp-info"></a>
### tcp-info
_extends [connection-info](/cl-async/base#connection-info)_

Base TCP condition, says "something" happened on a TCP connection.

<a id="tcp-info-tcp-socket"></a>
##### tcp-socket
Holds the TCP socket class. Can be used to write to the socket or close it.

<a id="tcp-error"></a>
### tcp-error
_extends [connection-error](/cl-async/base#connection-error) and [tcp-info](#tcp-info)_

Describes a general error on a TCP connection. If this is triggered, the socket
will generally be closed by cl-async, and the app doesn't need to worry about
doing this. If the app *does* want to close the socket, it can do so by getting
it from the [tcp-socket](#tcp-socket) accessor on the condition and using
[close-socket](#close-socket).

<a id="tcp-eof"></a>
### tcp-eof
_extends [tcp-info](#tcp-info)_

Triggered when the peer on a TCP connection closes the socket.

<a id="tcp-timeout"></a>
### tcp-timeout
_extends [tcp-error](#tcp-error)_

Triggered when a TCP connection times out.

<a id="tcp-refused"></a>
### tcp-refused
_extends [tcp-error](#tcp-error)_

Triggered when a TCP connection is refused by the peer.

<a id="tcp-accept-error"></a>
### tcp-accept-error
_extends [tcp-error](#tcp-error)_

Passed to a [tcp-server](#tcp-server)'s `event-cb` when there is an error
accepting a client connection.

<a id="tcp-accept-error-listener"></a>
##### tcp-accept-error-listener
The libevent listener c object. Provided in case your app needs to process it in
some way.

<a id="tcp-accept-error-tcp-server"></a>
##### tcp-accept-error-tcp-server
The `tcp-server` object that the accept error happened on.

<a id="socket-closed"></a>
### socket-closed
_extends [tcp-error](#tcp-error)_

This exception is thrown by cl-async when the app tries to perform an operation
on a socket that has already been closed via [close-socket](#close-socket).

