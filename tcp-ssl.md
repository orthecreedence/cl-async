---
title: SSL over TCP | Documentation
layout: documentation
---

SSL over TCP
============
Libevent's [bufferevents](http://www.wangafu.net/~nickm/libevent-book/Ref6a_advanced_bufferevents.html)
provide an easy way to wrap sockets in SSL. Cl-async provides a very simple
interface for setting this up.

Note that SSL in cl-async must be specifically loaded via the package
`cl-async-ssl`, otherwise it won't be available. This is because it must load a
separate library (`libevent_openssl`) which may not be available on the system
by default. `cl-async-ssl` depends on the `cl-libevent2-ssl` package, present in
the [latest versions of the libevent2 bindings](https://github.com/orthecreedence/cl-libevent2).

- [ssl-socket](#ssl-socket) _class_
- [wrap-in-ssl](#wrap-in-ssl) _function_
- [tcp-ssl-error](#tcp-ssl-error) _condition_

<a id="ssl-socket"></a>
### ssl-socket
_extends [socket](/cl-async/tcp#socket)_

This class is much like the [socket](/cl-async/tcp#socket) class, except that it
houses a socket that has been wrapped in an SSL filter. It exposes no public
accessors, except for the ones it inherits from `socket`.

It can be passed to any cl-async function that takes a `socket` argument.

<a id="wrap-in-ssl"></a>
### wrap-in-ssl
{% highlight cl %}
(defun wrap-in-ssl (socket/stream &key certificate key password
                                       (method 'cl+ssl::ssl-v23-client-method)
                                       close-cb))
  => socket/stream
{% endhighlight %}

_Note: `wrap-in-ssl` has only been testing with outgoing sockets, not servers.
There is more work to do before server SSL sockets are supported._

Wraps a socket or stream in SSL. The callbacks and timeouts on the original
socket/stream are reattached to the new SSL socket/stream, meaning you can
convert a non-SSL socket to SSL fairly simply. It's important to note that this
function does *not* replace the socket/stream passed to it, it returns a *new*
socket if a socket was passed in, and a *new* stream if a stream was passed in.

It's a bad idea to use the original socket/stream after wrapping it in SSL. Most
likely your writes will trigger SSL errors and your reads will be blank. So make
sure your app updates any references to the old socket/stream with the one
returned from `wrap-in-ssl`.

This function works by setting up an SSL filtering bufferevent that wraps the
one attached to the given socket/stream. Although this is handled transparently
by libevent, it still requires that an SSL context be set up, which
`wrap-in-ssl` uses [cl+ssl](http://common-lisp.net/project/cl-plus-ssl/) for.

The `close-cb` function passed in will be called when the SSL socket is closed.

{% highlight cl %}
;; example
;; init a socket, send nil data (so it just connects)
(let* ((socket (tcp-connect "www.google.com" 443
                            (lambda (sock data)
                              (declare (ignore sock))
                              (format t "GOT: ~a~%" (if (stringp data) data (babel:octets-to-string data))))
                            (lambda (ev)
                              (format t "event: ~a~%" ev))
                            :read-timeout 3))
       ;; now wrap the normal socket in SSL, save the new socket
       (socket-ssl (wrap-in-ssl socket)))
  ;; send out the request on the SSL socket
  (write-socket-data socket-ssl (format nil "GET /~c~c" #\return #\newline)))
{% endhighlight %}

<a id="wrap-in-ssl-close-cb"></a>
##### close-cb definition
`close-cb` is a lambda with no arguments.
{% highlight cl %}
(lambda () ...)
{% endhighlight %}

<a id="tcp-ssl-error"></a>
### tcp-ssl-error
_extends [tcp-error](/cl-async/tcp#tcp-error)_

Triggered when an error happens while communicating over an SSL socket.
