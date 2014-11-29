---
title: SSL over TCP | Documentation
layout: documentation
---

SSL over TCP
============
Cl-async provides a way to wrap your sockets in SSL.

Note that SSL in cl-async must be specifically loaded via the package
`cl-async-ssl`, otherwise it won't be available.

- [ssl-socket](#ssl-socket) _class_
- [tcp-ssl-connect](#tcp-ssl-connect) _function_
- [init-tcp-ssl-socket](#init-tcp-ssl-socket) _function_
- [tcp-ssl-server](#tcp-ssl-server-class) _class_
- [tcp-ssl-server](#tcp-ssl-server) _function_
- [tcp-ssl-error](#tcp-ssl-error) _condition_

<a id="ssl-socket"></a>
### ssl-socket
_extends [socket](/cl-async/tcp#socket)_

This class is much like the [socket](/cl-async/tcp#socket) class, except that it
houses a socket that has been wrapped in an SSL filter. It exposes no public
accessors, except for the ones it inherits from `socket`.

It can be passed to any cl-async function that takes a `socket` argument.

<a id="tcp-ssl-connect"></a>
### tcp-ssl-connect
{% highlight cl %}
(defun tcp-ssl-connect (host port read-cb event-cb
                        &key data stream
                             connect-cb write-cb
                             (read-timeout -1) (write-timeout -1)
                             (dont-drain-read-buffer nil dont-drain-read-buffer-supplied-p)
                             ssl-ctx ssl-options ciphers)
  => ssl-socket/stream
{% endhighlight %}

Much like [tcp-connect](/cl-async/tcp#tcp-connect), `tcp-connect-ssl` opens and
connects an async socket, but wrapped in the SSL protocol. In fact just about
every argument is the same for `tcp-connect` as it is for `tcp-connect-ssl`,
except for some special options that `tcp-connect-ssl` takes to help set up the
SSL context/object.

`tcp-ssl-connect` returns an [ssl-socket](#ssl-socket) object, which is an
extension of the [socket](/cl-async/tcp#socket) class, and can be used with all
the same functions/methods.

`:ssl-ctx` is an (optional) c-pointer to an SSL context object that you have
initialized outside of cl-async. If a context is not specified, one is created
and attached to the resulting socket.

`:ssl-options` is a list of options that allows us to set options on the created
CTX. Note that if `:ssl-ctx` is supplied, `:ssl-options` is ignored. Options can
be as follows:

{% highlight cl %}
+ssl-op-no-query-mtu+
+ssl-op-cookie-exchange+
+ssl-op-no-ticket+
+ssl-op-cisco-anyconnect+
+ssl-op-no-session-resumption-on-renegotiation+
+ssl-op-no-compression+
+ssl-op-allow-unsafe-legacy-renegotiation+
+ssl-op-single-ecdh-use+
+ssl-op-single-dh-use+
+ssl-op-ephemeral-rsa+
+ssl-op-cipher-server-preference+
+ssl-op-tls-rollback-bug+
+ssl-op-no-sslv2+
+ssl-op-no-sslv3+
+ssl-op-no-tlsv1+
+ssl-op-no-tlsv1-2+
+ssl-op-no-tlsv1-1+
{% endhighlight %}

`:ciphers` is a *string* cipher list you want the SSL object to be initialized
with. The default is
`"HIGH:!RC4:!MD5:!aNULL:!EDH:!EXP:+ECDHE-RSA-AES128-SHA256:+3DES"`.
Note that if `:ssl-ctx` is supplied, `:ciphers` is ignored.

{% highlight cl %}
;; simple SSL socket example
(tcp-ssl-connect "www.google.com" 443
                 (lambda (socket data)
                   (declare (ignore socket))
                   (format t "GOT: ~a~%" (babel:octets-to-string data)))
                 (lambda (ev)
                   (format t "EV: ~a~%" ev))
                 :read-timeout 3
                 :data (format nil "GET /~c~c" #\return #\newline))
{% endhighlight %}

<a id="tcp-ssl-connect-read-cb"></a>
##### read-cb definition (default)

{% highlight cl %}
(lambda (socket byte-array) ...)
{% endhighlight %}

<a id="tcp-ssl-connect-read-cb-stream"></a>
##### read-cb definition (when tcp-ssl-connect's :stream is t)

{% highlight cl %}
(lambda (socket stream) ...)
{% endhighlight %}

Note that in this case, `stream` replaces the data byte array's position. Also,
when calling `:stream t` in `tcp-ssl-connect`, the read buffer for the socket is
not drained and is only done so by [reading from the stream](/cl-async/tcp-stream).

`stream` is always the same object returned from `tcp-ssl-connect` with
`:stream t`.  It wraps the [ssl-socket](#ssl-socket) object.

<a id="tcp-ssl-connect-connect-cb"></a>
##### connect-cb definition
{% highlight cl %}
(lambda (socket) ...)
{% endhighlight %}

The `connect-cb` will be fired when the connection from `tcp-ssl-connect` has been
established. Since sending data over the socket is somewhat transparent (either
via `:data` or [write-socket-data](#write-socket-data)), you don't really have
to know when a socket is ready to be written to. In some instances though, it
may be useful to know when the connection has been established, which is why
`:connect-cb` is exposed.

<a id="tcp-ssl-connect-write-cb"></a>
##### write-cb definition

{% highlight cl %}
(lambda (socket) ...)
{% endhighlight %}

The `write-cb` will be called after data written to the socket's buffer is
flushed out to the socket. If you want to send a command to a server and
immediately disconnect once you know the data was sent, you could close the
connection in your `write-cb`.

<a id="tcp-ssl-server-class"></a>
### tcp-ssl-server (class)
_extends [tcp-server](/cl-async/tcp#tcp-server-class)_

This is an opaque class which is returned by the function [tcp-ssl-server](#tcp-server)
to allow [closing the server](/cl-async/tcp#close-tcp-server) and allowing for
future expansion of the server's abilities. It has no public accessors.

<a id="tcp-ssl-server"></a>
### tcp-ssl-server
{% highlight cl %}
(defun tcp-ssl-server (bind-address port read-cb event-cb
                       &key connect-cb (backlog -1) stream
                            ssl-ctx
                            certificate key (keytype :pem) ssl-options ciphers)
  => tcp-ssl-server
{% endhighlight %}

This function works by calling [tcp-server](/cl-async/tcp#tcp-server) to bind to
the given address/port. It then replaces the callback that is fired when a new
connection is accepted with one that wraps the incoming socket in the SSL
protocol. In other words, `tcp-ssl-server` functions almost exactly like
[tcp-server](/cl-async/tcp#tcp-server) except the connecting client must speak
SSL.

It takes `:certificate`, `:key`, `:keytype` keyword arguments which are
used for loading a certificate and a private key. The
certificate file can be a chain file, so if you have a Certificate Authority
you want to use, you can just dump it in the same file (much like how [NginX
handles SSL](http://nginx.org/en/docs/http/configuring_https_servers.html#chains)).

Like [tcp-server](/cl-async/tcp#tcp-server), `tcp-ssl-server` can be closed
gracefully by calling the [close-tcp-server](/cl-async/tcp#close-tcp-server)
method on the [tcp-ssl-server](/cl-async/tcp-ssl#tcp-ssl-server-class) object it returns.

`:ssl-ctx` is an (optional) c-pointer to an SSL context object that you have
initialized outside of cl-async. If a context is not specified, one is created
and attached to the resulting server.

`:ssl-options` is a list of options that allows us to set options on the created
CTX. Note that if `:ssl-ctx` is supplied, `:ssl-options` is ignored. Options can
be as follows:

{% highlight cl %}
+ssl-op-no-query-mtu+
+ssl-op-cookie-exchange+
+ssl-op-no-ticket+
+ssl-op-cisco-anyconnect+
+ssl-op-no-session-resumption-on-renegotiation+
+ssl-op-no-compression+
+ssl-op-allow-unsafe-legacy-renegotiation+
+ssl-op-single-ecdh-use+
+ssl-op-single-dh-use+
+ssl-op-ephemeral-rsa+
+ssl-op-cipher-server-preference+
+ssl-op-tls-rollback-bug+
+ssl-op-no-sslv2+
+ssl-op-no-sslv3+
+ssl-op-no-tlsv1+
+ssl-op-no-tlsv1-2+
+ssl-op-no-tlsv1-1+
{% endhighlight %}

`:ciphers` is a *string* cipher list you want the SSL object to be initialized
with. The default is
`"HIGH:!RC4:!MD5:!aNULL:!EDH:!EXP:+ECDHE-RSA-AES128-SHA256:+3DES"`.
Note that if `:ssl-ctx` is supplied, `:ciphers` is ignored.

{% highlight cl %}
;; example
(tcp-ssl-server "127.0.0.1" 443
                (lambda (socket data)
                  (format t "data: ~a~%" data)
                  (write-socket-data socket "THIS IS A SECURE LINE!"
                                     :write-cb (lambda (socket)
                                                 (close-socket socket))))
                (lambda (ev)
                  (format t "SSL ev: ~a~%" ev)))
{% endhighlight %}

<a id="tcp-ssl-server-read-cb"></a>
##### read-cb definition (default)

{% highlight cl %}
(lambda (socket byte-array) ...)
{% endhighlight %}

<a id="tcp-ssl-server-read-cb-stream"></a>
##### read-cb definition (when tcp-ssl-server is called with :stream t)

{% highlight cl %}
(lambda (socket stream) ...)
{% endhighlight %}

Note that in this case, `stream` replaces the data byte array's position. Also,
when calling `:stream t` in `tcp-stream`, the read buffer for the connecting
socket is not drained and is only done so by [reading from the stream](/cl-async/tcp-stream).

<a id="tcp-ssl-server-connect-cb"></a>
##### connect-cb definition

{% highlight cl %}
(lambda (socket) ...)
{% endhighlight %}

Called when a client connects (but not necessarily when it has sent data). If
present, is *always* called before the [read-cb](#tcp-ssl-server-read-cb).

<a id="conditions"></a>
Conditions
----------
These are the conditions the TCP SSL system can signal in [event callbacks](/cl-async/event-handling).

<a id="tcp-ssl-error"></a>
### tcp-ssl-error
_extends [tcp-error](/cl-async/tcp#tcp-error)_

Triggered when an error happens while communicating over an SSL socket.

