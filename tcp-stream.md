---
title: TCP stream | Documentation
layout: default
---

TCP stream
==========
A TCP stream wraps a [tcp-socket](/cl-async/tcp#socket), which is returned by
[tcp-connect](/cl-async/tcp#tcp-connect):

{% highlight cl %}
(let ((socket (tcp-connect "google.com" 80 ... :dont-drain-read-buffer t)))
  (make-instance 'async-io-stream :socket socket))
{% endhighlight %}

`tcp-connect` makes this a bit more convenient as well:

{% highlight cl %}
(tcp-connect "google.com" 80 ... :stream t)
{% endhighlight %}

The stream we just created is a normal lisp stream which can be operated on via
`close`, `read-sequence`, `write-sequence`, etc. One caveat is that the stream
does not block when pulling data from it, so reading from the stream should be
triggered by the `read-cb` given to `tcp-connect`.

See the [notes on buffer draining](#tcp-stream-notes) section for an explanation
of the `:dont-drain-read-buffer` parameter.

<a id="tcp-stream-classes"></a>
Classes
-------
<a id="async-output-stream"></a>
### async-output-stream
The base output stream, can only be used to send data on a socket.

<a id="async-input-stream"></a>
### async-input-stream
The base input stream, can only be used to recieve data on a socket.

<a id="async-io-stream"></a>
### async-io-stream
Extends both `async-output-stream` and `async-input-stream`, and allows both
reading and writing on the underlying socket.

<a id="tcp-stream-notes"></a>
Buffer draining notes
---------------------
Streams work by sucking the data out of libevent's bufferevent for the given
socket. Normally, when a `read-cb` fires on a socket, cl-async will drain the
data received out of the bufferevent and put it into a byte array it passes to
the `read-cb`.

In the case of streams, the stream drains this data directly, so you have to
tell cl-async to *not* drain the read bufferevent, which will still call
`read-cb`, but only to notify you that data is ready. This is what
`:dont-drain-read-buffer` does for you. Note that if you specify `:stream t`
then `dont-drain-read-buffer` is assumed to be `T` unless explicitely stated
otherwise.

When the `read-cb` fires and `dont-drain-read-buffer` is `T`, the `data` param
will always be the stream and the socket data can be read via `read-sequence` or
similar. Note that the socket arg (the first arg) to the `read-cb` is still a
socket.

<a id="tcp-stream-examples"></a>
Example usage
-------------
{% highlight cl %}
(tcp-connect "musio.com" 80
  (lambda (sock stream)
    (let* ((seq (make-array 4096 :element-type '(unsigned-byte 8)))
           (num-bytes (read-sequence seq stream :end 4096)))
      (format t "~a" (babel:octets-to-string (subseq seq 0 num-bytes)))))
  (lambda (ev) (format t "ev: ~a~%" ev))
  :data (format nil "GET /~C~C" #\return #\newline)
  :stream t
  :read-timeout 5)
{% endhighlight %}

Note that you can write the socket returned by `(tcp-connect ... :stream t)` with
`write-sequence` like any other stream.

