---
title: HTTP | Documentation
layout: default
---

HTTP
====
This section detials sending and receiving data over HTTP, and also the events
and conditions one might run into while using the HTTP system.

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
- [http-info](#http-info) _condition_
- [http-error](#http-error) _condition_
- [http-timeout](#http-timeout) _condition_
- [http-refused](#http-refused) _condition_

<a id="http-client"></a>
### http-client
{% highlight cl %}
(defun http-client (uri request-cb event-cb &key (method :GET))
  => nil
{% endhighlight %}

Asynchronously communicates with an HTTP server. Allows setting the method,
headers, and body in the request which should be enough to make just about any
HTTP request. This functionality wraps the libevent HTTP client.

If a "Host" header isn't passed in, it is automatically set with whatever host
is pulled out of the `uri`. Also, any "Connection" header passed in will be
ignored...for right now, every request is sent out with `Connection: close`.
Although this is probably not going to change, I'm working on porting Drakma
over to use cl-async, which should alleviate many issues with the `http-client`.

The `timeout` arg is in seconds.

{% highlight cl %}
;; example
(http-client "http://musio.com/"
             (lambda (status headers body)
               (format t "Result: ~s~%" (list status headers (babel:octets-to-string body :encoding :utf-8))))
             (lambda (err)
               (format t "http event: ~a~%" err))
             :method :GET
             :headers '(("Accept" . "text/html"))
             :timeout 5)
{% endhighlight %}

<a id="http-client-request-cb"></a>
##### request-cb definition

{% highlight cl %}
(lambda (http-status http-headers body-byte-array) ...)
{% endhighlight %}

- `http-status` is an integer corresponding to the HTTP status code returned.
- `http-headers` is an alist, as such: '(("Content-Type" . "text/html") ...)
- `body-byte-array` is pretty self-explanatory. Convert to string w/ babel if
needed.

<a id="http-server"></a>
### http-server
{% highlight cl %}
(defun http-server (bind port request-cb event-cb))
   => http-server
{% endhighlight %}

Start a server that asynchronously processes HTTP requests. It takes data out of
the request and populates the [http-request](#http-request) with it, which is
passed into the request callback.

This function returns an `http-server` object, which allows you to close the
server via [close-http-server](#close-http-server).

Once the application is done processing the request, it must respond by calling
the [http-response](#http-response) function.

If `nil` is passed in into the `bind` arg, the server is bound to "0.0.0.0"

{% highlight cl %}
;; example
(http-server "192.168.0.1" 8090
             (lambda (req)
               (format t "Request: ~a~%" req)
               (http-response req :body "hai")))
{% endhighlight %}

<a id="http-server-request-cb"></a>
##### request-cb definition

{% highlight cl %}
(lambda (http-request) ... )
{% endhighlight %}

`http-request` is a on object of type [http-request](#http-request).

<a id="close-http-server"></a>
### close-http-server
{% highlight cl %}
(defun close-http-server (http-server))
  => nil
{% endhighlight %}

Takes an `http-server` class, created by [http-server](#http-server) and closes
the server it wraps. This can be useful if you want to shut down a HTTP server
without forcibly closing all its connections.

If the given server is already closed, this function returns without doing
anything.

Note: This function closes the listener for new HTTP client requests. Once the
current requests are finished processing, it frees all resources associated with
the server. In other words, a graceful exit.

{% highlight cl %}
;; example
(let ((server (http-server "127.0.0.1" 80 ...)))
  (signal-handler 2 (lambda (sig)
                      (declare (ignore sig))
                      ;; close the server when we get SIGINT
                      (close-http-server server))))
{% endhighlight %}

<a id="http-response"></a>
### http-response
{% highlight cl %}
(defun http-response (http-request &key (status 200) headers (body "")))
  => nil
{% endhighlight %}

This is the function called by the application using an [http-server](#http-server)
after it is done processing a request. It takes the [http-request](#http-request)
object passed into the request callback, along with some information about the
response we're sending.

{% highlight cl %}
;; example
(http-server nil 80
             (lambda (req)
               (http-response req
                              :status 200
                              :headers '(("Content-Type" . "application/json"))
                              :body "{\"name\":\"larry\"}")))
{% endhighlight %}

<a id="http-request"></a>
### http-request
This is the class passed to an HTTP request callback after a request comes in
from [http-server](#http-server). It must also be passed into
[http-response](#http-response) when the request is finished, since it holds the
pointer to the socket the request came in on.

`http-request` has a pretty-print method associated with it, so if you do
something like `(format t "~a~%" http-request)`, you'll get a nice, detailed
overview of the request (method, uri, headers, content body length (in bytes),
etc).

<a id="http-request-accessors"></a>
### http-request accessors
This details the accessors in `http-request`.

<a id="http-request-c"></a>
##### http-request-c
Pulls out the pointer to the libevent request object. This is included just in
case extra processing is needed on the request that the library doesn't handle
for you. In other words, ignore this accessor unless you know the libevent evhttp
internals and are comfortable using the libevent CFFI wrapper included with
cl-async.

<a id="http-request-method"></a>
##### http-request-method
Pull out the request method. This is a keyword, and will be one of

{% highlight cl %}
'(:GET :POST :HEAD :PUT :DELETE :OPTIONS :TRACE :CONNECT :PATCH)
{% endhighlight %}

<a id="http-request-uri"></a>
##### http-request-uri
This is the full request URI in the request. For instance, if the request was

    GET /documents/45?format=json

Then this will be the string "GET /documents/45?format=json"

<a id="http-request-resource"></a>
##### http-request-resource
This is a string of the request resource (path). A request of

    GET /mysite/index?page=4

The resource will be "/mysite/index"

<a id="http-request-querystring"></a>
##### http-request-querystring
The querystring from the request (string). Everything after (and not including)
the "?"

<a id="http-request-headers"></a>
##### http-request-headers
All headers given in the request as an alist:

{% highlight cl %}
'(("Host" . "musio.com")
  ("Accept" . "text/html"))
{% endhighlight %}

<a id="http-request-body"></a>
##### http-request-body
Get the body out of the request. Since we don't make any assumptions about the
data that's being passed around, it is a byte array. Convert it to a string in
your app via `babel:octets-to-string` if needed.

It's important to note that at this time, multipart form data, posted files, etc
are *not* decoded by `http-server`. As such, it is currently up to your app to
do this. *This may change in the future* and if so, I will do my best to make the
change backwards compatible.

<a id="conditions"></a>
Conditions
----------
These are the conditions the HTTP system can signal in [event callbacks](/cl-async/event-handling).

<a id="http-info"></a>
### http-info
_extends [connection-info](/cl-async/base#connection-info)_

Base HTTP condition.

<a id="http-error"></a>
### http-error
_extends [connection-error](/cl-async/base#connection-error) and [http-info](#http-info)_

Base HTTP error condition.

<a id="http-timeout"></a>
### http-timeout
_extends [http-error](#http-error)_

Triggered when an HTTP connection times out.

<a id="http-refused"></a>
### http-refused
_extends [http-error](#http-error)_

Triggered when an HTTP connection is refused by the peer.

