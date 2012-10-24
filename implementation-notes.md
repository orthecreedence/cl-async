---
title: Implementation notes
layout: default
---

Implementation notes
====================
<a id="libevent"></a>
### Libevent
Libevent was chosen for a few reasons:

- It provides a socket API. The IOLib library was too undocumented for me
  to figure out. Plus things like delayed functions/timers were not clear to me.
- It wraps the socket implementation and buffering in a [simple and wonderful
  API](http://www.wangafu.net/~nickm/libevent-book/Ref6_bufferevent.html).
- It was very easy to generate bindings for and wrap in CFFI.
- It is portable to Windows, and with a bit of work, I'm assuming cl-async
  could be programmed to use the IOCP parts of libevent (I think for now it uses
  select()).
- It comes with asynchronous HTTP client/server implementations. These are not
  trivial, and if libevent makes it easier to have an asynchronous CL webserver
  or client, then hell let's use it.

The bindings for libevent are auto-generated. I'm not proud of the bindings
themselves, but because I planned to completely wrap them all along, didn't put
too much work into making them pretty and useful. They will most likely stay
as-is (and undocumented).

<a id="http-server"></a>
### HTTP server
The [http-server](/cl-async/http#http-server) is a simple way to get a quick HTTP interface
for your app. However, as someone who does a lot of ops as well as dev, I must
warn you that **I would not trust this to be public-facing**. This is not
because I am a terrible programmer, but because I don't think libevent's HTTP
implementation takes into account a lot of things that other HTTP servers have
been battle tested with.

In other words, put [HAProxy](http://haproxy.1wt.eu/) or [NginX](http://nginx.org/)
(or similar) in front of it. Let someone else bear the brunt of dealing with the
security flaws of the open web so you can focus on building a solid application.

<a id="internals"></a>
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

<a id="todo"></a>
### TODO
Please see the [Issues list](https://github.com/orthecreedence/cl-async/issues)
for the complete list of what needs to be done.


