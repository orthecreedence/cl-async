---
title: Working on HTTP client
layout: post
---

I've been working pretty hard on an HTTP client in cl-async that supports
streaming. The current implementation of the HTTP client in Libevent2 as it is
only supports simple requests:

 1. Init request, set in headers (must be predefined)
 2. Send in body content, must be known and fully loaded into memory
 3. Fire off request, trigger callback when response has been parsed

This works fine for simpler things, but I'm currently porting Drakma to use
cl-async, and the easiest way to do this is to support streaming. I built a
stream implementation on top of the cl-async `socket` type, which seems to work
great (check out the `stream` branch in cl-async). The problem is that Drakma
has a hard time knowing when the response is fully loaded. I have three options:

 1. Reprogram the streaming implementation in Drakma (ie Chunga) to be async.
 This would require a lot of work and state management which, honestly, seems
 like too much work.
 2. Reprogram Drakma to not use streams. This severely limits the number of
 features that Drakma implements, some of which may be crucial to users of the
 library. I don't really find this method acceptable, although it seems to be 
 the easiest to implement.
 3. Hack libevent to support streaming in the HTTP client such that the entire
 HTTP request is streamed in (like how Drakma currently does it). Once a
 response comes through, libevent will parse it and fire the callback when it's
 ready to be parsed, at which point Drakma can read from the stream.

I've decided to go with option #3. While fairly dangerous in that I have to
use a lot of "private" functions in the libevent `evhttp` implementation, it
provides the most features for users of the library.

There are a lot of hurdles to this implementation though. Not only does
libevent not export some of the functions needed to do this, it actually
defines them as static, making it literally impossible to call them from lisp.
This means that I'm re-implementing a lot of existing C code in lisp/CFFI that
could easily be avoided with a simple `foreign-funcall` if I had the option.

So, things right now are in an unknown state. I'm working my best to get a
working prototype out of the project. I think if worse comes to worse, I might
circumvent libevent's HTTP client altogether, and implement an extremely simple
HTTP parser that basically takes an array of HTTP data and returns `t` if the
response has finished downloading, and `nil` if it's still getting data. Then
once it's finished, I can just stream the data into Drakma and it won't even
know that the data loaded async.

Check out the latest version of the [http-stream implementation](https://github.com/orthecreedence/cl-async/blob/future%2Bstream/http-stream.lisp)
on the [future+stream branch](https://github.com/orthecreedence/cl-async/tree/future+stream)
of cl-async. Please note that `http-stream` is completely broken! It can stream
data out, but doesn't trigger any complete callbacks yet. It's broken.
