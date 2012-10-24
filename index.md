---
title: Home
layout: default
---

Asynchronous operations for Common Lisp
=======================================
Cl-async is a library for general purpose, non-blocking programming library for
Common Lisp. I tried other non-blocking libraries, but they either require a
large number of dependencies, aren't portable, or are too specialized to one task.
Cl-async uses [Libevent2](http://libevent.org/) as the async backend, which is
a fast, stable, portable library for asynchronous IO (see my
[notes on choosing Libevent](/cl-async/implementation-notes#libevent)).

The main goal is to provide an experience that makes general asynchronous 
programming in lisp a delight instead of a chore. Portability and ease of use
are favored over raw speed.

*This library's current status is BETA*

<a id="documentation"></a>
### Documentation
Please see the [documentation page](/cl-async/documentation). Also, for example
usage see the [examples section](/cl-async/examples).

<a id="todo"></a>
### TODO
Please see the [Issues list](https://github.com/orthecreedence/cl-async/issues)
for the complete list of what needs to be done.

