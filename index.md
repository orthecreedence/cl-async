---
title: Home
layout: default
---

cl-async - Asynchronous operations for Common Lisp
==================================================
Cl-async is a library for general purpose, non-blocking programming library for
Common Lisp. I tried other non-blocking libraries, but they either require a
large number of dependencies, aren't portable, or are too specialized to one task.
Cl-async uses [Libevent2](http://libevent.org/) as the async backend, which is
a fast, stable, portable library for asynchronous IO (see my
[notes on choosing Libevent](/cl-async/implementation-notes#libevent)).

The main goal is to provide an experience that makes general asynchronous 
programming in lisp a delight instead of a chore. Portability and ease of use
are favored over raw speed.

[__Full documentation__](/cl-async/documentation)

__Quicklisp note:__ Although this library is quicklisp-loadable, I _strongly
urge you_ to use the master branch of this repo until otherwise noted. A lot has
been changed/fixed since it was included, and I suspect this trend will continue
for at least a few more weeks. If you do use the quicklisp version, please check
the [closed issues list](https://github.com/orthecreedence/cl-async/issues?state=closed)
before complaining about something being broken.

Also note that while the current style of this library is
[CPS](http://en.wikipedia.org/wiki/Continuation-passing_style), in the future a
syntactic layer may be built on top of it using [cl-cont](http://common-lisp.net/project/cl-cont/)
or futures. For now, you're stuck with nested callback HELL __>:)__

*This library's current status is BETA*

<a id="todo"></a>
### cl-async TODO
Please see the [Issues list](https://github.com/orthecreedence/cl-async/issues)
for the complete list of what needs to be done.

