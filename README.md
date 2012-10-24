cl-async - Asynchronous operations for Common Lisp
==================================================
Cl-async is a library for general purpose, non-blocking programming library for
Common Lisp. I tried other non-blocking libraries, but they either require a
large number of dependencies, aren't portable, or are too specialized to one task.
Cl-async uses [Libevent2](http://libevent.org/) as the async backend, which is
a fast, stable, portable library for asynchronous IO (see my [notes on choosing Libevent](#libevent)).

The main goal is to provide an experience that makes general asynchronous 
programming in lisp a delight instead of a chore. Portability and ease of use
are favored over raw speed.

*This library's current status is BETA*

### Documentation
Please see the [cl-async website](http://orthecreedence.github.com/cl-async) for
full documentation, examples, etc.

Quick links:

- [Documentation](http://orthecreedence.github.com/cl-async/documentation)
  - API Documentation
    - [Base system](http://orthecreedence.github.com/cl-async/base)<br/>
    - [Timers](http://orthecreedence.github.com/cl-async/timers)<br/>
    - [Signal handling](http://orthecreedence.github.com/cl-async/signal-handling)<br/>
    - [DNS](http://orthecreedence.github.com/cl-async/dns)<br/>
    - [TCP](http://orthecreedence.github.com/cl-async/tcp)<br/>
    - [HTTP](http://orthecreedence.github.com/cl-async/http)<br/>
    - [Stats](http://orthecreedence.github.com/cl-async/stats)<br/>
  - [Event callbacks and error handling](http://orthecreedence.github.com/cl-async/event-handling)
- [Examples](http://orthecreedence.github.com/cl-async/examples)
- [Benchmarks](http://orthecreedence.github.com/cl-async/benchmarks)
- [Implementation notes](http://orthecreedence.github.com/cl-async/implementation-notes)
- [Drivers](http://orthecreedence.github.com/cl-async/drivers)

### Quicklisp
Although this library is quicklisp-loadable, I _strongly urge you_ to use the
master branch of this repo until otherwise noted. A lot has been changed/fixed
since it was included, and I suspect this trend will continue for at least a few
more weeks. If you do use the quicklisp version, please check the
[closed issues list](https://github.com/orthecreedence/cl-async/issues?state=closed)
before complaining about something being broken.

### TODO
See the [TODO list](https://github.com/orthecreedence/cl-async/issues).

### Notes
Note that while the current style of this library is
[CPS](http://en.wikipedia.org/wiki/Continuation-passing_style), in the future a
syntactic layer may be built on top of it using [cl-cont](http://common-lisp.net/project/cl-cont/)
or futures. For now, you're stuck with nested callback HELL __>:)__

### License
As always, my code is MIT licenced. Do whatever the hell you want with it. Enjoy!


