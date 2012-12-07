cl-async - Asynchronous operations for Common Lisp
==================================================
Cl-async is a library for general purpose, non-blocking programming in
Common Lisp. I tried other non-blocking libraries, but they either require a
large number of dependencies, aren't portable, or are too specialized to one task.
Cl-async uses [Libevent2](http://libevent.org/) as the async backend, which is
a fast, stable, portable library for asynchronous IO (see my [notes on choosing Libevent](http://orthecreedence.github.com/cl-async/implementation-notes#libevent)).

The main goal is to provide an experience that makes general asynchronous 
programming in lisp natural, and to also provide a number of
[drivers](http://orthecreedence.github.com/cl-async/drivers) on top of cl-async.

*This library's current status is BETA*

### [Documentation](http://orthecreedence.github.com/cl-async/documentation)
Please see the [cl-async website](http://orthecreedence.github.com/cl-async) for
full documentation, examples, etc.

Quick links:

- [Documentation](http://orthecreedence.github.com/cl-async/documentation)
  - [Base system](http://orthecreedence.github.com/cl-async/base)
  - [Timers](http://orthecreedence.github.com/cl-async/timers)
  - [Signal handling](http://orthecreedence.github.com/cl-async/signal-handling)
  - [DNS](http://orthecreedence.github.com/cl-async/dns)
  - [TCP](http://orthecreedence.github.com/cl-async/tcp)
  - [TCP stream](http://orthecreedence.github.com/cl-async/tcp-stream)
  - [TCP SSL](http://orthecreedence.github.com/cl-async/tcp-ssl)
  - [HTTP](http://orthecreedence.github.com/cl-async/http)
  - [Futures](http://orthecreedence.github.com/cl-async/future)
  - [Stats](http://orthecreedence.github.com/cl-async/stats)
  - [Event callbacks and error handling](http://orthecreedence.github.com/cl-async/event-handling)
- [Examples](http://orthecreedence.github.com/cl-async/examples)
- [Benchmarks](http://orthecreedence.github.com/cl-async/benchmarks)
- [Implementation notes](http://orthecreedence.github.com/cl-async/implementation-notes)
- [Drivers](http://orthecreedence.github.com/cl-async/drivers)

### Quicklisp
This library is now fully in quicklisp, along with its sister bindings,
[cl-libevent2](/orthecreedence/cl-libevent2). If you want to keep up with the
most recent changes (recommended, as many bugfixes happen on master), download
from git, otherwise feel free to use the quicklisp release. Also, if using
quicklisp, be familiar with the [closed issues list](/orthecreedence/cl-async/issues?state=closed).
An issue you're having may have already been fixed =].

### Tests
There is a fairly complete suite of tests in the `cl-async-test` package:

```common-lisp
(ql:quickload :cl-async-test)

(cl-async-test:run-tests)
```

As bugs happen (or as I remember old bugs and encounter new bugs) I'll be adding
more tests.

### TODO
See the [TODO list](https://github.com/orthecreedence/cl-async/issues).

### License
As always, my code is MIT licenced. Do whatever the hell you want with it. Enjoy!


