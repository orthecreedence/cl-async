cl-async - Asynchronous operations for Common Lisp
==================================================
Cl-async is a library for general purpose, non-blocking programming in Common
Lisp. Cl-async uses [libuv](http://docs.libuv.org/en/v1.x/) as the backend,
which is a fast, stable, portable library for asynchronous IO (used as the
backend library in Node.js).

The main goal is to provide an experience that makes general asynchronous 
programming in lisp natural, and to also provide a number of
[drivers](http://orthecreedence.github.io/cl-async/drivers) on top of cl-async.

__NOTE:__ cl-async uses the v1.x branch of libuv, so make sure to grab that
version of it (not the v0.10.x branch).

### [Documentation](http://orthecreedence.github.io/cl-async/documentation)
Please see the [cl-async website](http://orthecreedence.github.io/cl-async) for
full documentation, examples, etc.

Quick links:

- [Documentation](http://orthecreedence.github.io/cl-async/documentation)
  - [Base system](http://orthecreedence.github.io/cl-async/base)
  - [Timers](http://orthecreedence.github.io/cl-async/timers)
  - [Signal handling](http://orthecreedence.github.io/cl-async/signal-handling)
  - [DNS](http://orthecreedence.github.io/cl-async/dns)
  - [TCP](http://orthecreedence.github.io/cl-async/tcp)
  - [TCP stream](http://orthecreedence.github.io/cl-async/tcp-stream)
  - [TCP SSL](http://orthecreedence.github.io/cl-async/tcp-ssl)
  - [Pollers](http://orthecreedence.github.io/cl-async/pollers)
  - [Idlers](http://orthecreedence.github.io/cl-async/idlers)
  - [Notifiers](http://orthecreedence.github.io/cl-async/notifiers)
  - [Futures](http://orthecreedence.github.io/cl-async/future)
  - [Threading](http://orthecreedence.github.io/cl-async/threading)
  - [Stats](http://orthecreedence.github.io/cl-async/stats)
  - [Event callbacks and error handling](http://orthecreedence.github.io/cl-async/event-handling)
- [Examples](http://orthecreedence.github.io/cl-async/examples)
- [Benchmarks](http://orthecreedence.github.io/cl-async/benchmarks)
- [Implementation notes](http://orthecreedence.github.io/cl-async/implementation-notes)
- [Drivers](http://orthecreedence.github.io/cl-async/drivers)

### Install
```lisp
(ql:quickload :cl-async)
```

Please be aware that until cl-async v0.6.x is in quicklisp, you might want to
git clone the master branch into `quicklisp/local-projects/`.

### Tests
There is a fairly complete suite of tests in the `cl-async-test` package:

```common-lisp
(ql:quickload :cl-async-test)
(cl-async-test:run-tests)
(cl-async-test:run-tests :ssl t)
(cl-async-test:run-tests :threading t)
```

### License
As always, my code is MIT licensed. Do whatever the hell you want with it. Enjoy!

