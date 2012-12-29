---
title: Deprecating HTTP
layout: post
---
After much careful thought and consideration, I've decided that the
cl-async HTTP implementation does not meet the criteria for its continued
maintenance, and is now officially deprecated. The functions will still be
loaded into the cl-async package for the forseeable future, so any existing
applications built off of them (none...) will not break. That said, any
bugfixes or feature requests will most likely be ignored from here on out,
unless you wish to fork and issue a pull request (if you do this and change the
API, doc updates are also encouraged).

There are a few reasons for this, which I will, of course, go into now:

- I never trusted the libevent HTTP server implementation. Unlike the libevent
socket API (bufferevents), it offers very little customization and moldability
and feels like a slapped together afterthought. Maybe I'm being paranoid and
it's really stable and wonderful, but I tend to go with my gut. The more I think
about features I'd want to build on top of the HTTP server, the more I realize
that I'll just be reinventing the wheel (and the more I realize that the HTTP
server is really just a glorified header processor that prohibits streaming of
chunked content).
- The HTTP client feels the same way...not as much thought was put into it as
the rest of the libevent library, and its feature set is small.
- There is now a [drakma-async](https://github.com/orthecreedence/drakma-async)
library...an async port of the [drakma](http://weitz.de/drakma/) library that
everyone loves. This makes the HTTP client in cl-async completely and utterly
redundant.
- There are a few bugs/issues in the libevent HTTP implementation that would
require a great deal of workarounds in C land to solve, and I just don't care
enough to get it done.

I'd much rather spend my time building on top of a solid base of well-thought
out TCP libraries than holding together a feature-poor, shaky HTTP
implementation which can be replaced by better applications with a bit of time.

I'm going to be spending my free time over the next few weeks porting
[hunchentoot](http://weitz.de/hunchentoot/) to be async. I've started on some
initial conversion code (which is going well thanks to hunchentoot's use of
methods) and it *looks* like it might not be too bad of a process. I'll keep
everyone updated as I make progress.

Bye, HTTP.
