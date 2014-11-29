---
title: Upgrading to v0.6.x | Documentation
layout: documentation
---

Upgrading to v0.6.x
===
cl-async has swapped out its backend library from libevent to libuv. For more
information on why, [see the related post](/cl-async/2014/11/29/cl-async-now-with-libuv.html).

The goal of the v0.6.x upgrade was to port cl-async over to libuv with the
*least amount of API changes as possible*.

## Library version
cl-async v0.6.x is built against libuv-1.0.0-rc2. This means you must use
cl-async with libuv >= v1.0.0-rc2 or it will not work.

## Breaking changes
Note that we're only listing breaking changes here.

### Base system
- `start-event-loop`/`with-event-loop` no longer have `fatal-cb` or `logger-cb`
keywords. Both features have been completely removed.

### Events
- `watch-fd` is gone and has been replaced with [pollers](/cl-async/pollers).

### TCP
- `init-tcp-socket` no longer takes an `fd` keyword. The concept of wrapping a
cl-async socket around an existing fd no longer exists. Instead use the new
[poller object](/cl-async/pollers), which allows you to fire a callback whenever
an fd can be read from/written to (although it does *not* provide an abstraction
for writing to an fd which is no longer supported).
- `enable-socket`/`disable-socket` are no longer implemented and will throw
errors if used. libuv doesn't have the concept of disabling reading/writing on
a socket, and I'm not sure if this was ever that useful to begin with. If your
app relies heavily on this feature, it can most likely be built back into
cl-async (just [open an issue](https://github.com/orthecreedence/cl-async/issues)).

### Threading support
cl-async still has threading support, but it is now extremely simplified. Be
sure to familiarize yourself with [notifiers](/cl-async/notifiers), which are
the new threading abstraction.

- `enable-threading-support` is gone and has no replacement. Threading support
is always on now.
- `with-threading-context` is gone and has no replacement. You can no longer add
or change events to an event loop from another thread (but see [notifiers](/cl-async/notifiers)).

## Having trouble?

I may have missed some breaking changes or explained some things wrong. If you
are having a problem getting your app running on cl-async v0.6.x, please let me
know by [opening a github issue](https://github.com/orthecreedence/cl-async/issues)
and venting your frustrations. I'm here to help!

