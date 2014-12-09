---
title: cl-async - now using libuv
layout: post
---
Cl-async has swapped its backend from libevent to libuv. Please see the
[v0.6.x upgrade guide](/cl-async/upgrade-v0.6) if you have no already.

There are a number of reasons for swapping out libevent:

- Libuv is more actively maintained.
- Libuv provides more abstractions, including filesystem IO and UDP sockets,
which cl-async did not have access to before (however filesystem and UDP are
not currently implemented in cl-async).
- Libuv fixes a number of bugs and warts that libevent has had for quite a
while now.
- Libuv uses IOCP on windows, where libevent uses `select()` (this means that
cl-async will now run a lot faster on windows).

Overall, we had enough issues with libevent and got enough push from the
cl-async community to rewrite the backend to use libuv. Although this upgrade
will be a lot better for cl-async in the future, it may cause some pain for
people in the meantime. Please feel free to [open an issue](https://github.com/orthecreedence/cl-async/issues)
on our github or email me directly at orthecreedence [at] gmail if you run into
problems or have questions.

## Improvements

There are a number of great things this rewrite has given cl-async.

First an foremost, faster TCP IO. We swapped out the manual byte-by-byte copies
between C &gt;--&lt; lisp, instead using [static-vectors](https://github.com/sionescu/static-vectors)
for transfering data between C and lisp. On top of this, all socket buffering
is done using [fast-io](https://github.com/rpav/fast-io) which is another speed
boost on top of static-vectors.

cl-async now also has a simpler threading abstraction. Previously, you had to
lock the entire event loop and prevent its execution to add an event to it from
another thread. By using [notifiers](/cl-async/notifiers) instead, you can
trigger callbacks in the event loop thread *without* going through the dance of
adding dummy events and triggering them. And because notifiers provide a simpler
abstraction, it's no longer necessary to manually turn on threading support in
cl-async: it's always on now. See the updated [threading docs](/cl-async/threading)
for more info.

SSL has also been updated. Previously, cl-async used libevent's SSL wrapper.
This worked very well for most cases, but fell over in others. cl-async's SSL
support has been completely re-written *from scratch*, allowing it to be changed
and fixed as needed going forward. Libuv doesn't provide a wrapper for SSL, so
all the work is done by cl-async itself, using OpenSSL's memory BIO feature to
allow async communication over SSL/TLS.

[Idlers](/cl-async/idlers) are also a nice update, allowing you to run code on
each pass of the event loop.

## Plans

The v0.6.x upgrade goal was to switch libevent to libuv with minimal change to
the cl-async API. Coming up, we're planning to wrap the filesystem API for both
polling and IO, as well as async UDP support.

Also, there are bound to be bugs and problems after the switch to libuv, so 
as we go forward, we'll be tightening all the bolts.

