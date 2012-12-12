---
title: SSL over TCP, and a few other minor updates
layout: post
---
### SSL
You can now do [SSL over TCP](/cl-async/tcp-ssl) in cl-async. This is a fairly
new feature, so be on the lookout for any issues, but so far tests are
favorable.

This was built to support the [drakma-async](https://github.com/orthecreedence/drakma-async) HTTP
client I'm building, which now has https:// support.

One thing to be aware of: SSL is in its own package, `cl-async-ssl` which requires the
`cl-libevent2-ssl` package included in the [latest versions of the cl-libevent2 bindings](https://github.com/orthecreedence/cl-libevent2).
Also needed is the libevent2 SSL implementation, installed in its own library,
`libevent_openssl.(so|dll|dylib)`.

### Library loading change
In [cl-libevent2](https://github.com/orthecreedence/cl-libevent2), libevent's
`libevent_core` and `libevent_extra` are now loaded *instead of* `libevent`.
This means your libevent2 distribution needs to include the \_core and \_extra
libraries for cl-libevent2/cl-async to work. Using the plain `libevent` library
is deprecated, and also caused problems with loading the `libevent_openssl` lib
required by the SSL implementation.

If you get library errors while loading cl-libevent2, please make sure these
libs are correctly installed.
