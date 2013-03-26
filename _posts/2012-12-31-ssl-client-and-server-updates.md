---
title: SSL client and server updates
layout: post
---
After having some problems with the `wrap-in-ssl` function, I decided to rewrite
the SSL client code. The result is [tcp-ssl-connect](/cl-async/tcp-ssl#tcp-ssl-connect),
which functions almost exactly like [tcp-connect](/cl-async/tcp#tcp-connect)
except that it can speak SSL.

Also is the new [SSL server implementation](/cl-async/tcp-ssl#tcp-ssl-server),
which also acts a lot like the [plain tcp server function](/cl-async/tcp#tcp-server)
but it speaks SSL (and takes keyword arguments for setting up your certificates
and key).

Check out the [new documentation on SSL](/cl-async/tcp-ssl) for an overview.
It's now possible to write clients and servers that use SSL with the same
simplicity as writing normal clients/servers.

__Please note__ that the SSL implementation is rather new, and although it's
passing its [tests](/cl-async/tests), I expect a few more bugs will pop up until
it has a while to stabilize.
