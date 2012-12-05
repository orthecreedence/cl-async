---
title: Deprecating tcp-send
layout: post
---
[tcp-send](/cl-async/tcp#tcp-send) has been renamed to [tcp-connect](/cl-async/tcp#tcp-connect).
The reasoning behind this was because I felt that `tcp-send` was too transparent
a name for a lower-level library. Although libevent makes this easy (and also
somewhat transparent) via its bufferevents, I felt once I started building
drivers that having an established connection is much more "satisfying" than
sending data to a location, not caring if the connection is open or not.

So while the functionality has not changed *at all*, I believe the new API makes
more sense conceptually.

Also, `data` is no longer a required parameter in `tcp-connect`, but has been
moved to a keyword position, `:data`. This way, the *default* is to connect and
do nothing, which I believe is more standard and benefitial than to connect and
send data transparently (although like I said, this is still available via
`:data`).

`tcp-send` is still exported by cl-async, but just forwards to `tcp-connect`.
Please use `tcp-connect` from now on, as `tcp-send` may be removed later on.
