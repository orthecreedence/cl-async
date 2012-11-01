---
title: HTTP client update
layout: post
---
I decided to rethink my method for [creating a streaming HTTP client](http://orthecreedence.github.com/cl-async/2012/10/30/working-on-http-client.html)
after playing around with the internals of libevent a bit more. The verdict is
that I'd have to reprogram so many pieces to make up for the static functions I
can't access that it would be a fragile mess which would surely break in future
releases of libevent.

While depending on an already built (and already installed, in this case)
library seems like the way to go, in this case there are too many hurdles. I'm
going to write a very simple HTTP response parser in lisp that can tell if a
response from a server is "complete" yet. Its only function will be to determine
that yes, the server has sent a complete response, or no, the response is still
pending and we only have one or more pieces of it.

Once a response has been completed, the stream it exists on can be passed off to
Drakma and parsed as usual.

This simplifies things in some ways. I won't be writing a full parser, and can
leave most of the details up to Drakma, which is fully capable of doing the real
parsing. The downside is that I'll have to write more code than I had originally
hoped.

I was also hoping to avoid this method because I'm not intimately knowledgable
about HTTP. I know a lot of the protocol, but I'm assuming there are bits I
don't know about that will throw a wrench in my code. Maybe not.

Anyway, I'm getting to work on this new branch ([http-stream](https://github.com/orthecreedence/cl-async/tree/http-stream))
and hopefully should have some results in a few days.

Stay tuned.
