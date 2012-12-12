---
title: Playing with futures (and async interfaces in general)
layout: post
---
One of the main feedback points I've gotten on cl-async is the fact that it uses
CPS (continuation apssing style), which essentially means that asynchronous
operations do not return values, but instead are given callbacks which receive
the values computed by the operation as arguments.

The biggest problem I see with CPS is not so much the style (I do a lot of JS
programming and honestly it doesn't bug me too much), but the fact that if a
function works synchronously, converting it to work asynchronously suddenly
means that every other function that calls it must not be converted to CPS. This
is, thanks to Common Lisp's lack of continuation support, impossible to get
around, as far as I can tell. I know of [cl-cont](http://common-lisp.net/project/cl-cont/)
and a few other libraries, but they don't fix the fundmental problem.

The syntax can be changed around to make it prettier, but _currently there is no
way to convert a synchronous function to being asynchronous without rewriting
every function that calls it in CPS_. How horrible.

### Futures
A future is an object that may recieve a value later. One or more callbacks can
be attached to it, and these callbacks will be called when the future's value is
done computing. This makes the syntax for async operations a bit more flexible
and natural, but also allows some nice opportunities for syntax wrapping via
macros that's a *lot* cleaner than code walkers (like cl-cont).

Here is a [comment on Reddit about futures, with a great example](http://www.reddit.com/r/lisp/comments/11lo3a/clasync_asynchronous_operations_for_common_lisp/c6nnvwk).
along with some great discussion about the implementation and how it works.
Basically, a value from deep down in a bunch of nested callbacks can be attained
by being smart about how futures are binding to each other.

I've implemented this type of future in the [future branch](https://github.com/orthecreedence/cl-async/tree/future)
of cl-async, and plan on using it for building drivers.

### cl-async syntax
I've decided that although I'd like to include my future implementation in the
master branch of cl-async once I know it's stable, I will not be implementing
futures in cl-async itself, but rather have futures be the standard for creating
drivers.

The reason behind this is that futures are very value-based (like most drivers)
and the cl-async library is very stream/multi-value based. It doesn't really
make sense to implement futures in TCP, for instance, and re-trigger a future
whenever new data comes in. Futures also don't make sense for servers, and would
add uneeded complexity to delayed functions. I feel there is no part of cl-async
that would benefit from having integrated futures.

I've also decided that I don't want to include cl-cont or any other code walker.
I have nothing against them and I think the goals are admirable, but they don't
solve the real problem cl-async has: the impossibility of blocking the current
operation while processing other events on the same thread, and continuing the
original operation once it completes (ie, continuations).

While some might cringe at CPS, I feel that cl-async has an easy, understandable
API and if someone wants to wrap it in a syntax layer, be my guest, but know
that the root problem is still there: you will never be able to call functions
using CPS without converting all functions using it to be CPS, either by hand or
with a code walker.

Such is our plight.
