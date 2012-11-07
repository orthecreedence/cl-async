---
title: Missing coroutines in Common Lisp
layout: post
---
Since a [healthy discussion of the CPS syntax of cl-async](http://www.reddit.com/r/lisp/comments/11lo3a/clasync_asynchronous_operations_for_common_lisp/)
on reddit, I've been thinking a lot about [CPS](http://en.wikipedia.org/wiki/Continuation-passing_style)
style and how terrible it is. If you don't agree with me, try converting a
blocking library (like [usocket](http://common-lisp.net/project/usocket/), for
instance) to be CPS.  It's fairly strightforward. Now the real issue comes into
play. What about every library that depends on usocket? Oh, they all have to be
converted to CPS as well. All the way down. Example of CPS:

{% highlight cl %}
(defun sleeper (finished-cb)
  (as:delay finished-cb :time 2))

(defun my-app ()
  (format t "Now sleeping for 2s.~%")
  ;; execution is passed via a series of callbacks
  (sleeper (lambda ()
			 ;; once the delay is done, execution continues here
             (format t "Done sleeping!~%"))))

(as:start-event-loop #'my-app)
{% endhighlight %}

Notice how when the sleep finishes, control is passed to the next function via a
callback. So every library that depends on blocking behavior will have to be
rewritten to pass any futher processing it needs to do after what used to be the
blocking call into a callback. This conversion process can be simplified greatly
by tools like [cl-cont](http://common-lisp.net/project/cl-cont/), but you still
have to wrap a lot of your code in macros to support this.

I've been researching ways around this. The only thing that seems to make sense
is [coroutines](http://en.wikipedia.org/wiki/Coroutine). Here's the same example
from above, but using coroutines (using a mythical package `coro`):

{% highlight cl %}
(defparameter *event-loop-coro* nil)

(defun sleeper ()
  (let ((app-coro (coro:current-coroutine)))
	;; when the delay returns, we resume the app-coro
    (as:delay (lambda () (coro:resume app-coro)) :time 2))
  ;; we resume the event loop so it can process its events
  (coro:resume *event-loop-coro*))

(defun my-app ()
  (format t "Now sleeping for 2s.~%")
  (sleeper)  ; this now blocks
  ;; execution resumes on the same stack
  (format t "Done sleeping!~%"))

(coro:with-coroutine (*event-loop-coro*)
  (as:start-event-loop
    (lambda ()
	  (coro:with-coroutine (app-coro)
	    (my-app)))))
{% endhighlight %}

In this case, `sleeper` is a bit more complicated, but `my-app` becomes much
more readable, understandable, and most importantly, preserves its call stack.
Most importantly, while `my-app` is blocking on `sleeper`, the event loop is
available to process more events.

What we have here is a hybrid between blocking calls with OS threads and
asynchronous processing with an event loop. Imagine now converting usocket to
use this new blocking/async interface via coroutines. All of a sudden, every
driver that's build on top of usocket (most of them are) is able to process
tens of thousands of requests/responses *at the same time* with just one OS
thread and __without any code modification__.

#### So what's the hold up?
Well, Common Lisp does not support coroutines. Coroutines are easily implemented
via continuations, but Common Lisp does not support continuations. Coroutines
can also be implemented via direct stack manipulation, but Common Lisp does not
support direct stack manipulation (aside from the catch/throw, block/return
directives and the condition system).

Not only does Common Lisp not support the building blocks to make coroutines,
as far as I know, the lisp implementations themselves don't expose interfaces to
do this either. So it's not as simple as creating a compatibility layer over
some implementation-specific extensions.

I tried [solving this problem directly via CFFI](https://github.com/orthecreedence/cl-coro)
and calling out to [libpcl](http://xmailserver.org/libpcl.html), but lisp just
chokes on it and segfaults. Interestingly, [ECL](http://ecls.sourceforge.net/)
does a lot better (the stacks switch fine, but the occasional segfault occurs),
presumably because it's all implemented in C stack already.

I think the problem is that the lisp stack is much more complicated than a
C stack and switching the C stack doesn't magically make everything work in
lisp land.

#### What's next?
Good question. The next step would probably be for me to probe the maintainers
of a few open-source lisp distributions and see what kind of problem I'm up
against. OpenMCL (now [Clozure CL](http://ccl.clozure.com/)) used to have a 
concept of ["stack groups"](http://psg.com/~dlamkins/sl/chapter32.html) (see
"Processes & Stack Groups: Juggling multiple tasks") which I'm assuming was the
DIY threading method exposed when OS threads weren't mainstream. Apparently,
stack groups have since been removed (most likely in favor of pure OS threads).

In other words, unless there's a portable way in C/CFFI to expose an interface
for coroutines/continuations/direct stack control across implementations, we're
at the mercy of the already busy lisp maintainers to include them. Or I can
always roll up my sleeves and dive in, but let's be realistic...this is a fairly
low-level feature, and I'd have to get to know the internals of all the open
source implementations fairly well.

So I'm screwed for now. Anyway, I'll keep everyone updated on this as time goes
on. In the meantime, I'd still like to continue converting libraries to be
async (coroutines or not). The [async versino of drakma](https://github.com/orthecreedence/drakma-async)
is coming along nicely, although not fully ready to use yet.
