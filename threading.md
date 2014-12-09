---
title: Threading | Documentation
layout: documentation
---

<a id="threading"></a>
Threading
=========

The goal of this section is to describe how to do some basic threaded tasks with
cl-async.

Please familiarize yourself with [notifier](/cl-async/notifiers).

- [Example: queuing a background job](#queuing)
- [Example: using promises seamlessly](#promises)

<a id="queuing"></a>
### Example: queuing a background job
Here's a really short example of how we can create notifier, queue some
background work, and once the work is done, trigger the notifier (from the
background thread). Then the callback will be triggered *from the event loop
thread*.

{% highlight cl %}
(ql:quickload '(:cl-async :bordeaux-threads))

(as:with-event-loop ()
  (let* ((result nil)
         (notifier (as:make-notifier (lambda () (format t "Job finished! ~a~%" result)))))
    (bt:make-thread (lambda ()
                      (sleep 3)  ; such work
                      (setf result 42)
                      (as:trigger-notifier notifier)))))
{% endhighlight %}

Here, we create a notifier (which sits in the event loop inactive), start a thread
that does work in the backgorund (while we continue to process events in our
main thread). Once *all of our hard work* is done, the event is marked as active
and then libevent triggers the callback attached to our event *in our main
thread*.

Note that we save the result of our work into `result`. We can do this without
locking in this case because we know that once the callback fires, `result` is
done being written to (and won't be written to again by the worker thread). Your
specific use-cases may require locking, so keep that in mind.

<a id="promises"></a>
### Example: using promises seamlessly
Let's integrate what we just did with [promises](http://orthecreedence.github.io/blackbird/).

{% highlight cl %}
(defun work (operation)
  "Run `operation` in a background thread, and finish the returned future with
   the result(s) of the operation once complete. The future will be finished on
   the same thread `(work ...)` was spawned from (your event-loop thread)."
  (bb:with-promise (resolve reject :resolve-fn resolver)
    (let* ((err nil)
           (result nil)
           (notifier (as:make-notifier (lambda ()
                                         (if err
                                             (reject err)
                                             (apply resolver result))))))
      (bt:make-thread (lambda ()
                        (handler-case
                          (setf result (multiple-value-list (funcall operation)))
                          (t (e) (setf err e)))
                        (as:trigger-notifier notifier))))))

(as:with-event-loop ()
  (as:with-delay (1) (format t "event loop still running...~%"))
  (bb:catcher
    (alet ((val (work (lambda () (sleep 3) (+ 4 5)))))
      (format t "got val: ~a~%" val))
    (t (e) (format t "err: ~a~%" e))))
{% endhighlight %}

Notice that we're catching and forwarding errors to our future.

This is a toy example: you shouldn't spawn a thread every time you need a
background job done (it makes more sense to use a thread pool or something like
[lparallel](http://lparallel.org/)). What's important is that we can use promises
for both async work and on a threaded basis when using cl-async's threading
support.

