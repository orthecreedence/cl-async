---
title: Threading | Documentation
layout: documentation
---

<a id="threading"></a>
Threading
=========

The goal of this section is to describe how to do some basic threaded tasks with
cl-async.

- [enable-threading-support](#enable-threading-support)
- [with-threading-context](#with-threading-context)
- [Example: queuing a background job](#queuing)
- [Example: using futures seamlessly](#futures)

<a id="enable-threading-support"></a>
### enable-threading-support
{% highlight cl %}
(defun enable-threading-support ())
  => nil
{% endhighlight %}

Tells libevent that you plan to use threading in this session. This sets up
proper locking around your event base and makes it safe to call libevent's
functions from different threads.

<a id="with-threading-context"></a>
### with-threading-context
{% highlight cl %}
(defmacro with-threading-context ((&optional base-id) &body body))
  => <return val of body form>
{% endhighlight %}

This macro grabs the context of the current running event base and binds it to
a thread-local variable. It also creates thread-local versions of the standard
lisp/C buffer objects.

This allows you to run any cl-async operations from within your separate thread
while being thread safe. A trivial example:

{% highlight cl %}
(bt:make-thread
  (lambda ()
    (as:with-threading-context ()
      (as:with-delay (3)
        ;; this form will be run in the same thread as your event loop
        (format t "hai!~%")))))
{% endhighlight %}

Using this method makes it trivial to share events that occur on an outside
thread with our event loop without having to resort to barbaric methods such as
polling.

For those running more than one event loop, here is a method to get the current
event loop's ID:

{% highlight cl %}
(defvar *base-id* nil)
(as:with-eventloop ()
  (setf *base-id* (cl-async-base:event-base-id cl-async-base:*event-base*))
  ...)
(bt:make-thread
  (lambda ()
    (as:with-threading-context (*base-id*)
      ...)))
{% endhighlight %}

<a id="queuing"></a>
### Example: queuing a background job
Here's a really short example of how we can create an event (with a callback),
queue some background work, and once the work is done, signal the event as
complete (from the background thread). Then the callback will be triggered *from
the libevent thread*.

{% highlight cl %}
(ql:quickload '(:cl-async :bordeaux-threads))

(as:enable-threading-support)
(as:with-event-loop ()
  (let* ((result nil)
         (event (as:make-event (lambda () (format t "Job finished! ~a~%" result)))))
    (bt:make-thread (lambda ()
                      (sleep 3)  ; such work
                      (setf result 42)
                      (as:add-event event :activate t)))))
{% endhighlight %}

Here, we create an event (which sits in the event loop inactive), start a thread
that does work in the backgorund (while we continue to process events in our
main thread). Once *all of our hard work* is done, the event is marked as active
and then libevent triggers the callback attached to our event *in our main
thread*.

Note that we save the result of our work into `result`. We can do this without
locking in this case because we know that once the callback fires, `result` is
done being written to (and won't be written to again by the worker thread). Your
specific use-cases may require locking, so keep that in mind.

<a id="futures"></a>
### Example: using futures seamlessly
Let's integrate what we just did with [futures](/cl-async/futures).

{% highlight cl %}
(defun work (operation)
  "Run `operation` in a background thread, and finish the returned future with
   the result(s) of the operation once complete. The future will be finished on
   the same thread `(work ...)` was spawned from (your event-loop thread)."
  (let* ((future (make-future))
         (err nil)
         (result nil)
         (event (as:make-event (lambda ()
                                 (if err
                                     (signal-error future err)
                                     (apply 'finish (append (list future) result)))))))
    (bt:make-thread (lambda ()
                      (handler-case
                        (setf result (multiple-value-list (funcall operation)))
                        (t (e) (setf err e)))
                      (as:add-event event :activate t)))
    future))

(as:enable-threading-support)
(as:with-event-loop ()
  (as:with-delay (1) (format t "event loop still running...~%"))
  (future-handler-case
    (alet ((val (work (lambda () (sleep 3) (+ 4 5)))))
      (format t "got val: ~a~%" val))
    (t (e) (format t "err: ~a~%" e))))
{% endhighlight %}

Notice that we're catching and forwarding errors to our future.

This is a toy example: you shouldn't spawn a thread every time you need a
background job done (it makes more sense to use a thread pool or something like
[lparallel](http://lparallel.org/)). What's important is that we can use futures
for both async work and on a threaded basis when using cl-async's threading
support.

