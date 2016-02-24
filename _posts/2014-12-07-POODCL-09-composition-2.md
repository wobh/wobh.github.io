---
layout: post
title: "POODCL part 9: Composition, part 2"
description: ""
category: 
tags: [lisp, clos, poodr]
date: "2014-12-07 10:52:45 PM"
---
{% include JB/setup %}

At the end of chapter 8 of POODR I felt very puzzled and didn't have
confidence in my understanding of the Composite pattern as presented,
even after doing a translation of it in Common Lisp as above. This was
unusual situation in my experience of reading this this book, and I
thought I really should dig into the subject of the Composite pattern
to see what it was I was missing.

I think what it is, is that the Composite pattern is more complex,
powerful, and involving than presented in the example. Metz's text on
the subject indicates this, but without an example that really
demonstrates this, I found it hard to see through to appreciate the
Composite pattern until I read more on the subject in other books.

I should say, POODR has still been an enormous help to me, and I
recommend it with the only caveat so far, that a beginner may need
some additional support in understanding the Composite pattern in this
chapter.

It's a small, thing, but I'm going to dwell on it. I'm going to try a
rather naive implementation of the Composite pattern in Common Lisp
but try to make it so it demonstrates it's power and utility, in a way
I will probably be unsatisfied with, ultimately, but at least I tried.

<!-- more -->

Considered generally we could begin implementing the composition
pattern with these classes:

{% highlight common-lisp %}
(defclass component ()
  ((composite :initform nil :reader component-of))
  (:documentation "Base class of components"))

(defclass composite (component)
  ((components :initform nil :reader comprises))
  (:documentation "Base class of composites"))
{% endhighlight %}

In this implementation the `COMPONENT` class provides a slot to link
back to a single composite object. There is only a reader method,
`COMPOSED-OF` for this slot, we do not want this slot to be casually
overwritten with a `SETF`. There is not an `:INITARG` parameter for
this slot, for we don't want it set on initialization. Here we also
see a good use of `:INITFORM` for a slot. When `COMPONENT` objects are
made, the `COMPOSITE` slot *must* be nil. Of course, it is by default,
but `:INITFORM` still serves in a "code as documentation" way. We
signal to others (and our future selves) that values for this slot are
set in a particular way.

The `COMPOSITE` class inherits all this from `COMPONENT` (in Ruby
`Component` would be a module) and adds a `COMPONENTS` slot with the
same restrictions, and a reader method `COMPRISES`.

With these constraints, almost all interaction with the data of objects
of these classes must be through tightly bound methods which provide a
simple coherent interface.

{% highlight common-lisp %}
(defgeneric piece-p (piece whole)
  (:documentation "Test if component member of composite"))
(defgeneric compose (piece whole)
  (:documentation "Include a component into a composite"))
(defgeneric dispose (piece whole)
  (:documentation "Exclude a component from a composite"))
(defgeneric transit (piece whole)
  (:documentation "Transfer a component between composites"))

(defmethod piece-p ((piece component) (whole composite))
  (equalp (composed-of piece) whole))


(defmethod compose ((piece component) (whole composite))
  (adjoin piece (slot-value whole components)))

(defmethod compose :around ((piece (component)) (whole composite))
  (setf (slot-value piece 'composite) piece)
  (call-next-method))


(defmethod dispose ((piece component) (whole composite))
  (delete piece (slot-value whole components)))

(defmethod dispose :around ((piece component) (whole composite))
  (setf (slot-value piece 'composite) nil)
  (call-next-method))


(defmethod transit ((piece component) (whole composite))
  (let ((source (composed-of piece)))
    (cond ((null source)
           (compose piece whole))
          ((not (equalp source whole))
           (dispose piece source)
           (compose piece whole)))))
{% endhighlight %}

There are numerous problems with this implementation.

There's almost certainly some things to do here to make this more
transactional so that updates are better approximate ACID operations
in databases, but that's a research topic a bit out-of-scope here.

Another problem is that a given class can only inherit from one of
either `COMPONENT` or `COMPOSITE`. We'd need to do a bit of macro work
so that users could `DEFCOMPONENT` or `DEFCOMPOSITE` and specify
appropriate slot names. The methods may have to be generated by the
macro too. While we're at it we should fix the above-mentioned problem
of not being able to set the `:COMPONENT-OF` value at object creation,
by setting up `INITIALIZE-INSTANCE :AFTER` methods.

What happens if you want to `COMPOSE` a `COMPONENT` with another
`COMPONENT`? Well, in this case you get an error, which is probably
fine most of the time. But you might want to define component classes
that return nil, or perhaps even another to convert it to `COMPOSITE`
class.

These are all mildly tricky but solvable issues, in any case, what we
have is a set of methods on `COMPONENT` and `COMPOSITE` objects, and
classes that can be inherited to implement a Composite design pattern.
As naive as this implementation is, it's worth looking at what
`BICYCLE` would look like if we use it. Here's my take:

{% highlight common-lisp %}
(defclass sizable ()
  ((size :reader size-of :initarg :size))
  (:documentation "Provides a size slot"))

(defclass colorful ()
  ((color :reader color-of :initarg :color))
  (:documentation "Provides a color slot"))

(defclass bike-part (component)
  ((spare :reader sparep :initarg :is-spare))
  (:default-initargs :is-spare nil)
  (:documentation "General bike part class"))

(defclass bike-frame (bike-part sizable colorful)
  ())

(defclass chain (bike-part sizable)
  ()
  (:default-initargs :is-spare t))

(defclass tire (bike-part sizable)
  ()
  (:default-initargs :is-spare t))

(defclass tape (bike-part colorful)
  ())

(defclass bicycle (composite)
  ((spare-parts :initform (make-instance 'composite) :reader spares)))

(defmethod find-spare ((part bike-part) (bike bicycle))
  (when (piece-p part bike)
    (find-if (lambda (spare) (typep spare (type-of part)))
             (spares bike))))

(defmethod add-spare ((part bike-part) (bike bicycle))
  (unless (find-spare part bike)
    (compose part (spares bike))))

(defmethod use-spare ((spare bike-part) (bike bicycle))
  (let ((spare (find-spare part bike)))
    (when spare
      (transit spare bike))))

(defun make-bicycle-road (frame-size frame-color tape-color)
  (let ((bike (make-instance bicycle)))
    (compose (make-instance 'frame
                            :size frame-size
                            :color frame-color)
             bike)
    (compose (make-instance 'chain :size "10 speed") bike)
    (compose (make-instance 'tire :size "23 mm") bike)
    (compose (make-instance 'tape :color "red") bike)
    (loop
       for part in (remove-if-not #'sparep (comprises bike))
       do (compose part (spares bike))
       finally (return bike))))
{% endhighlight %}

Taken altogether, this is *way* more complex than the implementation
based on the one in POODR. I've all but abandoned the factory method
technique, but you can see what remains of it in `MAKE-BICYCLE-ROAD`
and I hope you imagine the equivalent for `MAKE-BICYCLE-MOUNTAIN`.
Instead I am relying on a combination of inheritance and composition
to define properties for these classes and their instances. I've
implemented, two different compositions, one in the `SPARES` slot of
`BICYCLE` and `BICYCLE` itself, and written methods so that we can
make reasonable use of them.

Despite the complexity our classes and methods are short, and
reasonably easy to understand and use. The Composite pattern
interfaces don't simply delegate but provide real utilities to these
objects. Despite their implementation problems `COMPONENT` and
`COMPOSITE` set down a solid foundation which can be refactored into
something more general.

I'll try another iteration of this to address the implementation
issues. In the meantime I hope this serves as illustration of
composition pattern concepts in Common Lisp.

[prev]({% post_url 2014-09-21-POODCL-08-composition-1 %})