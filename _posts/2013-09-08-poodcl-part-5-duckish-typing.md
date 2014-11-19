---
layout: post
title: "POODCL part 5: Duckish typing"
description: ""
category: 
tags: []
date: "2013-09-08 02:42:24 PM"
---
{% include JB/setup %}

> The purpose of object oriented design is to reduce the cost of change.

One of the ideas behind type systems is that a language compiler can
make some optimizations if it knows about the type of objects it will
be dealing with in runtime. Common Lisp allows, but does not require,
a programmer to declare types ahead of time. Ruby doesn't care about
types at all, if an object has a method, it can be called. This is
called "Duck typing" after the idea that, "if it walks like a duck,
swims like a duck, and quacks like a duck" then we can call it a duck.

However, just because Ruby pretty much ignores types, programmers
might feel tempted, even subconsciously, to write type-sensitive code.
Chapter 5 of POODR
[gives us a couple of examples](https://github.com/skmetz/poodr/blob/master/chapter_5.rb)
in Ruby, and discusses how to rewrite these. I'll review and translate
these examples in Common Lisp.

<!-- more -->

The first example shows a `Trip` class with a `prepare` method that
depends on an argument of type `Mechanic` to work. Translating the
examples from page 87 into Common Lisp shows a different side to the
problem:

{% highlight common-lisp %}
(defclass trip ()
  ((bicycles  :reader :bicycles)
   (customers :reader :customers)
   (vehicle   :reader :vehicle)))

(defclass mechanic () ())

(defmethod prepare ((trip trip) (mechanic mechanic))
  (prepare-bicycles mechanic (bicycles trip)))

(defmethod prepare-bicycle ((mechanic mechanic) bicycle)
  ;; do something
  T)

(defmethod prepare-bicycles ((mechanic mechanic) &rest bicycles)
  (map 'list (lambda (bicycle) (prepare-bicycle mechanic bicycle)) 
       bicycles))
{% endhighlight %}

The methods `PREPARE-BICYCLE` and `PREPARE-BICYCLES` more-or-less
follow from earlier principles of specializing on their tasks, but as
interfaces, they are brittle and specific. We want to change this
code, to generalize or add other objects and methods, so we'll have to
change the interface.

POODR discusses how a programmer might feel tempted to build their own
dispatching code to try to solve this problem. Translated to Common
Lisp it looks like this:

{% highlight common-lisp %}
(defclass trip-coordinator () ())

(defmethod buy-food ((trip trip) (tc trip-coordinator)))

(defclass driver () ())

(defmethod gas-up ((trip trip) (driver driver)))

(defmethod fill-water-tank ((trip trip) (driver driver)))

(defmethod prepare ((trip trip) &rest preparers)
  (loop
     for preparer in preparers
     do (etypecase preparer
          (mechanic (apply #'prepare-bicycles preparer (bicycles trip)))
          (trip-coordinator (apply #'buy-food preparer (customers trip)))
          (driver (progn
                    (apply #'gas-up preparer (vehicle trip))
                    (apply #'fill-water-tank preparer (vehicle trip)))))))
{% endhighlight %}

Although it seems like the `PREPARE` method is nicely generalized and easy to
extend, it doesn't reflect the relationship of the tasks. This is
where duck typing is necessary.

In in Ruby, and in this chapter of POODER, we could give each of
`TripCoordinator`, `Mechanic`, and `Driver` classes a `prepare_trip`
method to specialize on. There's no need to be concerned about their
type, only that they have the expected behavior.

But in CLOS, methods have their own type dispatch. If used properly,
it has the effect that defining a specialized method for each class
does in the Ruby example. Something like this:

{% highlight common-lisp %}
(defmethod prepare ((trip trip) &rest preparers)
  (loop
     for preparer in preparers
     do (prepare-trip trip preparer)))

(defmethod prepare-trip ((trip trip) (mechanic mechanic))
  (loop
     for bicycle in (bicycles trip)
     do (prepare-bicycle bicycle)))

(defmethod prepare-trip ((trip trip) (tc trip-coordinator))
  (buy-food trip tc))

(defmethod prepare-trip ((trip trip) (driver driver))
  (let ((vehicle (vehicle trip)))
    (gas-up vehicle)
    (fill-water-tank vehicle)))
{% endhighlight %}

This isn't exactly the type insensitivity as in Ruby, the methods do
depend on the type of object they recieve, but in Common Lisp we're
not building a hierarchy of objects, we're building a network of them.
The `PREPARE-TRIP` method doesn't "belong" to the objects of
`MECHANIC`, `TRIP-COORDINATOR`, of `DRIVER`, it belongs to it's class
of `GENERIC-FUNCTION`. Generic function objects are for creating
behaviors for different objects, classes, or any combination thereof.
It's not duck typing, but it's just as flexible.

This is discussed further in
[Chapter 16 of Peter Seibel's <cite>Practical Common Lisp</cite>](http://www.gigamonkeys.com/book/object-reorientation-generic-functions.html)
and in Sonja Keene's excellent <cite>Object Oriented Programming in
Common Lisp<cite>.

When should we write our own type dispatch? Metz asks us to think
about this when considering using Ruby's `kind_of?` and `responds_to?`
methods. She draws on an example from Rails (since rewritten), and
shows a method with some type conditions. It's not easily translatable
without some distracting elements so consider the following code I
used in my Wizard's Castle adaptation:

{% highlight common-lisp %}
(defun value-of-event (event &optional value-ref)
  "Get most specific information about event"
  (assert (event-p event))
  (etypecase value-ref
    (null        (first (last event)))
    (symbol      (rest (member value-ref event)))
    ((integer 0) (last event value-ref))))
{% endhighlight %}

The first assertion is a kind of early failure type checking that Metz
is leading us to be skeptical of. This function will only ever work on
objects that `EVENTP` approves of. This code cannot be reused.
Events objects will have a kind of brittleness to them.

However the `ETYPECASE` form is not necessarily so bad. Metz makes the
case that the interface to the built in types, here being the `NIL`,
`SYMBOL`, and `INTEGER`, are much more stable than other classes that
I might make. With this stability, it makes sense that I could use
them like this to decide something (in this case, how to retrieve data
from my event objects implemented as a Lisp list).

Should the time come to refactor the events system, I'll consider
making the events a class, and define methods for it. I'll consider
carefully those methods, and how they can work with the other
objects in the Wizard's Castle game.
