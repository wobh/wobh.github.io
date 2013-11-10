---
layout: post
title: "POODCL part 7: Roles, mixins, and templates"
description: ""
category: 
tags: [lisp, clos, poodr]
date: "2013-11-10 09:55:45 AM"
---
{% include JB/setup %}

Chapter 7 of POODR opens with the question of what would happen if we
need to develop recumbent mountain bikes? This is where we begin to
engage with inheritance more seriously. Metz proceeds to introduce
Ruby modules and how to identify roles for your code, but before we
discuss that aspect of we should look at the simplest answer in Common
Lisp.

{% highlight common-lisp %}
(defclass recumbent-mountain (recumbent-bike mountain-bike) ())
{% endhighlight %}

That's it.

{% highlight common-lisp %}
CL-USER> (tire-size (make-instance 'recumbent-mountain :rear-shock "Fox" 
                                   :flag "tall and red"))
"28"
{% endhighlight %}

We should note that the order of the superclasses matters for CLOS
objects. Here's `MOUNTAIN-RECUMENT`:

{% highlight common-lisp %}
(defclass mountain-recumbent (mountain-bike recumbent-bike) ())
{% endhighlight %}

{% highlight common-lisp %}
CL-USER> (tire-size (make-instance 'mountain-recumbent :flag "tall and yellow" 
                                   :rear-shock "Fox"))
"2.1"
{% endhighlight %}

Order of the superclasses is defines the "class precedence list" which
CLOS's uses for method combination.

Let's move on to the other examples and concepts in this chapter.

<!-- more -->

The examples in this chapter orient us around the idea of reusing code
by thinking about it's role. With inheritance, child classes can
resuse the methods and data of their ancestor classes. But what if an
object, needs to use methods and data from another class family? In
Ruby the answer lies in using extending Ruby Modules. But in Common
Lisp we can still use class inheritance.

We're going to revisit the problem of trip planning and scheduling
different objects like we did in <a
href="/2013/09/08/poodcl-part-5-duckish-typing.html">Part 5</a> but
now we want to generalize. Metz explains some of the process of
determining what objects respond to requests about schedule, and what
they have to know about it.

We want to consider a class-mixin `SCEDULABLE` for which classes that
need to be scheduled can use.

Here's a Common Lisp implementation of the `Schedule` class from
[POODR 148](https://github.com/skmetz/poodr/blob/master/chapter_7.rb#L1"):

{% highlight common-lisp %}
(defclass schedule ()
  ((start-date :accessor start-date :initarg :start-date)
   (end-date :accessor end-date :initarg :end-date)))

(defmethod scheduled? ((schedule schedule))
  (with-accessors ((start start-date) (end end-date)) schedule
    (format T "This ~S is not scheduled
  between ~A and ~A" schedule start end)))
{% endhighlight %}

In principle this class is more-or-less ready to be a mixin or
superclass. The `SCHEDULED?` method will respond for instances of
`SCHEDULE` which or any object that inherits from it, but that's not
how we're going to use it, it's really more of a data type for this
demonstration. Instead, we consider the a "role" of "schedulable"
things first by implementing these features within another class. From
this we consider a protocol for `SCHEDULABLE` which we figure out
later.

Here is a `BICYCLE` class which implements a scheduling protocol.

{% highlight common-lisp %}
(defclass bicycle ()
  ((size      :reader size      :initarg :size)
   (chain     :reader chain     :initarg :chain)
   (tire-size :reader tire-size :initarg :tire-size)
   (schedule  :reader schedule  :initarg :schedule)
   (lead-days :reader lead-days :initarg :lead-days))
  (:default-initargs
   :lead-days 1))

(defmethod initialize-instance :after ((bike bicycle) &key)
  (with-slots (schedule lead-days) bike
    (setf schedule (make-instance 'schedule)
          lead-days (make-duration :days lead-days))))

(defmethod scheduled? ((bike bicycle) start-date, end-date)
  (scheduled? (schedule bike) start-date end-date))

(defmethod schedulable? ((bike bicycle) start-date end-date)
  (not (scheduled? bike 
                   (date-minus start-date (lead-days bike))
                   end-date)))
{% endhighlight %}

A couple of notes here. We're going to suppose there's a package of
date-time functions which define `DATE-MINUS` and `MAKE-DURATION` for
us and that the values of `START-DATE` and `END-DATE` are already
types which can be used by those methods.

It's good to see this implementation, but we're going to refactor it
of course. The methods are all tied to `BICYCLE` objects. We would
have to implement these methods for every class of object we want to
schedule. What we need a mixin class for `SCHEDULABLE` objects.

{% highlight common-lisp %}
(defclass schedulable ()
  ((schedule  :writer schedule  :initargs :schedule)
   (lead-days :reader lead-days :initarg :lead-days))
  (:default-initargs
   :lead-days 0))

(defmethod initialize-instance :after ((schedulable schedulable) &key)
  (with-slots (schedule lead-days) bike
    (setf schedule (make-instance 'schedule)
          lead-days (make-duration :days lead-days))))

(defmethod schedulede? ((schedulable schedulable) start-date, end-date)
  (scheduled? (schedule bike) start-date end-date))

(defmethod schedulable? ((schedulable schedulable) start-date end-date)
  (not (scheduled? bike 
                   (date-minus start-date (lead-days bike))
                   end-date)))
{% endhighlight %}

With this implementation we can easily setup other schedulable
classes.

{% highlight common-lisp %}
(defclass vehicle (schedulable)
  ()
  (:default-initargs
   :lead-days 3))

(defclass mechanic (schedulable)
  ()
  (:default-initargs
   :lead-days 4))
{% endhighlight %}

This example demonstrates the basic inheritance we saw in
[part 6](/2013/09/11/poodcl-part-6-inheritance.html), which is one
reason why I thought it would be good to demonstrate combining
`RECUMBANT-BIKE` and `MOUNTAIN-BIKE` earlier. I would like to
demonstrate with other examples, but those will have to wait. Most of
Chapter 7 discusses important design concerns.

First there is the matter of finding the roles for our superclasses.
We're looking for features we expect to reuse. It's probably worth
implementing them a couple of times before refactoring them to stand
on their own.

Metz warns about a couple of antipatterns. If you have slot with a
name like `TYPE` or `CATEGORY` (I've personally done this with a
`KIND`) which the object uses to decide things, you probably have a
problem. What if you have to add more types? Similarly with using any
of the `TYPECASE` for deciding things depending on classes you've
defined. In Ruby these are signs that you will need a class or module,
but in Common Lisp, you can start with seeing if just a new method
will do.

In a short section called "Insist on the Abstraction," Metz describes
another design problem symptom: child class overriding a slot or
method to raise a not implementmented error. The last sentence there
is worth quoting in full:

    "If you cannot correctly identify the abstraction there may not be
    one, and if no common abstraction exits then inheritance is not
    the solution to your design problem."

The next section discusses the idea of the contract and how it relates
to the class abstraction, child classes agree to be substitutes for
their parent classes. Objects should behave in all ways that users of
objects of the parent class would expect. They are allowed to add
features and specialize, but they shouldn't subtract.

Lastly we review the problem of creating additional dependencies
between child classes and parent classes. In Ruby the sign of this is
using `super` to call a corresponding method in the parent class. In
Common Lisp the likely equivalent of this is `CALL-NEXT-METHOD` but
because of the nature of the CLOS method combination system, I'm not
sure this is quite so problematic. Certainly, if you are using
`:BEFORE` or `:AFTER` methods you don't have to use `CALL-NEXT-METHOD`
explicitly, but in some sense this happens behind the scenes.

Metz encourages us to use the template method pattern, but advises
that it's only useful with adjacent child-classes. If you made a third
generation of classes, you would need to define additional methods for
them, and likely end up with a confusing design. Her example for this
is a `MonsterMountainBike` class which inherits from `MountainBike`.
Although Metz doesn't implement it in her book, I feel that we should
here.

First, a refresher. Here's the Common Lisp `BICYCLE` and
`MOUNTAIN-BIKE` classes. I've added an addition slot, `SEAT` for the
purposes of demonstration.

{% highlight common-lisp %}
(defclass bicycle ()
  ((size      :reader size      :initarg :size)
   (chain     :reader chain     :initarg :chain)
   (tire-size :reader tire-size :initarg :tire-size)
   (seat      :reader seat      :initarg :seat)
   (spares    :reader spares))
  (:default-initargs
   :chain "10-speed"))

(defclass mountain-bike (bicycle)
  ((front-shock :reader front-shock :initarg :front-shock)
   (rear-shock  :reader rear-shock  :initarg :rear-shock))
  (:default-initargs
   :tire-size "2.1 inches"))

(defmethod initialize-instance :after ((b bicycle) &key)
  (with-slots (spares tire-size chain) b
    (setf spares
          (list
           :chain chain
           :tire-size tire-size))))

(defmethod initialize-instance :after ((mb mountain-bike) &key)
  (with-slots (spares rear-shock) mb
    (setf (getf spares :rear-shock) rear-shock)))
{% endhighlight %}

Now lets look at a basic implementation of `MONSTER-MOUNTAIN-BIKE`:

{% highlight common-lisp %}
(defclass monster-mountain-bike (mountain-bike)
  ())
{% endhighlight %}

Our concern is that `MONSTER-MOUNTAIN-BIKE` objects have be able to
make their own specializations. For example, let's say they're hard on
their seats, and have to carry a spare seat, in addition to everything
else. We can specialize their own `INITIALIZE-INSTANCE :AFTER` method:

{% highlight common-lisp %}
(defmethod initialize-instance :after ((mmb monster-mountain-bike) &key)
  (with-slots (spares seat) mmb
    (setf (getf spares :seat) seat)))
{% endhighlight %}

Does it setup the `SPARES` slot as we hope?

{% highlight common-lisp %}
CL-USER> (let ((mmb (make-instance 'monster-mountain-bike
                                   :rear-shock "Fox"
                                   :front-shock "Manitou"
                                   :seat "Brooks")))
           (spares mmb))
(:SEAT "Brooks" :REAR-SHOCK "Fox" :CHAIN "10-speed" :TIRE-SIZE "2.1 inches")
{% endhighlight %}

It does! Metz advises if you use the template method you should not
subclass more than one generation, it seems like we can implement the
template method pattern in Common Lisp across multiple generations
without the inheritance problems we would have in Ruby. I'll come up
with a more challenging example.

However, Metz also argues that having "wide and shallow" class
heirarchy is easier for humans to reason about. But surely, there are
problem domains complex enough to all but require it. With that
restriction, we might find ourselve creating arane, strangely named
classes which only serve the template pattern, while all our
functionality gets segmented into modules which get distributed over
the pattern. This could be easy enough to reason about, but it seems
to me like it might require regular refactoring in modules and
template classes when extending.

There's no substitute for a good, extensible model, and if that model
requires a deeply nested class heirarchy, we might have to reconsider
the template pattern. Fortunately in Common Lisp, methods belonging to
standard generic functions come with extensible, inheritable
`:BEFORE`, `:AFTER`, and `:AROUND` method specializers. I'm still
researching if these really help with the template method inheritance
problem, or if they are fancy way of calling `super` in Ruby. But with
the method combining powers of CLOS, they at least allow us to have
the effect of inheritable, extensible template methods.

