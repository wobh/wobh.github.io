---
layout: post
title: "POODCL part 6: Inheritance"
description: ""
category: 
tags: []
date: "2013-09-11 07:21:30 AM"
---
{% include JB/setup %}

Chapter 6 of POODR discusses inheritance of class properties. You can
define subclasses whose objects have all the properties of their
superclasses plus whatever other properties we define for
them. Subclasses are specialized, more concrete versions of their more
general, more abstract superclasses.

In
[the code for POODR chapter 6](https://github.com/skmetz/poodr/blob/master/chapter_6.rb),
Metz approaches strategies for designing inheritable classes starting
with a concrete `Bicycle` class. It begins as a class of road bikes
and progressively gets abstracted to provide an abstract class for
road, mountain, and recumbent bikes. I've translated the examples into
Common Lisp and discussed how to refactor them.

<!-- more -->

Here is a `BICYCLE` class based on the Ruby version from
[page 107](https://github.com/skmetz/poodr/blob/master/chapter_6.rb#L1).

{% highlight common-lisp %}
(defclass bicycle ()
  ((size       :reader size       :initarg :size)
   (tape-color :reader tape-color :initarg :tape-color)
   (spares     :reader spares)))

(defmethod initialize-instance :after ((b bicycle) &key)
  (with-slots (spares tape-color) b
    (setf spares
          (list :chain "10-speed"
                :tire-size "23" ;millimeters
                :tape-color tape-color))))
{% endhighlight %}

I decided the best way to implement the Ruby `spares` method for this
example as a slot which gets initialized with an
`INITIALIZE-INSTANCE :AFTER` method. I feel this is the most
appropriate way to capture the value of the `TAPE-COLOR` slot to make
sure it's shared in the value for `SPARES`. It works equivalently to
the Ruby examples.

{% highlight common-lisp %}
CL-USER> (let ((bike (make-instance 'bicycle :tape-color "red" :size "M")))
           (values
            (size bike)
            (spares bike)))
"M"
(:CHAIN "10-speed" :TIRE-SIZE "23" :TAPE-COLOR "red")
{% endhighlight %}

Every child class of bicycle is going to inherit these features. If we
wanted to extend this class to cover mountain bikes and proceeded in
the naivest way possible, we might end up with something like this:

{% highlight common-lisp %}
(defclass bicycle ()
  ((style       :reader style       :initarg :style)
   (size        :reader size        :initarg :size)
   (tape-color  :reader tape-color  :initarg :tape-color)
   (front-shock :reader front-shock :initarg :front-shock)
   (rear-shock  :reader rear-shock  :initarg :rear-shock)
   (spares      :reader spares)))

(defmethod initialize-instance :after ((b bicycle) &key)
  (with-slots (style spares tape-color rear-shock) b
    (setf spares
          (if (eq style 'road)
              (list
               :chain "10-speed"
               :tire-size "23" ;millimeters
               :tape-color (tape-color b))
              (list
               :chain "10-speed"
               :tire-size "2.1" ;inches
               :rear-shock (rear-shock b))))))
{% endhighlight %}

We shouldn't mock this code. The constraints of the professional world
are sometimes cruel, so I'm sure there are many professionals who knew
better, but have done worse. I've added slots for mountain bike parts
and a style option to select on appropriate settings for the the
`SPARES` slot.

However, the specifications recognize two kinds of bicycle with
disjoint properties. `TAPE-COLOR` is just as irrelevant to the
`MOUNTAIN` style as `FRONT-SHOCK` and `REAR-SHOCK` are to `ROAD`
style, but every object will have those slots regardless. `STYLE` is
only useful for deciding what `SPARES` will be.

It works.

{% highlight common-lisp %}
CL-USER> (let ((bike (make-instance 'bicycle
                                    :style 'mountain
                                    :size "S"
                                    :front-shock "Manitou"
                                    :rear-shock "Fox")))
            (spares bike))
(:CHAIN "10-speed" :TIRE-SIZE "2.1" :REAR-SHOCK "Fox")

{% endhighlight %}
But that doesn't make it right.

We should proceed a little more reasonably, setting up a separate
`MOUNTAIN-BIKE` class that inherits from bicycle.

{% highlight common-lisp %}
(defclass mountain-bike (bicycle)
  ((front-shock :reader front-shock :initarg :front-shock)
   (rear-shock :reader rear-shock :initarg :rear-shock)))
{% endhighlight %}

Now we have to look carefully at the remaining slots to separate what
would be specific to a `ROAD-BIKE` class and what is common to both.
Metz discussed how to perform this analysis better than I could. But
we end up with code like this:

{% highlight common-lisp %}
(defclass bicycle ()
  ((size   :reader size :initarg :size)
   (spares :reader spares)))

(defclass road-bike (bicycle)
  ((tape-color :reader tape-color :initarg :tape-color)))
{% endhighlight %}

Every bicycle class has `SIZE` and `SPARES` slots, but `ROAD-BIKE` and
`MOUNTAIN-BIKE` have additional slots specific and appropriate to
them. To decide spares for `ROAD-BIKE` and `MOUNTAIN-BIKE` we can
trivially adapt the original implementation of
`INITIALIZE-INSTANCE :AFTER` for `BICYCLE`.

{% highlight common-lisp %}
(defmethod initialize-instance :after ((rb road-bike) &key)
  (with-slots (spares tape-color) rb
    (setf spares
          (list :chain "10-speed"
                :tire-size "23" ;millimeters
                :tape-color tape-color))))

(defmethod initialize-instance :after ((mb mountain-bike) &key)
  (with-slots (spares rear-shock) mb
    (setf spares
          (list
           :chain "10-speed"
           :tire-size "2.1" ;inches
           :rear-shock rear-shock))))
{% endhighlight %}

If you have been evaluating this code as we go along, you will also
need to remove the `INITIALIZE-INSTANCE :AFTER` method for `BICYCLE`
we defined earlier.

{% highlight common-lisp %}
(remove-method #'initialize-instance
               (find-method #'initialize-instance
                            (list :after)
                            (list (find-class 'bicycle))))
{% endhighlight %}

This implementation demonstrates a classic model of inheritance
between a superclass and two subclasses. It also demonstrates an
appropriate initialization method which sets up slot-values which
depend on other slot values. We can expect it work but lets try it
out:

{% highlight common-lisp %}
CL-USER> (let ((rb (make-instance 'road-bike :size "M" :tape-color "red"))
               (mb (make-instance 'mountain-bike :size "S"
                                  :front-shock "Manitou"
                                  :rear-shock "Fox")))
           (values
            (size rb)
            (spares rb)
            (size mb)
            (spares mb)))
"M"
(:CHAIN "10-speed" :TIRE-SIZE "23" :TAPE-COLOR "red")
"S"
(:CHAIN "10-speed" :TIRE-SIZE "2.1" :REAR-SHOCK "Fox")
{% endhighlight %}

Careful readers of the `INITITIALIZE-INSTANCE :AFTER` methods have
perhaps thought that our `BICYCLE` superclass is missing some slots,
which means that our subclasses are also missing them. Lets redefine
`BICYCLE` with `CHAIN` and `TIRE-SIZE` slots.

{% highlight common-lisp %}
(defclass bicycle ()
  ((size :reader size :initarg :size)
   (chain :reader chain :initarg :chain)
   (tire-size :reader tire-size :initarg :tire-size)
   (spares :reader spares)))
{% endhighlight %}

Several complications arise. We're going to want to setup `SPARES`
with the appropriate values for the subclass of bikes. It would also
be nice to setup default values for the new slots too. Some defaults
are appropriate for all bike types, but some are appropriate to the
subclasses.

We have options. One is to setup our classes like this:

{% highlight common-lisp %}
(defclass bicycle ()
  ((size  :reader size :initarg :size)
   (chain :reader chain
          :initarg :chain
          :initform "10-speed")
   (tire-size :reader tire-size :initarg :tire-size)
   (spares    :reader spares)))

(defclass road-bike (bicycle)
  ((tire-size :reader tire-size
              :initarg :tire-size
              :initform "23" ;millimeters
              )
   (tape-color :reader tape-color :initarg :tape-color)))

(defclass mountain-bike (bicycle)
  ((tire-size :reader tire-size
              :initarg :tire-size
              :initform "2.1" ;inches
              )
   (front-shock :reader front-shock :initarg :front-shock)
   (rear-shock  :reader rear-shock  :initarg :rear-shock)))
{% endhighlight %}

This roughly corresponds to the code on
[page 126](https://github.com/skmetz/poodr/blob/master/chapter_6.rb#L298).
The `:INITFORM` key gives a default value to the slot when an instance
is initialized. This kind of setup seems solid, but it leads to a
couple of problems. The first of which comes when Metz introduces a
new bike subclass, this one with a different default chain. This seems
easy enough to get around, by overiding the chain slot setup in the
new class.

{% highlight common-lisp %}
(defclass recumbent-bike (bicycle)
  ((chain :reader chain
          :initarg chain
          :initform "9-speed")))
{% endhighlight %}

But this is graceless. We also have a problem in that this new class
doesn't define a default value for `TIRE-SIZE`. Metz recommends that
the Ruby `Bicycle` class method `default_tire_size` raise a
`NotImplemented` error in this circumstance to alert future developers
adding classes and accessing new objects. Our Lisp implementation is
already going to raise an `UNBOUND-SLOT` error if one proceeds with
calling `TIRE-SIZE` on new `RECUMBENT-BIKE` objects.

There can be other issues, particular to Common Lisp, with using
`:INITFORM` key values like this. These are discussed
[Chris Reisbeck's notes on Graham's <span class="underline">ANSI Common Lisp</span> chapter 11](http://www.cs.northwestern.edu/academics/courses/325/readings/graham/chap11-notes.php)
but also at
[this Lisp tips post](http://lisptips.com/post/11728375873/initform-and-default-initargs).
The recommended way of dealing with these is with the
`:DEFAULT-INITARGS` which results in cleaner code overall. While we're
at it, we'll implement `RECUMBENT-BIKE` more completely.

{% highlight common-lisp %}
(defclass bicycle ()
  ((size      :reader size      :initarg :size)
   (chain     :reader chain     :initarg :chain)
   (tire-size :reader tire-size :initarg :tire-size)
   (spares    :reader spares))
  (:default-initargs
   :chain "10-speed"))

(defclass road-bike (bicycle)
  ((tape-color :reader tape-color :initarg :tape-color))
  (:default-initargs
   :tire-size "23" ;millimeters
    ))

(defclass mountain-bike (bicycle)
  ((front-shock :reader front-shock :initarg :front-shock)
   (rear-shock  :reader rear-shock  :initarg :rear-shock))
  (:default-initargs
   :tire-size "2.1" ;inches
    ))

(defclass recumbent-bike (bicycle)
  ((flag :reader flag :initarg :flag))
  (:default-initargs
   :chain "9-speed"
   :tire-size "28"))
{% endhighlight %}

Not only is this implementation much cleaner, the default-values are
now pushed outwards, external to the slot definitions. They are
important only where they need to be important: in the initialization
parameters. Reading this code, I find the `:DEFAULT-INITARGS` section
almost documentary. `:INITFORM` values can have a place, but it's a
very particular one.

We should now rexamine how the `SPARES` slots of `BICYCLE` objects are
initialized. We've been populating the `SPARES` slot with a
pregenerated property list. It's time to take that apart a bit.

{% highlight common-lisp %}
(defmethod initialize-instance :after ((b bicycle) &key)
  (with-slots (spares tire-size chain) b
    (setf spares
          (list
           :chain chain
           :tire-size tire-size))))

(defmethod initialize-instance :after ((rb road-bike) &key)
  (with-slots (spares tape-color) rb
    (setf (getf spares :tape-color) tape-color)))

(defmethod initialize-instance :after ((mb mountain-bike) &key)
  (with-slots (spares rear-shock) mb
    (setf (getf spares :rear-shock) rear-shock)))

(defmethod initialize-instance :after ((rb recumbent-bike) &key)
  (with-slots (spares flag) rb
    (setf (getf spares :flag) flag)))
{% endhighlight %}

We've added a new `INITIALIZE-INSTANCE :AFTER` method to the `BICYCLE`
superclass and this gets inherited by the subclasses including the new
method for `RECUMBENT-BIKE` objects. The handy thing about `:BEFORE`
and `:AFTER` methods is that they *all* get called for every
superclass of your object. `:AFTER` methods get called in
most-specific-last order, so the `BICYCLE` class's `:AFTER` method is
called first to create the `SPARES` property list. The `:AFTER`
methods for the subclass gets called next, to add properties to the
list.

{% highlight common-lisp %}
CL-USER> (spares (make-instance 'road-bike :tape-color "red"))
(:TAPE-COLOR "red" :CHAIN "10-speed" :TIRE-SIZE "23")
CL-USER> (spares (make-instance 'mountain-bike :front-shock "Manitou" :rear-shock "Fox"))
(:REAR-SHOCK "Fox" :CHAIN "10-speed" :TIRE-SIZE "2.1")
CL-USER> (spares (make-instance 'recumbent-bike :flag "tall and orange"))
(:FLAG "tall and orange" :CHAIN "9-speed" :TIRE-SIZE "28")
{% endhighlight %}

If we hadn't added the new method for `RECUMBENT-BIKE` objects, it
would initialize, but it wouldn't add the flag to the list of spares.

This is all pretty slick; we've implemented these classes and covered
most of the design concerns from this chapter using only features
built into the CLOS architecture: `INITIALIZE-INSTANCE` methods,
`:DEFAULT-INITARGS`, and inheritance. Our code is tight, focused, and
nearly self-explanatory. The technique that Metz introduces in this
chapter is of defining "template methods", which subclasses can either
specialize or inherit. Metz discusses how to setup method hooks so
that implementors don't have to rely on calling `super` for
features. Although we don't need to implement any of these to achieve
the same ends, it's worth considering what we could do to make it
easier.

One outstanding concern is that the `INITIALIZE-INSTANCE` methods have
a very significant dependence on knowing about the implementation
details of the class. The signs of this are in using the `WITH-SLOTS`
macro to access the slots directly, but also in the subclass `:AFTER`
methods knowing that `SPARES` is a property list, they can add
properties to.

On the one hand, it's reasonable to expect that implementors of
`INITIALIZE-INSTANCE` methods should require this kind of inside
knowledge about the classes they create objects of. But this is *not*
true of most methods we might define for our classes. It would be
best, even for `INITIALIZE-INSTANCE` methods, if we could limit how
much has to be known outside the class implementation. For this
iteration of demo code, I think we've done enough.

[next]({% post_url 2013-11-10-POODCL-07-roles-mixins-templates %})

[prev]({% post_url 2013-09-08-poodcl-part-5-duckish-typing %})
