---
layout: post
title: "POODCL part 8: Composition, part 1"
description: ""
category:
tags: [lisp, clos, poodr]
date: "2014-09-21 07:54:31 AM"
---
{% include JB/setup %}

So it's been an epic year for me and I haven't had a lot of time to
work on this, except here and there. I also ran into a bit of a
conceptual blocker that took a while to resolve, but more on that
later.

Object oriented techniques allow us to start thinking about composing
complex objects out of simpler ones. Class slot values can be objects
of our other classes, not just the basic types provided by the
language.

We're going to take another look at the `BICYCLE` class and see
different ways of composing it from simpler `PART` objects.

<!-- more -->

Here's a new look at the `BICYCLE` class which now has a size slot and
a parts slot. The new `PARTS` class will be a collection object which
is comprised of the parts of our bicycle.

{% highlight common-lisp %}

    (defclass bicycle ()
      ((size :reader size :initarg :size)
       (parts :reader parts :initarg :parts)))

    (defmethod spares ((bicycle bicycle))
      (spares (parts bicycle)))

{% endhighlight %}

In this implementation we've delegated knowledge of all the components
of the bicycle to the object in the `PARTS` slot. We have defined a
`SPARES` method for `BICYCLE` objects, but we can see it's just
calling a to-be-defined `SPARES` method for whatever may occupy the
`PARTS` slot of `BICYCLE`.

Here's a `PARTS` class and two child classes reusing the
implementations of `BICYCLE` classes from part 6.

{% highlight common-lisp %}

    (defclass parts ()
      ((chain :reader chain :initarg :chain)
       (tire-size :reader tire-size :initarg :tire-size)
       (spares :reader spares))
      (:default-initargs
       :chain "10-speed"))

    (defmethod initialize-instance :after ((parts parts) &key)
      (with-slots (spares tire-size chain) parts
        (setf spares
              (list
               :chain chain
               :tire-size tire-size))))

    (defclass road-bike-parts (parts)
      ((tape-color :reader tape-color :initarg :tape-color))
      (:default-initargs
       :tire-size "23 millimeters"))

    (defmethod initialize-instance :after ((rbp road-bike-parts) &key)
      (with-slots (spares tape-color) rbp
        (setf (getf spares :tape-color) tape-color)))

    (defclass mountain-bike-parts (parts)
      ((front-shock :reader front-shock :initarg :front-shock)
       (rear-shock  :reader rear-shock  :initarg :rear-shock))
      (:default-initargs
       :tire-size "2.1 inches"))

    (defmethod initialize-instance :after ((mbp mountain-bike-parts) &key)
      (with-slots (spares rear-shock) mbp
        (setf (getf spares :rear-shock) rear-shock)))

{% endhighlight %}

We can see that it works as expected, pretty much just like the code
in part 6.

{% highlight common-lisp %}

    CL-USER> (let ((rb (make-instance 'bicycle :size "L"
                                      :parts (make-instance 'road-bike-parts
                                                            :tape-color "red"))))
               (values (size rb) (spares rb)))
    "L"
    (:TAPE-COLOR "red" :CHAIN "10-speed" :TIRE-SIZE "23 millimeters")
    CL-USER> (let ((mb (make-instance 'bicycle :size "L"
                                      :parts (make-instance 'mountain-bike-parts
                                                            :rear-shock "Fox"))))
               (values (size mb) (spares mb)))
    "L"
    (:REAR-SHOCK "Fox" :CHAIN "10-speed" :TIRE-SIZE "2.1 inches")

{% endhighlight %}

There's an unfortunate aspect of the next few examples in that the
bike metaphor isn't necessarily a great example of the kind of
composition we are working towards. Bicycles aren't just a collection
of parts, but an arrangement of them. We've preserved a sense of this
arrangement so far, with parts first as slot values of one class or
another. The model we are working towards collects the parts and uses
the properties of the collection.

First, we refactor `PARTS` class, so that it's `PARTS` slot has a list
`PART` objects, rather than properties. We're going to define `PART`
and a `PRINT-OBJECT` method for it for these examples.

{% highlight common-lisp %}

    (defclass parts ()
      ((parts :reader parts :initarg :parts)))

    (defmethod spares ((parts parts))
      (remove-if-not #'needs-spare (parts parts)))

    (defclass part ()
      ((name :reader name :initarg :name)
       (description :reader description :initarg :description)
       (needs-spare :reader needs-spare :initarg :needs-spare))
      (:default-initargs
       :needs-spare t))

    (defmethod print-object ((part part) stream)
      (print-unreadable-object (part stream :type t)
        (with-slots (name description needs-spare) part
          (format stream ":name ~A :description ~A :needs-spare ~A"
                         name description needs-spare))))

{% endhighlight %}

Now we can define `PART` objects to use for composing bikes. In the
following example we make a bunch of `PART` objects and then make two
`BICYCLE` objects composing the parts list for them, in slightly
different ways.

{% highlight common-lisp %}

    CL-USER>(let* ((chain         (make-instance 'part :name  "chain"
                                                 :description "10-speed"))
                   (road-tire     (make-instance 'part :name  "tire-size"
                                                 :description "23 millimeters"))
                   (tape          (make-instance 'part :name  "tape-color"
                                                 :description "red"))
                   (mountain-tire (make-instance 'part :name  "tire-size"
                                                 :description "2.1 inches"))
                   (rear-shock    (make-instance 'part :name  "rear-shock"
                                                 :description "Fox"))
                   (front-shock   (make-instance 'part :name  "front-shock"
                                                 :description "Manitou"
                                                 :needs-spare nil))
                   (road-bike-parts (make-instance 'parts
                                                   :parts (list chain
                                                                road-tire
                                                                tape)))
                   (road-bike     (make-instance
                                   'bicycle :size "L" :parts road-bike-parts))
                   (mountain-bike (make-instance
                                   'bicycle :size "L"
                                   :parts
                                   (make-instance 'parts
                                                  :parts (list chain
                                                               mountain-tire
                                                               front-shock
                                                               rear-shock)))))
              (values (size road-bike) (spares road-bike)
                      (size mountain-bike) (spares mountain-bike)))
    "L"
    (#<PART :name chain :description 10-speed :needs-spare T>
     #<PART :name tire-size :description 23 millimeters :needs-spare T>
     #<PART :name tape-color :description red :needs-spare T>)
    "L"
    (#<PART :name chain :description 10-speed :needs-spare T>
     #<PART :name tire-size :description 2.1 inches :needs-spare T>
     #<PART :name rear-shock :description Fox :needs-spare T>)

{% endhighlight %}

One problem with this implementation is that the `PARTS` object
doesn't behave much like we would expect a collection to behave. We
can't call `LENGTH` or `SORT` or `FIND` on it and have it do anything
meaningful. Worse, we can't just define these methods on it, because
they're already defined as built-in functions for built-in objects of
the sequence type. Common Lisp doesn't allow us to subclass built-in
objects either.

As much as I understand it, the basic idea is that the Common Lisp
built-ins like `ARRAY`, `HASH`, and `SEQUENCE`, are intended to be
optimizable by a lisp compiler. As such, they are not really general
enough to be superclasses of CLOS objects, which are intended to be
high-level abstractions which could have that extensibility.

One thing we can do is use built-in objects as slot values, and define
methods to use their properties in a way abstracted from the
implentation. That's potentially a lot of customized methods. There
are also libraries which provide extensible collections objects. But
possibly the simplest solution, which happens to be good for a great
many uses, is simply something like these:

{% highlight common-lisp %}

    (defmethod list-of ((parts parts))
      (coerce (parts parts) 'list))

    (defmethod list-of ((bike bicycle))
      (list-of (parts bike)))

{% endhighlight %}

This is most similiar to the Ruby `to_a` method. It returns a list form of
the collection (however implemented) that can be manipulated with the
sequence functions.

It may seem that the parts object is redundant, however, it still
provides an abstraction to the implementation of the parts collection,
which could be an array, hash, list, or perhaps even a custom class.

It's unfortunate that Common Lisp doesn't have a standard sequence and
mapping object types we can inherit properties from and extend for our
own uses, like Ruby. However the Lisp cons cell primitives are
extremely flexible and let us build a variety of data structures we
can implement all kinds of things in.

One of these types is the property list which is like a dictionary,
but much looser than a hash. Property lists are simply regular lists
but with every other item being a keyword symbol. If we wanted to be
more rigorous about it, I would suggest we use a struct.

In order to preserve the wide-shallow class hierarchy recommended in
the last chapter, we will make a parts factory which generates
`BIKE-PARTS` objects. We'll use a properties list to setup up some
templates for the factory.

{% highlight common-lisp %}

    (defparameter *road-config*
      (list
       :chain "10-speed"
       :tire-size "23 millimeters"
       :tape-color "red"))

    (defparameter *mountain-config*
      (list
       :chain "10-speed"
       :tire-size "2.1 inches"
       :front-shock "Manitou"
       :rear-shock "Fox"))

{% endhighlight %}

These are basic property lists, assigned to dynamic variables. We
expect to use them with a bike parts factory like this:

{% highlight common-lisp %}

    (defun (parts-factory parts-config
                          &key
                          (parts-class parts)
                          (part-class part))
      (make-instance
       parts-class
       :parts (loop
                 for part-config in parts-config
                 collect
                   (make-instance
                    part-class
                    :name        (getf :name part-config)
                    :description (getf :description part-config)
                    :needs_spare (getf :needs-spare part-config)))))

{% endhighlight %}

You might fairly wonder why we don't use inheritance for this as there
doesn't seem to be any advantage to this except to preserve the
wide-shallow class hierarchy to a single level. But that's exactly
why. Factories are one way to make sure the class abstractions are
focused on the particular model.

As an extreme example of the kind of thing we're avoiding, consider
how we might design a bike object with many mixin part classes:

{% highlight common-lisp %}

    (defmacro defbikepart (part-name slot-name doc-string)
      `(prog1
           (defclass ,part-name ()
             ((,slot-name :reader ,slot-name
                          :initarg ,(intern (symbol-name slot-name) "KEYWORD")
                          :documentation ,doc-string)))
         (defun ,(intern (concatenate 'string "MAKE-" (symbol-name part-name)))
             (slot-value)
           (make-instance (quote ,part-name)
                          ,(intern (symbol-name slot-name) "KEYWORD")
                          slot-value))))

    (defbikepart bike-chain chain "a bike chain")

    ;; many other part classes defined this way

    (defclass bike (bike-frame
                    bike-handlebar
                    bike-seat
                    bike-fork
                    bike-tire-rear
                    bike-tire-front
                    bike-gear
                    bike-pedal
                    bike-chain)
      ((spares :accessor spares :initarg :spares
               :documentation "Spare parts")))

{% endhighlight %}

Although each parent class provides slots and behavior to the child
class, it also inherits particular behaviors simply from being
implemented this way, from the nature of being class with many parent
classes. Each of the parent classes is providing slots and behaviors
such that a this kind of `BIKE` "is a" kind of `BIKE-CHAIN`, but also
"is a" kind of `BIKE-PEDAL`, `BIKE-GEAR`, and so on.

When we implement the `BICYCLE` class with a composition pattern, we
create a "has a" relationship from bicycle objects to it's parts.

Implementing Metz's design in Common Lisp, here's one way we might
compose the `BICYCLE` class with a collection of parts:

{% highlight common-lisp %}

    (defclass bicycle ()
      ((size :reader size :initarg :size)
       (parts :reader parts :initarg :parts)))

    (defmethod spares ((bicycle bicycle))
      (spares (parts bicycle)))

    (defclass parts ()
      ((parts :reader parts :initarg :parts)))

    (defmethod spares ((parts parts))
      (remove-if-not #'needs-spare (parts parts)))

    (defclass list-of ((parts parts))
      (coerce (parts parts) 'list))

    (defclass part ()
      ((name :reader name :initarg :name)
       (description :reader description :initarg :description)
       (needs-spare :reader needs-spare :initarg :needs-spare))
      (:default-initargs
       :needs-spare t))

    (defmethod print-object ((part part) stream)
      (print-unreadable-object (part stream :type t)
        (with-slots (name description needs-spare) part
          (format stream ":name ~A :description ~A :needs-spare ~A"
                         name description needs-spare))))

    (defun (parts-factory parts-config
                          &key
                          (parts-class parts)
                          (part-class part))
        (make-instance
         parts-class
         :parts (loop
                   for part-config in parts-config
                   collect
                     (make-instance
                      part-class
                      :name        (getf :name part-config)
                      :description (getf :description part-config)
                      :needs_spare (getf :needs-spare part-config)))))

    (defparameter *road-config*
      (list
       :chain "10-speed"
       :tire-size "23 millimeters"
       :tape-color "red"))

    (defparameter *mountain-config*
      (list
       :chain "10-speed"
       :tire-size "2.1 inches"
       :front-shock "Manitou"
       :rear-shock "Fox"))

{% endhighlight %}

This chapter sums up with a discussion of the virtues and faults of
designing objects in composition verses inheritance. It's an important
section of the book and deserves serious consideration. However, I
find I have to discuss another unfortunate aspect of our example,
first, because it confused me for a long time, and it wasn't until I
read some more of about the Composite pattern in other books that I
think I figured it out.

The issue is that the [example implementation](https://github.com/skmetz/poodr/blob/master/chapter_8.rb#L422), translated here, really
just barely implements a composite pattern. True, `BICYCLE` and
`PARTS` share an interface in the `SPARES` method, and we can infer
others, but I feel like I could fairly say it merely delegates the
containing and filtering of parts to a specialized `PARTS` collection
object. To be a Composite we need to create an interface that `BIKE`
shares with `TIRE`, `HANDLEBARS`, `SEAT`, etc, not the container
object they happen to be stored in.

We'll see what I came up with for that in part 2.
