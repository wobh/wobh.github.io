---
layout: post
title: "Practical Object Oriented Design in Common Lisp part 3"
description: ""
category: poodcl
tags: [common-lisp, clos, poodr, ruby]
date: 2013-07-30 22:58:28 -0700
---

Hiding data structures in the next strategy Metz discusses in
Chapter 2. Just as hiding instance variables with accessors, is a good
idea, we'll see how hiding data structures also makes your code more
flexible. Again we'll adapt the sample code to Common Lisp and see how
these principles work in CLOS.

<!-- more -->

Here is one of Metz's examples, a class whose objects must be
initialized with an array of two member arrays.

~~~~~ruby
class ObscuringReferences
  attr_reader :data

  def initialize(data)
    @data = data
  end

  def diameters
    # 0 is rim, 1 is tire
    data.collect {|cell|
      cell[0] + (cell[1] * 2)}
  end
end
~~~~~

Here the diameters methods must know about the array's structure as it
loops over it.

Here's a quick pass at implementing it in Common Lisp.

~~~~~common-lisp
(defclass obscuring-references ()
 (data :reader data :initarg data))
  
(defmethod diameters ((o-ref obscuring-references))
  (map 'list (lambda (wheel)
               (destructuring-bind (rim tire) wheel
                 (+ rim (* tire 2))))
       (data o-ref)))
~~~~~

This may not seem so bad at first, since
<code>DESTRUCTURING-BIND</code> allows us to create structural
references on the fly, and it's also useful in the "code as
documentation" sense, but the references are only useful in that one
place. As Metz discusses, this code is vulnerable to future changes in
the initializing structure. Ultimately we want to create a class and
set up objects that be more flexibly adapted.

Metz suggests thinking about how to take structure and give it labels,
and interface that can be used, while hiding the structure, like this:

~~~~~ruby
class RevealingReferences
  attr_reader :wheels

  def initialize(data)
    @wheels = wheelify(data)
  end

  def diameters
    wheels.collect {|wheel|
      wheel.rim + (wheel.tire * 2)}
  end

  Wheel = Struct.new(:rim, :tire)
  def wheelify(data)
    data.collect {|cell|
      Wheel.new(cell[0], cell[1])}
  end
end
~~~~~

In Common Lisp we might do it like this:

~~~~~common-lisp
(defclass revealing-references ()
  ((wheels :reader wheels :initarg :wheels)))
  
(defstruct (wheel (:conc-name Nil)) rim tire)
  
(defmethod initialize-instance :after ((o-ref revealing-references) &key wheels)
  (setf (slot-value o-ref 'wheels)
        (map 'list (lambda (wheel)
                     (destructuring-bind (rim tire) wheel
                       (make-wheel :rim rim :tire tire)))
             wheels)))
  
(defmethod diameters ((o-ref revealing-references))
  (map 'list (lambda (wheel)
               (with-accessors ((rim rim) (tire tire)) wheel
                 (+ rim (* tire 2))))
       (wheels o-ref)))
~~~~~

We're still using <code>DESTRUCTURING-BIND</code> but now it's in the
specialized <code>INITIALIZE-INSTANCE :AFTER</code> method. This will
transparently create a list of <code>WHEEL</code> structures with
<code>RIM</code> and <code>TIRE</code> accessors. The diameters method
just has to roll over the wheel objects and use the accessors.

In Ruby the Struct class is more like a Common Lisp class, a basic
structure which provides accessors. You can add other methods to it
easily enough, and, of course you can lisp too. Here's what we should
do in Common Lisp to isolate the diameter calculating functionality

~~~~~common-lisp
(defmethod diameter ((wheel wheel))
  (with-accessors ((rim rim) (tire tire)) wheel
    (+ rim (* tire 2))))
  
(defmethod diameters ((o-ref revealing-references))
  (map 'list 'diameter (wheels o-ref)))
~~~~~

Common Lisp structs are optimized but if you want turn the wheel into
a class, now you can. The interface is stable. Lets look at the wheel
class.

~~~~~common-lisp
(defclass wheel ()
  ((rim  :reader rim  :initarg :rim)
   (tire :reader tire :initarg :tire)))
~~~~~

The <code>DIAMETER</code> method stays the same as above, lets bring
back the <code>GEAR</code> class and <code>RATIO</code> method.

~~~~~common-lisp
(defclass gear ()
  ((cog       :reader cog       :initarg :cog)
   (chainring :reader chainring :initarg :chainring)))

(defmethod ratio ((gear gear))
  (with-accessors ((cog cog) (chainring chainring)) gear
    (/ chainring (float cog))))
~~~~~

In Common Lisp we don't have to figure out whether a gear should
belong to a wheel or a wheel have a gear. We can write a method for
both.

~~~~~common-lisp
(defmethod gear-inches ((wheel wheel) (gear gear))
  (* (ratio gear) (diameter wheel)))
~~~~~

This chapter has been about isolating responsibilities in your code.
The <code>GEAR</code> and <code>WHEEL</code> classs and methods are
pretty much down to a single responsibility. There's still more to
learn, and in POODR chapter 3 we learn about "Managing Dependencies".
We've already avoided one dependency, by using multiple-dispatch with
our <code>GEAR-INCHES</code> method, but there's some other
dependencies worth considering next.

[next]({% post_url 2013-08-28-POODCL-04-cl-macros-and-rubyish-clos %})

[prev]({% post_url 2013-07-28-practical-object-oriented-design-in-common-lisp-part-2 %})

