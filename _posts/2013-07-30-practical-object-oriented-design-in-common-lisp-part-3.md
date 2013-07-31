---
layout: post
title: "Practical Object Oriented Design in Common Lisp Part 3"
description: ""
category: 
tags: []
date: 2013-07-30 22:58:28 -0700
---
<!-- {% include JB/setup %} -->

Hiding data structures in the next strategy Metz discusses in
Chapter 2. Just as hiding instance variables with accessors, is a good
idea, we'll see how hiding data structures also makes your code more
flexible. Again we'll adapt the sample code to Common Lisp and see how
these principles work in CLOS

<!-- more -->

Here is one of Metz's examples, a class whose objects must be
initialized with an array of two member arrays.

<pre><code class="ruby">class ObscuringReferences
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
</code></pre>

Here the diameters methods must know about the array's structure as it
loops over it.

Here's a quick pass at implementing it in Common Lisp.

<pre><code class="lisp">(defclass obscuring-references ()
 (data :reader data :initarg data))
  
(defmethod diameters ((o-ref obscuring-references))
  (map 'list (lambda (wheel)
               (destructuring-bind (rim tire) wheel
                 (+ rim (* tire 2))))
       (data o-ref)))
</code></pre>

This may not seem so bad at first, since
<code>DESTRUCTURING-BIND</code> allows us to create structural
references on the fly, and it's also useful in the "code as
documentation" sense, but the references are only useful in that one
place. As Metz discusses, this code is vulnerable to future changes in
the initializing structure. Ultimately we want to create a class and
set up objects that be more flexibly adapted.

Metz suggests thinking about how to take structure and give it labels,
and interface that can be used, while hiding the structure, like this:

<pre><code class="ruby">class RevealingReferences
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
</code></pre>

In Common Lisp we might do it like this:

<pre><code class="lisp">(defclass revealing-references ()
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
</code></pre>

We're still using <code>DESTRUCTURING-BIND</code> but now it's in the
specialized <code>INITIALIZE-INSTANCE :AFTER</code> method. This will
transparently create a list of <code>WHEEL</code> structures with
<code>RIM</code> and <code>TIRE</code> accessors. The diameters method
just has to roll over the wheel objects and use the accessors.

In Ruby the Struct class is more like a Common Lisp class, a basic
structure which provides accessors. You can add other methods to it
easily enough, and, of course you can lisp too.

<pre><code class="lisp">(defmethod diameter ((wheel wheel))
  (with-accessors ((rim rim) (tire tire)) wheel
    (+ rim (* tire 2))))
  
(defmethod diameters ((o-ref revealing-references))
  (map 'list 'diameter (wheels o-ref)))
</code></pre>

Common Lisp structs are optimized but if you want turn the wheel into
a class, now you can. The interface is stable. Lets look at the wheel
class.

<pre><code class="lisp">(defclass wheel ()
  ((rim  :reader rim  :initarg :rim)
   (tire :reader tire :initarg :tire)))
</code></pre>  

The <code>DIAMETER</code> method stays the same as above, lets bring
back the <code>GEAR</code> class and <code>RATIO</code> method.

<pre><code class="lisp">(defclass gear ()
  ((cog       :reader cog       :initarg :cog)
   (chainring :reader chainring :initarg :chainring)))

(defmethod ratio ((gear gear))
  (with-accessors ((cog cog) (chainring chainring)) gear
    (/ chainring (float cog))))
</code></pre>

In Common Lisp we don't have to figure out whether a gear should
belong to a wheel or a wheel have a gear. We can write a method for
both.

<pre><code class="lisp">(defmethod gear-inches ((wheel wheel) (gear gear))
  (* (ratio gear) (diameter wheel)))
</code></pre>

This section has been about isolating responsibilities in your code.
We've now begun to use Common Lisps multiple dispatch features, but
even so, the code has only a single responsibilities.
