---
layout: post
title: "Practical Object Oriented Design in Common Lisp part 1"
description: ""
category: 
tags: []
date: 2013-07-27 07:36:49 -0700
---
{% include JB/setup %}

I learned a lot when I first read Sandi Metz's <cite>Practical Object
Oriented in Ruby</cite> (POODIR), but one of the things I have
wondered was how much would be apply in Common Lisp's Object System
(CLOS). At the time, I didn't know CLOS very well, but I knew it was
very different from Ruby's. CLOS has multiple-inheritance and
multiple-dispatch. The notion of "message passing" as guiding metaphor
for OOP doesn't even work for CLOS. How would lispers apply Sandi
Metz's advice?

<!-- more -->

To get familiar with CLOS I read Sonja Keene's <cite>Object Oriented
Programming in Common Lisp</cite> (OOPCL). Keene's book is full of
well-formed Common Lisp code, to introduce CLOS concepts and good OOP
style. Metz's book has a lot of examples of Ruby code that violate
good OOP design principles which Metz uses to explain those principles
and how to make them better.

Lets look at one of Metz's early examples and see how we might
implement in CLOS. Here's the Gear class from chapter 2 "Designing
Classes That Have a Single Responsibility."

<pre><code class="ruby">class Gear
  attr_reader :chainring, :cog
  def initialize(chainring, cog)
    @chainring = chainring
    @cog       = cog
  end

  def ratio
    chainring / cog.to_f
  end
end
</code></pre>

Implementing it in CLOS:

<pre><code class="lisp">(defclass gear ()
  ((chainring :reader chainring :initarg :chainring)
   (cog       :reader cog       :initarg :cog)))

(defmethod ratio ((gear gear))
  (with-slots (chainring cog) gear
    (/ chainring (float cog))))
</code></pre>

This isn't very exciting code in either language, but it's just an
example. A few things to note, in Ruby we have some syntax to define
standard reader methods for the chainring and cog instance variables.
In CL we could create a similiar syntax but, natively we have to spell
things out a bit more. The instance variables (or "slots" as they are
called in CL) are essentially all there is of the class. However, the
DEFCLASS macro actually has it's own syntax and is automatically
generating the reader methods for the slots as well as setting up a
MAKE-INSTANCE method with key arguments for defining gears.

The ratio method in CL also seems a bit more complicated than the Ruby
ratio. You can see it's defined separately from the class. In CLOS
methods don't belong to a class, but to a "generic function" which
means we can define ratio methods for whatever classes, objects, or
combinations thereof we want. When called, Lisp will find and apply
the method definition by the arguments given or raise an error if it
can't find one. Keene explains this in detail in chapter 4 of OOPCL.

Let's see them in action:

In Ruby:

<pre><code class="ruby">> g = Gear.new(52, 11)
=> #&lt;Gear:0x2a23520 @chainring=52, @cog=11&gt;
> g.ratio
=> 4.7272727272727275
</code></pre>

In Common Lisp:

<pre><code class="lisp">> (setf g (make-instance 'gear :chainring 52 :cog 11))
#&lt;GEAR #x2100B0135D&gt;
> (ratio g)
4.7272725
</code></pre>

Over the course of chapter 2 Metz transforms the initial code,
demonstrating by principles of OOP design how to make it work with a
wheel class. In further articles, I'll take a closer look at these
transformations from bad code to good to see what they mean in Common
Lisp.