---
layout: post
title: "Practical Object Oriented Design in Common Lisp part 1"
description: ""
category: 
tags: [ruby, common-lisp, clos]
date: 2013-07-27 07:36:49 -0700
---
<!-- {% include JB/setup %} -->

I learned a lot when I first read Sandi Metz's <cite>Practical Object
Oriented Design in Ruby</cite> (<a
href="http://www.poodr.info/">POODR</a>), but one of the things I have
wondered was how much I would be apply in Common Lisp's Object System
(CLOS). At the time, I didn't know CLOS very well, but I knew it was
very different from Ruby's object system. CLOS has
multiple-inheritance and multiple-dispatch. The notion of "message
passing" as guiding metaphor for OOP doesn't even work for CLOS. How
would lispers apply Sandi Metz's advice?

<!-- more -->

To get familiar with CLOS I read Sonja Keene's <cite>Object Oriented
Programming in Common Lisp</cite> (OOPCL). Keene's book is full of
well-formed Common Lisp code, to introduce CLOS concepts and good OOP
style. Metz's book has a lot of examples of Ruby code that violate
good OOP design principles which Metz uses to explain those principles
and how to make them better.

Lets look at one of Metz's early examples and see how we might
implement it in CLOS. Here's the Gear class from chapter 2 "Designing
Classes That Have a Single Responsibility."

{% highlight ruby %}
class Gear
  attr_reader :chainring, :cog

  def initialize(chainring, cog)
    @chainring = chainring
    @cog       = cog
  end

  def ratio
    chainring / cog.to_f
  end
end
{% endhighlight %}

Implementing it in CLOS:

{% highlight common-lisp %}
(defclass gear ()
  ((chainring :reader chainring :initarg :chainring)
   (cog       :reader cog       :initarg :cog)))

(defmethod ratio ((gear gear))
  (/ (chainring gear) (float (cog gear))))
{% endhighlight %}

This isn't very exciting code in either language, but it's just an
example. A few things to note, in Ruby we have some syntax to define
standard reader methods for the chainring and cog instance variables.
In CL we could create a similiar syntax but, natively we have to spell
things out a bit more. The instance variables (or "slots" as they are
called in CL) are essentially all there is of the class. However, the
DEFCLASS macro actually has it's own syntax and is automatically
generating the reader methods for the slots as well as setting up a
MAKE-INSTANCE method with key arguments for defining the values of
gear objects slots.

The ratio method in CL also seems a bit more complicated than the Ruby
ratio. You can see it's defined separately from the class. In CLOS
methods don't belong to a class, but to a "generic function" which
means we can define ratio methods for whatever classes, objects, or
combinations thereof we want. When called, Lisp will find and apply
the method definition by the arguments given or raise an error if it
can't find one. Keene explains this in detail in chapter 4 of OOPCL.

Let's see them in action:

In Ruby:

{% highlight ruby %}
>> g = Gear.new(52, 11)
=> #<Gear:0x2a23520 @chainring=52, @cog=11>
> g.ratio
=> 4.7272727272727275
{% endhighlight %}

In Common Lisp:

{% highlight common-lisp %}
(setf g (make-instance 'gear :chainring 52 :cog 11))
#<GEAR #x2100B0135D>
> (ratio g)
4.7272725
{% endhighlight %}

Over the course of chapter 2 Metz transforms the initial code,
demonstrating by principles of OOP design how to make it work with a
wheel class. In further articles, I'll take a closer look at these
transformations from bad code to good to see what they mean in Common
Lisp.

[next]({% post_url 2013-07-28-practical-object-oriented-design-in-common-lisp-part-2 %})
