---
layout: post
title: "Practical Object Oriented Design in Common Lisp part 2"
description: ""
category: 
tags: []
date: 2013-07-28 00:48:31 -0700
---
{% include JB/setup %}

In POODIR chapter 2 in the section called "Writing Code That Embraces
Change" Metz discusses two strategies: hiding instance variables and
hiding data structures. I'm going discuss the first of these.

Metz strongly recommends wrapping instance variables in accessor
methods instead of directly referring to them.

<!-- more -->

In this code the ratio method calls on the instance variables directly.

<pre><code class="ruby">class Gear
  def initialize(chainring, cog)
    @chainring = chainring
    @cog       = cog
  end

  def ratio
    @chainring / @cog.to_f
  end
end
</code></pre>

This code defines reader methods for it's instance variables and the
ratio method uses those:

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

In Common Lisp the method <code>SLOT-VALUE</code> can be used to
access the values of Common Lisp slots. We can define a very primitive
gear class and ratio method like so:

<pre><code class="lisp">(defclass gear ()
  ((chainring :initarg :chainring)
   (cog       :initarg :cog)))

(defmethod ratio ((gear gear))
  (/ (slot-value gear 'chainring)
     (float (slot-value gear 'cog))))
</code></pre>

The macro <code>WITH-SLOTS</code> provides a convenient shorthand:

<pre><code class="lisp">(defmethod ratio ((gear gear))
  (with-slots (chainring cog) gear
    (/ chainring (float cog))))
</code></pre>

Metz advises that all methods use accessors to access the values of
instance variables. The <code>DEFCLASS</code> macro will create
accessor functions for slots if specified:

<pre><code class="lisp">(defclass gear ()
  ((chainring :reader chainring :initarg :chainring)
   (cog       :reader cog       :initarg :cog)))
</code></pre>

And now we can use them in the ratio method

<pre><code class="lisp">(defmethod ratio ((gear gear))
  (/ (chainring gear) (cog gear))))
</code></pre>

The macro <code>WITH-ACCESSORS</code> allows you to use the accessor
methods, but requires you define aliases for them:

<pre><code class="lisp">(defmethod ratio ((gear gear))
  (with-accessors ((cr chainring) (cg cog)) gear
    (/ ch (float cg))))
</code></pre>

For the sake of clarity, most lisp code I've seen uses aliases with the
same name as the accessor:

<pre><code class="lisp">(defmethod ratio ((gear gear))
  (with-accessors ((chainring chainring) (cog cog)) gear
    (/ chainring (float cog))))
</code></pre>

Stylistic considerations aside, unquestionably, using accessor methods
like this is more flexible in both Common Lisp and Ruby. If you had to
refactor you would be much better off changing the accessor once
rather than adjusting everywhere you called the value. This classic
code virtue is called "Don't Repeat Yourself" or DRY.

What's funny, is that in these simple examples we see that Common Lisp
requires you to repeat yourself often.

Ruby classes allow you to define standard accessors with it's attr_
syntax, where Common Lisp's <code>DEFCLASS</code> has you specify the
accessor keyword for every slot that has one.

Because Ruby methods belong to the class, you don't have to specify
method arguments with a name and a class, like you do with Common
Lisp's <code class="lisp">(defmethod method-name ((object-parameter
class-specializer)) ... )</code> method definitions. You can name the
object parameter whatever you want to call it in the body of the
method, but for clarity's sake, most method definitions I've seen give
the object parameter is given the same symbol as the
class-specializer. Likewise with <code>WITH-ACCESSORS</code> with it's
accessors and aliases.

I'm not going to defend this extra verbosity of
<code>WITH-ACCESSORS</code>, which I hope you can see is trivial,
rather than seriously problematic. But I do want to show you why
<code>DEFMETHOD</code> takes the class-specializer and why it's useful
when hiding instance variables/slots in accessors.

As in Ruby, so in Common Lisp, you can redefine the accessor, to
provide extra functionality and isolate features:

<pre><code class="lisp">(defmethod chainring ((gear gear))
  (+ (slot-value gear 'chainring)
     *unanticipated-adjustment-factor*))
</code></pre>

But in Common Lisp you can, and really should, define what's called an
"around method" like this:

<pre><code class="lisp">(defmethod chainring :around ((gear gear))
  (+ (call-next-method)
     *unanticipated-adjustment-factor*))
</code></pre>

Around methods wrap around the core method definition and are where
you can make changes to what the method call returns, further
isolating the functionality. There are also <code>:before</code> and
<code>:after</code> methods you can define which you can use to setup
side effects; they don't return anything to the method call, they just
get triggered, either before or after it, and inside the :around
method. You can make as many of these ancillary methods as you want,
even define how they combine.

In this way, you can preserve the accessor method (or any other method
you define), but add all kinds of features to the same method call,
while isolating the features to distinct parts of your code.
Furthermore, because Common Lisp methods can be specialized by class,
the ancillary methods will apply to objects of child classes the
methods specialize on. You can overwrite them too, of course, but
using the class specialization features provides you options you
couldn't easily do in single-inheritance single dispatch object
system, and makes the system even more flexible.

Next, hiding data structures.
