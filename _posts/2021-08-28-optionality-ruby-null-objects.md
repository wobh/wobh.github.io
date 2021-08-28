---
layout: post
title: "Optionality in Ruby: using the NullObject pattern"
date: "2021-07-28T09:30"
tags: ["optionality", "Ruby"]
---

Avdi Grimm has already written a very excellent little article about
using a Null Object pattern for optionality support in Ruby. You
should all read it:

<https://avdi.codes/null-objects-and-falsiness/>

Grimm talks about the limits of this pattern in Ruby and those limits
all orbit around the non-extensibility of Ruby's native `NilClass`. I
don't have mucht to say, but what follows are a couple of implications
I think worth expanding on.

<!-- more -->

Null objects can't be removed with `Array#compact` or `Hash#compact`
you have to write a special filter. Not a big deal if you did as
suggested and implemented `NullObject#nil?`, you can write:

    collection.reject(&:nil?)

I don't think it would be a dealbreaker either, if having a project
using `NullObject`, you made sure that others working on the project
understood the pattern when they saw it in use, and were encouraged to
use it. Lots of projects have such utilities whose usage requires
training and documentation.

But it is &#x2026; an impedance.

In lieu of native language support, it would be helpful if there were
a well-supported library for this. The top of my search results was
this gem:

<https://github.com/martinciu/nullobject>

Although, I haven't used it, the pattern is probably useful for a lot
of projects. It would be good to start adopting it.

Anyway, that's all I feel worth saying about this at the moment.

Next up, I might write about ActiveSupport's core extensions `#blank?`
and `#present?` although I don't have much to say about those
either. Alternatively, I might hop over to Python-land and look at
how it's handled over there, since you can create "falsy" objects in
Python.

