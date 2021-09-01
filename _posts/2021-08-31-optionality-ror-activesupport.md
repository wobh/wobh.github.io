---
layout: post
title: "Optionality in Ruby on Rails: Active Support extensions"
date: "2021-08-31T08:20"
tags: ["optionality", "Ruby", "Ruby on Rails"]
---

ActiveSupport is a Ruby library full of useful little tools a few of
which are relevant to the topic of optionality and provide an
alternative to the `NullObject` pattern [described previously](file:///2021/07/28/optionality-ruby-null-objects.html).

Documentation here:

<https://guides.rubyonrails.org/active_support_core_extensions.html>

Discussion below:

<!-- more -->


##### `#blank?` and `#present?`

-   <https://api.rubyonrails.org/classes/Object.html#method-i-blank-3F>
-   <https://api.rubyonrails.org/classes/Object.html#method-i-present-3F>

The core concept here is "blankness" with classes having to define
`#empty?`. The complementary notion of "presence" is defined as "not
blank". There's no module to include, it's defined on `Object`. In
`/active_support/core_ext/object/blank.rb` since v2.2 with `#blank?`
being slightly older.

It's interesting to note which native objects will be considered
"blank". From the documentation.

> An object is blank if it's false, empty, or a whitespace string. For
> example, nil, '', ' ', [], {}, and false are all blank.

Zeros are not considered "blank". For one thing, `Numeric` doesn't
implement `#empty?` and that's the protocol that `#blank?` is built
on. Neither does `Proc`, nor `Struct`, nor `IO` (a bit of a surprise),
nor `Exception`, nor `Regexp`, nor `MatchData` (hmm). For all of these
and some other ones less familiar to me, `#blank?` will always be
`false` and `#present?` will always be `true`.

But `ENV`, `Dir`, `File`, `Queue`, and perhaps others, do define
`#empty?` and these are the ones for which the interface will be
meaningful.

I didn't check the Ruby Stdlib, but I'm sure `Set` does define
`#empty` and `OStruct` doesn't.


##### `#presence`

-   <https://api.rubyonrails.org/classes/Object.html#method-i-presence>

Somehow, I did not know about this, and learning about it in the
research was a TIL moment for me. I will absolutely be using this
because I really don't like the patterns:

    thing.present? ? thing : alternative
    thing.blank? ? alternative : thing

I'm happy to be able to replace these with:

    thing.presence || alternative

This doesn't seem like a big deal, but having read, written, and
debugged some gnarly conditional logic that abstracts to the same
thing as the ternary conditions above, I welcome the ability to break
a three statement expression down to a two.

Defined in `/active_support/core_ext/object/blank.rb`, since v2.3.8.


##### `#in?` and `#presence_in`

-   <https://api.rubyonrails.org/classes/Object.html#method-i-in-3F>
-   <https://api.rubyonrails.org/classes/Object.html#method-i-presence_in>

Defined in `/active_support/core_ext/object/inclusion.rb`, `#in?` has
been around since v3.1 and `#presence_in` since v4.1.8. This turns
`#include?` (provided in all `Enumerable` objects) into a small
protocol of it's own. While `#in?` is a largely syntactical nicety,
`#presence_in` provides the same kind of optional utility `#presence`
does.

Another TIL I'm pretty excited about.


##### `#try` and `#try!`

-   <https://api.rubyonrails.org/classes/Object.html#method-i-try>
-   <https://api.rubyonrails.org/classes/Object.html#method-i-try-21>

Defined in `/active_support/core_ext/object/try.rb`, `#try` has been
around since v2.3.2 and `#try!` since v4.0.2.

It happens I have a lot to say about `#try`. A lot of it is already
written up for a future post about the optionality qualities of
`#fetch`. But it worth pulling some of it out here, which is thatt
`#try` is a profound generalization.

One way to look at this whole optionality business is that, with every
conditional we write, we're stepping into a world of functions which
map their domain to one of two values. You can very narrowly consider
the co-domain of these functions to be the elements of a two-valued
logic, for example, "true" and "false"&#x2013;the very familiar set of
boolean values.

Booleans have the virtues of being long understood, conventional, even
traditional. But they can be problematic too, in ways I don't want to
get distracted by here. I don't want to get distracted by
"isomorphisms" of boolean logic at the moment either (Set theory,
famously, but I recently learned probability too). I'll write about
those another time.

No, I want to consider what Common Lisp calls the "generalized boolean" 

> **generalized boolean** n. an *object* used as a truth value, where the
> symbol `nil` represents *false* and all other *objects* represent
> *true*.

<http://www.lispworks.com/documentation/lw50/CLHS/Body/26_glo_g.htm#generalized_boolean>

Consider the universe of functions which map a domain to either the
input value, or to a "bottom" value, called, in both Ruby and Common
Lisp "nil". This is the world `#try` is making available to us. We use
it to chain expressions, "safely". "Nil managment," really.

In my opinion, `#try` is a little too robust for most uses. A cool
thing about it's exclamatory sibline `#try!` is that it raises an
error if a non-nil receiver doesn't respond to the method
argument. That's probably what I should be using, in almost every
place, instead.


##### So, what about the Null Object pattern then?

In [the post about Null Objects](file:///2021/07/28/optionality-ruby-null-objects.html) we mentioned one of somewhat
problematic things of the pattern in Ruby was that `NilClass` wasn't
inheritable. Of course there was nothing stopping us from defining
classes of object with a `#nil?` method and using that as a
protocol. But, semantically, does that make sense? No Null Object is
or can be an instance of `NilClass`. Also `#nil?` isn't used as a
protocol for conditional operators or for `!`, or `#compact` or any
other method of that sort (can't actually think of others at the
moment, but seems like there could be some) so the idea of using
`#nil?` as a protocol doesn't quite work for me either&#x2013;it's not used
that way anywhere else, even though, it kinda seems like it should be.

What's nice about these ActiveSupport utilities is that they are
either protocol-based with methods supported in many other objects
(`#empty?` or `include?`) or so generalized that the audacity of
"monkey patching" `Object` and `NilClass` to support them, makes
sense.

On review, I'm starting to see a distinction to make for the
`Enumerable` objects which have natural notions of emptyness and
inclusion, and more general objects that may or may not respond to an
arbitrary message. This latter category (supported by `Tryable`'s
inclusion in `Object`) includes "record" objects. Seems to me now,
that 80% of the time where I might use a Null Object, what I really
want is one of a couple different variations of "Default Object". I'll
have to think on this more.

Anyway, this has been an appreciation post of ActiveSupport's
optionality tools. Up next I think we're in a good spot to take a
quick look at Python.

