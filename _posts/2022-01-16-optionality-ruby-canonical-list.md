---
layout: post
title: "Optionality in Ruby: Using A Canonical List"
date: "2022-01-16T15:15"
tags: ["optionality", "ruby"]
---

Let's say you have a list of optional values&#x2013;a standard example is
data submitted from checkbox inputs&#x2013;what's a convenient way of making
sure the optional values are "correct"? 

I like compare the input with a "canonical list". Here's how that
works, using Ruby.

<!-- more -->

Given a list of canonical values:

    canonical_list = ["Foo", "Bar", "Baz", "Qux"]

You can very conveniently use ruby intersection operator `&` to filter and sort:

    others = ["Qux", "Bar", "skip", "Foo"]
    
    canonical_list & others
    # => ["Foo", "Bar", "Qux"]

If input might a little unreliable, with a little extra work, you can
use the canonical list to sort, filter, compact, and normalize in one
pass:

    maybes = ["foo", "skip", "qux", "bar"]
    
    canonical_list.reduce([]) { |acc,elt|
      if maybes.find { |maybe| elt.casecmp?(maybe) }
        acc << elt
      end
    
      acc
    }
    # => ["Foo", "Bar", "Qux"]

Note how we're iterating over the canonical list, not the input list.

If you have a special value that supercedes the others, you can
include that in your canonical list and normalization:

    canonical_list = ["Nope", "Foo", "Bar", "Baz", "Qux"]
    
    maybes = ["foo", "bar", "nope", "qux"]
    
    canonical_list.reduce([]) { |acc,elt|
      break acc if acc == ["Nope"]
    
      if maybes.find { |maybe| elt.casecmp?(maybe) }
        acc << elt
      end
    
      acc
    }
    # => ["Nope"]

Here we put the special value first, so we can return early. You
probably want to handle more complicated cases before you go to the
canonical list, but this seemed okay for a simple case.

Anyway, there's a simple technique for cleaning up some user
inputs. Checkboxes and labels ("tags") are probably the most common
usecases for this trick, but you might find others.

