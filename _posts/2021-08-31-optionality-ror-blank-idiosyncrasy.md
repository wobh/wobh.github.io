---
layout: post
title: "Optionality in Ruby on Rails: A blankness idiosyncrasy"
date: "2021-09-03T19:30"
tags: ["optionality", "Ruby", "Ruby on Rails"]
---

A colleague and I discovered a funny little oddity in the way
`ActiveSupport` implements `#blank?` for different classes.

Source code here:

<https://github.com/rails/rails/blob/83217025a171593547d1268651b446d3533e2019/activesupport/lib/active_support/core_ext/object/blank.rb>

Discussion below:

<!-- more -->

The way `String#blank?` is defined has an extra convenience in that it
recognizes strings with only whitespace characters as "blank".

There's no corresponding convenience defined for, say `Array#blank?`
and it's interesting to think about why.

Imagine what it would take to consider this blank:

    [nil]

or this:

    [nil, nil]

or this:

    [[], nil, [nil]]

You'd need something that would recursively test for blankness every
member of the array. Strings are non-recursive, and only contain
characters, so the convenience can be much more efficiently
implemented, and had basically no special cases.

And that's basically it. 

