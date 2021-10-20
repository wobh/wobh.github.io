---
layout: post
title: "Optionality in Ruby: fetch chain delicacy"
date: "2021-10-20T08:30"
tags: ["optionality", "Ruby"]
---

It was pointed out to me that this example from the fetch chain post
is a little funny-looking:

    deep.
      fetch(:foo, {}).
      fetch(:baz, {}).
      fetch(:qux)

and it is, so let's talk about it!

<!-- more -->

One thing I like about fetch chains is that I can control how
"delicate" they are, and under what circumstances they raise
errors. This example only raises an error if `:qux` is missing,
regardless of whether `:foo` or `:bar` were also missing. Although I'm
sure I've written a fetch chain like this, it probably wasn't a good
idea, since the error could be seen as misleading.

A more robust version would have gone like this:

    deep.
      fetch(:foo, {}).
      fetch(:baz, {}).
      fetch(:qux, nil)

No errors raised, unless we really, really needed a non-nil value out
of `deep`. But if we wanted that level of robustness, we really should
have used `dig` instead of a fetch-chain.

A more likely scenario than the given example would be where we
require `:foo` and `:bar` but want to make `:qux` optional, so we'd do
this:

    deep.
      fetch(:foo).
      fetch(:baz).
      fetch(:qux, nil)

As unlikely to be useful as they are, it doesn't seem impossible that
circumstances exist where other combinations of optionality and
delicacy are useful. Maybe even something a bit silly like the first
example, but perhaps even sillier like this:

    deep.
      fetch(:foo).
      fetch(:baz, {}).
      fetch(:qux)

If find yourself writing one, it's probably a great idea to leave a
comment about why it matters.

I'm glad this came up, and I think it's the kind of thing that could
be worth exploring a bit more. Exceptions and breaks are one of the
consequences of optional values, usually unintended consequences, but
not always.

