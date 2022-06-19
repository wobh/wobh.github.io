---
layout: post
title: "Optionality in Common Lisp: From Nothing, Everything"
tags:
  - optionality
  - common-lisp
date: "2022-06-19 17:30-0400"
---

A brief follow-up thought I've been thinking about, but sort of
struggling with too. In Common-lisp, there's two worlds of values: the
world of "atoms" and the world of "lists" and `nil` is the only member
of both. Here's a kind "Vennish" diagram:

    {t [atom (nil] list)}

Why is this simple notion a struggle? I feel like there are a couple
of conceptual knots.

<!-- more -->

One is the idea that a list is a particular composition of cons cells
and yet, the type `cons` is a subtype of `list`. This means that while
`nil` is a valid `list`, it's not a valid `cons`. You can definitely
use `nil` as a parameter of the `cons` function, and of course, since
`nil` is a valid list.

On the other side of the house, both `t` and `nil` are of type `atom`
but, which `t` is not a valid `list` like `nil`, everything, including
nil is a valid member of type `t`. If you run `(typep * t)` it should
basically always be true.

One thing that's sort of helped is experimenting with `subtypep`. You
should see that type `null` is a subtype of both `atom` and
`list`. You should also see that type `cons` is of type `list`. You
should see that other configurations are invalid.

[prev]({% post_url 2022-05-11-optionality-common-lisp-generalized-booleans %})

