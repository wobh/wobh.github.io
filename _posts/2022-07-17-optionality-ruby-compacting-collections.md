---
layout: post
title: "Optionality in Ruby: compacting collections"
date: "2022-07-17T10:30"
tags: 
  - optionality
  - ruby
---

A handy trick for building parameters with optional arguments is to
build a parameter collection allowing the optional values to be `nil` then run
`#compact` on them. We've seen some hints of this, but lets look at the
technique more closely.

    maybe = [3, nil].sample
    
    [ 1, 2, maybe, 4].
        compact.
        append(5)
    
      { :foo => 1,
        :bar => 2,
        :baz => maybe,
        :qux => 4
      }.compact.
        merge(wat: 5)

Not much else to be said, really. `#compact` is defined on `Array`,
`Hash`, and `Enumerable` so you'll likely find it on many other
collections. It only works with `nil` values, but you can generalize
the technique with `Enumerable#select`.

