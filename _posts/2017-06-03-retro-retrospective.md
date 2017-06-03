---
layout: post
title: "Retro retrospective"
date: "2017-06-03 08:30:00"
---

One nice thing about writing about one's mistakes, is that one never
runs out of material to write about.

<!-- more -->

In my [last post](/2017/06/01/retrospective-of-a-trivial-bugfix.html) I said:

> As far as I know, nothing remotely like the structure literal code
> above exists for this sort of thing.

By "this sort of thing" I meant some way to treat traditional
control-flow done with conditionals, as language primitive data
structures. This was pretty silly to say, as we can, of course use a
programming language's native data structures to describe conditionals
just as I did the example initializing the game's setting. A common
data structure for this is a map. It's not hard to imagine `case`
statements as mappings of keys to functions.

Clearly I need to reconsider my complaint in the last post.

Functions themselves are defined as mappings of inputs and
outputs. Abstractly, there's doesn't seem to be much point in
distinguishing between mappings `c`, `f`, and, `m` where:

~~~~~clojure
    (defn c [n]
      (cond
        (= 1 n) 2
        (= 2 n) 4
        (= 3 n) 6
        :else nil))

    (defn f [n]
      (when (contains? #{1 2 3} n)
        (* 2 n)))

    (def m {1 2, 2 4, 3 6})
~~~~~

There's no end of words written about closely corresponding code about
the virtues and faults of each implementation. As one who feels
strongly about the preferability of implementation `m`, I wish for better
understanding of the techniques and technologies that support that.

I sometimes feel, despite having been programming since I was a little
kid, that I really don't know how.
