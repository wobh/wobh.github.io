---
layout: post
title: "Fundamental Arithmetic"
categories: "mathmusings"
date: "2021-02-15T23:00-0500"
---

I was planning on sharing this with a math teacher friend to see what
he thought. Maybe he would find something pedagogically useful in it.
Maybe he would help me understand it better myself--he loved teaching
and often wrote about what delight he found in helping a student "get
it". Unfortunately he passed away a couple of days ago. So, in
memoriam, here are some idle thoughts on basic arithmetic. I wasn't
sure how to wind it up, so it ends sort of suddenly. He would have
found that funny.

<!-- more -->

Given the set of all natural numbers greater than zero, `ℕ₁`, and the
operation of addition, `+`, there are no values of variables `a` and
`b` from the set of `ℕ₁` such that the sum, `a + b` is not also a
member of `ℕ₁`:

    a ∈ ℕ₁, b ∈ ℕ₁
    a + b ∈ ℕ₁

This notion is called "closure". If we expand the set to the positive
reals, `ℝ⁺` we'd still wouldn't be able to add two numbers escape the
positive reals. We could flip it around and try it with the set of
negative integers, `ℤ⁻`, but still, we can't escape. If we reduce the
set in some way things get a little interesting. Reduced to positive
odd numbers, we can derive the evens, but reduced to positive evens we
find ourselves confined to evens. Reduced to the set of primes, `ℙ`,
it looks like we can derive many if not all composites (if Goldbach's
conjecture is true we can derive all the evens). But in all variants
of one side of zero or the other, addition makes for a self-contained
world.

All that changes if you trade addition for subtraction.

This fact has long amused and intrigued me, even before I really knew
very much about algebra or what number classes were. Addition is
commutative `a + b = b + a` but subtraction isn't. Somehow the
asymmetry of subtraction opens up new possibilities for numbers and
makes symmetries possible. If you only had subtraction, you could
invent addition.

Given the set of natural numbers greater than zero, `ℕ₁` and the
operation of subtraction `-`, you can derive zero&#x2013;the additative
identity, the negative numbers&#x2013;the additive inverse, and of course,
addition itself.

    a ∈ ℕ₁, b ∈ ℕ₁
    a - a = 0
    a - 0 = a
    0 - a = (a - a) - a
    a + b = a - (0 - b)

We may have started out with counting numbers, but we discovered zero
and soon derived all the rest of the set of integers, `ℤ`. I'm not
versed enough in first order logic to really break it down, and
others, I'm sure, already have. But taking what we have, how far can
we go?

Lets skip ahead a bit an consider the case with multiplication.

Given the set of all natural numbers greater than zero, `ℕ₁`, and the
operation of multiplication, `×`, there are no values of `a` and `b`
from the set of `ℕ₁` such that the product, `a × b` is not also a
member of `ℕ₁`.

    a ∈ ℕ₁, b ∈ ℕ₁
    a × b ∈ ℕ₁

The same enclosure of positivity around addition is also around
multiplication. Within this enclosure is the fundamental theorem of
arithmetic which is pretty great and all, but, again, if you want to
get anywhere, you must trade multiplication for division.

    a ∈ ℕ₁, b ∈ ℕ₁
    a ÷ a = 1
    a ÷ 1 = a
    1 ÷ a = (a ÷ a) ÷ a
    a × b = a ÷ (1 ÷ b)

You could constrain `a` and `b` further, to simply prime numbers,
`ℙ`. Given division you can derive the multiplicative
identity&#x2013;1, the multiplicative inverses&#x2013;the rationals
~ℝ~, and multiplication emerges there after, by the fundamental
theorem of arithmetic everything else.

Can you derive or define division from subtraction? Maybe? If you're
confined to primes, you have to define `1` as `3 - 2`.

But then you can start walking up to it:

    a - (0 - a) = a ÷ (1 ÷ 2)
       a + a    =    a × 2

Generalizing this seems tedious, but perhaps there is an elegant
way. I'm not an expert, but I know the arithmetic I'm using is too
primitive to use for defining processes, and I'd have to be careful
with process definition because many looping constructs for repeating
actions could be seen as multiplications in disguise, and such a
definition of division by repeating subtraction could therefore be
seen as tautological.

But, supposing this is done, could we go further? what about powers,
roots and logarithms complex and algebraic numbers?

Well it happens that this has really intrigued me in the past because
it seems like the pattern of fundamental operations deriving new
classes of numbers repeats with a couple of interesting wrinkles,
however:

1.  My competence with these operations quickly wanes.
2.  You have to derive a product-over-sum operation, `(a × b) ÷ (a +
       b)`, to really bring them together. Per #1 I'm still figuring out
    how to use this.
3.  The offical notation is inconvenient for showing the patterns, but
    whatever it's faults are, it's at least compact, and I found
    spelling things out in a linear way was complicated enough to lose
    the patterns too. There's a couple of good attempts though, see
    3Blue1Brown's video "Triangle of Power"
    <https://www.youtube.com/watch?v=sULa9Lc4pck>
