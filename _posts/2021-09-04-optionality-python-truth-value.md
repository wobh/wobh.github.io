---
layout: post
title: "Optionality in Python: Truth Value Testing"
date: "2021-09-04T10:00"
tags: ["optionality", "Python"]
---

Python has, I think, a most admirable notion of "falsification". We'll
quote the documentation here:

> By default, an object is considered true unless its class defines
> either a `__bool__()` method that returns False or a `__len__()`
> method that returns zero, when called with the object. Here are most
> of the built-in objects considered false:
> 
> -   constants defined to be false: `None` and `False`.
> -   zero of any numeric type: `0`, `0.0`, `0j`, `Decimal(0)`, `Fraction(0, 1)`
> -   empty sequences and collections: `''`, `()`, `[]`, `{}`, `set()`, `range(0)`
> 
> Operations and built-in functions that have a Boolean result always
> return `0` or `False` for false and `1` or `True` for true, unless
> otherwise stated. (Important exception: the Boolean operations `or`
> and `and` always return one of their operands.)

<https://docs.python.org/3/library/stdtypes.html#truth-value-testing>

You can read about Python's boolean coercion methods, `__bool__` and
`__len__` here:

-   <https://docs.python.org/3/reference/datamodel.html#object.__bool__>
-   <https://docs.python.org/3/reference/datamodel.html#object.__len__>

Discussion below:

<!-- more -->

Knowing this, I know I can use the Null Object pattern, creating null
objects and expect *anything* that tests for "falsiness" will operate
on them as if "False". I find it somewhat exhilerating to see this in
action:

Given:

    class Wat:
        def __bool__(self):
    	return False

Then:

    >>> a_wat = Wat()
    >>> bool(a_wat)
    False
    >>> if a_wat and True:
    ...     True
    ... else:
    ...     False
    ... 
    False

If your object is a container, or non-atomic aggregate, it makes sense
to use the `__len__()` for the protocol.

    class Bag:
        def __init__(self, *args):
    	self.stuff = *args
    
        def __len__(self):
    	return len(self.stuff)

    >>> a_bag = Bag()
    >>> len(a_bag)
    0
    >>> if a_bag and True:
    ...     True
    ... else:
    ...     False
    ... 
    False

You can even invert them:

    >>> not a_wat
    True
    >>> not not a_wat
    False
    >>> not a_bag
    True
    >>> not not a_bag
    False

Wow! So, this seems pretty great. Are there any pitfalls? Well,
yes. It's documented in the parathetical remark I quoted in the
intro. For `and` and `or` all objects are truthy, and the only truly
falsy values are `False` and `None`. This means `and` and `or` behave
one way for `if` statements, and another way if used by themselves.

Still, this seems like a solid optionality interface with protocols
for both atomic and non-atomic objects. I don't know enough Python to
know how widely this might be used. I looked up some explanations or
tutorials for Null Object patterns using Python, and they talked about
the substitution aspect of them, but not how or why you might want
them to be "falsy".

One last note of appreciation here, Python's "falsification" interface
makes it easy to illustrate a super-interesting language-agnostic
fact: notice how all the falsy types are identity values for
different methods?

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">value</th>
<th scope="col" class="org-left">identity for</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left"><code>False</code></td>
<td class="org-left"><code>or</code></td>
</tr>


<tr>
<td class="org-left"><code>0</code></td>
<td class="org-left"><code>+</code> / <code>-</code></td>
</tr>


<tr>
<td class="org-left"><code>""</code></td>
<td class="org-left"><code>join</code> or <code>+</code></td>
</tr>


<tr>
<td class="org-left"><code>[]</code></td>
<td class="org-left"><code>extend</code></td>
</tr>


<tr>
<td class="org-left"><code>{}</code></td>
<td class="org-left"><code>update</code></td>
</tr>


<tr>
<td class="org-left"><code>Set()</code></td>
<td class="org-left"><code>union</code></td>
</tr>
</tbody>
</table>

This sort of thing has intrigued me for a long time, since this
identity property is often integral to the most elegant ways of
handling optionality in our data. We've already seen a couple of Ruby
examples examples in this discussion so far, but I can assure you,
many more are coming as well as some "big picture" posts about what's
going on.

