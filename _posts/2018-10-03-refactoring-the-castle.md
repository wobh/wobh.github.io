---
layout: post
title: Refactoring the Castle
date: 2018-10-03 22:30 -0500
---

> for each desired change, make the change easy (warning: this may be
> hard), then make the easy change
> 
> —Kent Beck <https://twitter.com/kentbeck/status/250733358307500032?lang=en>

I've been working on [*Wizard's Castle*](https://github.com/wobh/wizards_castle) lately, fixing some bugs and
making some incremental improvements.

Quick background: I "translated" this a few years ago, from the
original Basic to teach myself some things about Common Lisp and about
control and data-flow for simple games, about going from GOTOs to
structures and function calls<sup><a id="fnr.1" class="footref" href="#fn.1">1</a></sup>.

But I keep tinkering with it because &#x2026; I made some mistakes. I
didn't understand how to use some of the language features, reinvented
some wheels, and generally "over-engineered". The temptation to
re-write is strong, as it's a small program and I've learned a lot of
things since.

But it works.

I should say, I have, in fact rewritten a lot of it, in another file,
but that's not a working game. It's better code in most respects,
except the most crucial one. The challenge I see for myself now, is,
how do I go about making it better while conserving it's status as a
working game?

So I've resumed fiddling with it to exercise how to plan progressive
improvements, execute refactoring strategies, and the discipline of
not breaking the game. 

&#x2026; but let's get real. Although I like to imagine this as a
self-improvement effort for developing relevant professional skills, I
have to admit, the way I gravitate to this project is almost certainly
therapeutically motivated.

Consider: A small, low-stakes project, of obscure provenance, with a
personal, nostalgic connection&#x2014;it's not the kind of thing that one
works on when looking forward in life, at least not if they like what
they see ahead.

You've read the news. We'd all rather wander magic castles full of
monsters than do the real world work of making incremental
improvements in the systems we use to distribute justice and mercy in
society. Now doesn't seem like the time to be working on little games.

Still, it's the one area of my life right now where doing a little
thing, for the sake of a little thing, will seem like Enough.

---

<sup><a id="fn.1" href="#fnr.1">1</a></sup> Also because, hilariously, CL is roughly contemporaneous,
although perhaps I should have used Scheme.
