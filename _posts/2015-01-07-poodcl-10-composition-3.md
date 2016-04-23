---
layout: post
title: "POODCL part 10: Composition, part 3"
description: ""
category: poodcl
tags: [common-lisp, clos, poodr]
date: 2015-01-07 09:38:38
---

During my investigation of the composite pattern, the following
message came through loud and clear: 

> Prefer composition to inheritance.

And one of the things I came to wonder about was, if composition is to
be preferred, why isn't there better support for it in object oriented
languages? I had a few thoughts about this. Most of them sort of
wrong, but here's what I learned.

<!-- more -->

Consider functions: functions compose with little very little ceremony:

~~~~~common-lisp
(defun compose-f (f g)
  (lambda (x)
     (funcall f (funcall g))))
~~~~~

Composing classes seems to require a significant amount of
architecture. As we saw in the last post, we couldn't even do it in a
general way without adding some macro support that I hadn't yet come
up with and still haven't come up with anything more satisfactory than
the `COMPONENT` and `COMPOSITE` mixin classes from the last entry. 

I had a conversation with a coworker on this topic and he suggested
that I was thinking about composition too formally: that it's a design
criterion, not a model. I take his point. 

However, In doing some further research in this area, I learned that
the Go language [golang.org](http://golang.org/) was designed with
preference to composition. I haven't yet made time for the study of
this I would like, but here are some resources I first turned up in
searching for materials on the subject.

-   <http://talks.golang.org/2012/splash.article#TOC_15>.
-   <http://research.swtch.com/interfaces>
-   <http://golang.org/doc/effective_go.html#interfaces>

So, although I'm not at all done with this topic, I'm putting off,
further research in order to move on.
