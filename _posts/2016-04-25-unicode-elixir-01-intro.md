---
layout: post
title: "Unicode and Elixir, part 1: Introduction"
description: ""
category: uniex
tags: [elixir, unicode]
date: "2016-04-25 18:15:00"
---

For the next few posts, at least, I'm going to write about Unicode,
how it's implemented and used in Elixir. I intend to keep these posts
shorter than the last series so I can keep it much more lightweight
and regular. However, I'm a pretty ambitious guy, and I like getting
carried away with things, also, the scope of this extends in two
directions with a lot of ground, at the same time, personally, there's
a lot of "life happens" stuff happening, so &#x2026; we'll see.

At first though, lets get oriented. Here's a few resources we're going
to get familiar with as we go along.

From the [Elixir Getting started](http://elixir-lang.org/getting-started/introduction.html) book [Chapter 6: Binaries, strings and
char lists](http://elixir-lang.org/getting-started/binaries-strings-and-char-lists.html). Here you can read about how strings in Elixir
work. There's two kinds, but I expect to focus on strings as binaries,
rather than as char lists.

From [Elixir's docs](http://elixir-lang.org/docs.html), the [String module documentation](http://elixir-lang.org/docs/stable/elixir/String.html) will be our
reference for the different operations on Elixir's strings. Other
module documentation of note: [Enum](http://elixir-lang.org/docs/stable/elixir/Enum.html) and possibly [StringIO](http://elixir-lang.org/docs/stable/elixir/StringIO.html).

From [Elixir's source](https://github.com/elixir-lang/elixir), we'll find most of what we're interested in
[lib/elixir/unicode](https://github.com/elixir-lang/elixir/tree/master/lib/elixir/unicode) folder.

From <http://unicode.org> we'll want to get up to speed on the unicode
specification. Elixir is currently on [Unicode 8.0.0](http://www.unicode.org/versions/Unicode8.0.0/), but [Unicode 9.0.0](http://www.unicode.org/versions/Unicode9.0.0/)
is coming out June 2016, with a bunch of new characters and important
other changes.

And that last bit gets to heart of why I'm writing this. How does the
a project like Elixir implement and keep up with a large and
complicated standard like Unicode? Unicode as a standard of character
representation and Elixir as a language for humans and computers to
share, occupy an intersection where the crosswinds of
mathematical numbers and strings with thousands of years of human
traditions around reading and writing stir and mix. If find this
negotiated territory between computation and communication with all of
the conflicts and compromises very interesting. If you do too, please
follow along!
