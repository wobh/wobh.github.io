---
layout: post
title: "Unicode and Elixir part 2: Where Elixir defines and uses Unicode"
description: "Inventory of Elixir source files where Unicode functions are defined."
categories: uniex
tags: ["elixir", "unicode"]
date: "2016-04-28 12:30:00"
---

Again, in the interest of keeping this light, I'm just going to survey
where String and Unicode-related functionality is defined in Elixir
source.

<!-- more -->

Here's a quick list of files in `elixir/lib/elixir/unicode/`:

-   `CompositionExclusions.txt`
-   `GraphemeBreakProperty.txt`
-   `SpecialCasing.txt`
-   `UnicodeData.txt`
-   `WhiteSpace.txt`
-   `unicode.ex`

The `.txt` files are the unicode data, and `unicode.ex` processes them
into the Elixir String modules for the binary string type. You can
find the those in:

-   `elixir/lib/elixir/lib/string.ex`
-   `elixir/lib/elixir/string/chars.ex`

There's two files of tests for strings and characters:

-   `elixir/lib/elixir/test/elixir/string/chars_test.exs`
-   `elixir/lib/elixir/test/elixir/string_test.exs`

A quick digression: In `string_test.exs`, beginning on line 14, we'll
see this:

~~~~~elixir
# test cases described in http://mortoray.com/2013/11/27/the-string-type-is-broken/
test "unicode" do
  assert String.reverse("noël") == "lëon"
  assert String.slice("noël", 0..2) == "noë"
  assert String.length("noël") == 4

  assert String.length("") == 2
  assert String.slice("", 1..1) == ""
  assert String.reverse("") == ""

  assert String.upcase("baﬄe") == "BAFFLE"

  assert String.equivalent?("noël", "noël")
end
~~~~~

Digression prime: Evidently I'm going to have to find a different
font for this blog, or these posts, or these code blocks, or
something.

If you're reading this and you haven't already read the blog post
mentioned in the comment, [The string type is broken](http://mortoray.com/2013/11/27/the-string-type-is-broken), I recommend doing
so. It outlines the problems with the traditional implementation of
strings as a vector of characters, and provides a rationale for how
strings in Elixir work, and obviously inspired these test cases. I'll
write about it in detail in later posts.

There are two types of string in Elixir, the second type is the is the
"char list" which you can find implemented and tested in:

-   `elixir/lib/elixir/lib/list.ex`
-   `elixir/lib/elixir/lib/list/chars.ex`
-   `elixir/lib/elixir/test/elixir/list_test.exs`
-   `elixir/lib/elixir/test/elixir/list/chars_test.exs`

This pretty much covers the Elixir's strings as written in
Elixir. There's "lower level" concerns in the Erlang sources:

-   `elixir/lib/elixir/src/elixir_bitstring.erl`
-   `elixir/lib/elixir/test/erlang/string_test.erl`

And I think that's it. Next I'll do a little more orienting through
the files in `elixir/lib/elixir/unicode/`. Definitely check out that
blog post in the meantime.
