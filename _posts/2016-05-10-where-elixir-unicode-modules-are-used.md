---
layout: post
title: "Unicode and Elixir part 5: Where Elixir Unicode modules are used"
description: "Discovering where and how the unicode modules are used."
categories: uniex
tags: ["elixir", "unicode"]
date: "2016-05-10 16:00:00"
---

Review: here's a list of modules defined in
`elixir/lib/elixir/unicode/unicode.ex`:

-   `String.Unicode`
-   `String.Casing`
-   `String.Break`
-   `String.Normalizer`

What we'll find when we go searching for uses of these modules in the
rest of Elixir is they're cheifly used in
`elixir/lib/elixir/lib/string.ex`.

<!-- more -->

Most uses are simply where `String` uses the macro
[`Kernal.defdelegate/2`](http://elixir-lang.org/docs/stable/elixir/Kernel.html#defdelegate/2) to defer effectively import functions from
another module.

-   263: `defdelegate split(binary), to: String.Break`
-   515: `defdelegate normalize(string, form), to: String.Normalizer`
-   533: `defdelegate upcase(binary), to: String.Casing`
-   551: `defdelegate downcase(binary), to: String.Casing`
-   582: `defdelegate rstrip(binary), to: String.Break, as: :trim_trailing`
-   738: `defdelegate lstrip(binary), to: String.Break, as: :trim_leading`
-   769: `defdelegate trim_leading(string), to: String.Break`
-   799: `defdelegate trim_trailing(string), to: String.Break`
-   1137: `defdelegate codepoints(string), to: String.Unicode`
-   1159: `defdelegate next_codepoint(string), to: String.Unicode`
-   1286: `defdelegate graphemes(string), to: String.Unicode`
-   1325: `defdelegate next_grapheme_size(string), to: String.Unicode`
-   1385: `defdelegate length(string), to: String.Unicode`

If one is just getting started with Elixir this is good look at
another way to share code between modules&#x2013;an alternative to [`import/2`](http://elixir-lang.org/docs/stable/elixir/Kernel.SpecialForms.html#import/2)
or perhaps [`use/2`](http://elixir-lang.org/docs/stable/elixir/Kernel.html#use/2). We can see a couple of examples where some
functions are delegated with an alias.

It's also interesting to note that `String.split/1` delegated to
`String.Break` but `String.split/2` and `String.split/3` are defined
in `String`. If one is getting into Elixir macros, it's informative to
look at source of `Kernal.defdelegate/2`. You'll see it macroexpands
into a pass-through function.

There's a handful of areas where `String.Unicode.split_at/2` are
called directly. Here's a good example to introduce us:

~~~~~elixir
defp do_split_at(string, position) do
  {byte_size, rest} = String.Unicode.split_at(string, position)
  {binary_part(string, 0, byte_size), rest || ""}
end
~~~~~

This private function supports [`String.split_at/2`](http://elixir-lang.org/docs/stable/elixir/String.html#split_at/2). It returns a tuple
of byte size, and the rest of the string beginning at the
position. With these, it constructs a tuple of the first part of the
string to position, and the rest of the string, or an empty
string. This a convenient idiom for recursive functions, like
`String.split_at/2`. If you're new to recursion, check out the source
code for `String.split_at/2`. It's good to note that there's no
information loss in this function&#x2013;the original arguments can be
reconstructed from the output; an important property, although not one
that's directly used in many applications.

The private function `String.do_at/2` supports [`String.at/2`](http://elixir-lang.org/docs/stable/elixir/String.html#at/2), using
`String.Unicode.split_at/2` in a fall-through case&#x2013;determine if it's
reached the end of a string, returning `nil` if so, or the first
character of the rest of the string, if not.

~~~~~elixir
defp do_at(string, position) do
  case String.Unicode.split_at(string, position) do
    {_, nil}  -> nil
    {_, rest} -> first(rest)
  end
end
~~~~~

In [`String.slice/3`](http://elixir-lang.org/docs/stable/elixir/String.html#slice/3) we see `String.Unicode/split_at/2` used for
detecting the end of a string, but also to get the length of the
remaining string after the split.

~~~~~elixir
def slice(string, start, len) when start >= 0 and len >= 0 do
  case String.Unicode.split_at(string, start) do
    {_, nil} -> ""
    {start_bytes, rest} ->
      {len_bytes, _} = String.Unicode.split_at(rest, len)
      binary_part(string, start_bytes, len_bytes)
  end
end
~~~~~

In [`String.slice/2`](http://elixir-lang.org/docs/stable/elixir/String.html#slice/2), the use is very similiar but without an additional
split required.

~~~~~elixir
def slice(string, first..-1) when first >= 0 do
  case String.Unicode.split_at(string, first) do
    {_, nil} -> ""
    {start_bytes, _} ->
      binary_part(string, start_bytes, byte_size(string) - start_bytes)
  end
end
~~~~~

Lastly, here's where [`String.capitalize/1`](http://elixir-lang.org/docs/stable/elixir/String.html#capitalize/1) calls upon
`String.Casing.titlecase_once/1`:

~~~~~elixir
def capitalize(string) when is_binary(string) do
  {char, rest} = String.Casing.titlecase_once(string)
  char <> downcase(rest)
end
~~~~~

This looks simple enough, that, for the next post I'll unpack how it's
implemented, here, in `String.Casing`, and it's tests. It'll be a
good, simple introduction to a Unicode feature in Elixir.

##### Postscript on bodiless functions

While researching this entry, I noticed that several string functions
had bodiless function heads, preceding bodied function clauses, for
example:

~~~~~elixir
def jaro_distance(string1, string2)
~~~~~

I knew about how that's a necessity for functions with [more than one
default argument](http://elixir-lang.org/getting-started/modules.html#default-arguments), but some of these, like `String.slice/2` and
`String.printable?/1`, and `String.jaro_distance/2` didn't have any
default arguments. I couldn't figure it out for quite a while, but
while inquiring about it on the Slack Elixir group I noticed that the
commit message for `String.printable?/1` explained it:

So the reason for this is documentary, so that documentation
generators can print the function with sensible argument names were
none may exist in the following definitions.
