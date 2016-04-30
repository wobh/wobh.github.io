---
layout: post
title: "Unicode and Elixir part 3: Elixir's Unicode source"
description: "Inventory of Elixir source files where Unicode functions are defined."
categories: uniex
tags: ["elixir", "unicode"]
date: "2016-04-30 12:30:00"
---

Today we're going to survey the core Unicode source in Elixir. Again,
I'm keeping it very light. I realize we're very much in the "shallow
end of the pool", as it were, and I think that's okay. The topic is
big and complex, the code and language are still new to me. I'm aiming
for posts of about 500 to 1000 words and by controlling the size of
these posts, I hope to limit their complexity when we get to the
highly technical stuff. For now, we take the time to get to know the
major features and landmarks.

<!-- more -->

Review: here's a list of files in `elixir/lib/elixir/unicode/`:

-   `CompositionExclusions.txt`
-   `GraphemeBreakProperty.txt`
-   `SpecialCasing.txt`
-   `UnicodeData.txt`
-   `WhiteSpace.txt`
-   `unicode.ex`

The `.txt` files are copied from or derived from the Unicode Character
Database (UCD). We'll look at them in detail soon. For now we'll focus
on `unicode.ex` which defines some string modules, and generates data
from the `.txt` files.

-   `String.Unicode`
-   `String.Casing`
-   `String.Break`
-   `String.Normalizer`

Being internal, these modules aren't really documented. That said,
although obscure to use we can make some guesses about them. The
public functions defined in each of these modules looks like this:

-   `String.Unicode`
    -   `version/0`
    -   `next_grapheme_size/1`
    -   `graphemes/1`
    -   `length/1`
    -   `split_at/2`
    -   `next_codepoint/1`
    -   `codepoints/1`

-   `String.Casing`
    -   `downcase/1`
    -   `upcase/1`
    -   `titlecase_once/1`

-   `String.Break`
    -   `trim_leading/1`
    -   `trim_trailing/1`
    -   `split/1`
    -   `decompose/2`

-   `String.Normalizer`
    -   `normalize/2`

-   top level
    -   `to_binary/1`

This clarifies each module's domain a bit. We'll discuss each in more
detail in another post. Lets look at how the `.txt` files in the
folder are used in the modules of this file:

-   `String.Unicode`
    -   uses `GraphemeBreakProperty.txt` to create variable `cluster`

-   `String.Casing`
    -   uses `SpecialCasing.txt` to create variable `codes`

-   `String.Break`
    -   uses `WhiteSpace.txt` to create variable `whitespace`

-   `String.Normalizer`
    -   uses `CompositionExclusions.txt` to create variable `compositions`

-   top level
    -   uses `UnicodeData.txt` to create variables:
        -   `codes` used by `String.Casing`
        -   `non_breakable` used in `String.Break`
        -   `decompositions` used by `String.Normalizer`
        -   `combining_classes` used by `String.Normalizer`

As we look through the file I see one thing that suggests reviewing
Elixir's scoping rules would be a good idea: the top level `codes` is
shadowed after it's use in `String.Casing`, line 299.

~~~~~elixir
codes = Enum.reduce File.stream!(special_path), codes, fn line, acc ->
  # rest omitted
end
~~~~~
Here's a good document on Elixir scoping:

<http://elixir-lang.readthedocs.io/en/latest/technical/scoping.html>

A post on how Elixir variables:

<http://blog.plataformatec.com.br/2016/01/comparing-elixir-and-erlang-variables/>

Next: lets look at the Unicode data files, their structures and
purposes.
