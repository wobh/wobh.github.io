---
layout: post
title: "Unicode and Elixir part 4: Unicode data files"
description: "Examining structure and data in the Unicode data files."
categories: uniex
tags: ["elixir", "unicode"]
date: "2016-05-04 22:00:00"
---

Unicode is a big catalog of numbers and relationships between that
ideally describes human written communication in as many languages as
possible. Elixir implements a segment of it in it's String type. For
that it relies on a few data files drawn from or derived by the
Unicode Character Base.

<!-- more -->

The datafiles with the Elixir modules are in
`elixir/lib/elixir/unicode/`:

-   `CompositionExclusions.txt`
-   `GraphemeBreakProperty.txt`
-   `SpecialCasing.txt`
-   `UnicodeData.txt`
-   `WhiteSpace.txt`
-   `unicode.ex`

The file `unicode.ex` defines the Elixir modules we looked at in the
last post. The data files are the rest. You can find these files
described in detail in the standards document "Unicode Standard Annex
\\#44: Unicode Character Database" found here:

-   <http://www.unicode.org/reports/tr44/>

A few words on conventions found in these files.

-   `#` precedes comments, everything following should be ignored
-   `;` separates columns
-   hex numbers from `0000` through `FFFF` are code points
-   `..` between two code points indicates a code point range, for
    example `0000..FFFF`
-   a code point sequence separates each code point with a space, for
    example `0001 0003 0005 0007`

More details here: <http://www.unicode.org/reports/tr44/#Format_Conventions>

# `CompositionExclusions.txt`<a id="orgheadline1"></a>

This file documents different categories of composition
exceptions. The format is a single column containing a code point or
code-point range and a comment. For single codes the comment is just
the name. For code ranges it's the range-size, and the names of the
first and last characters in the range.

    <code> # <comment: name>
    <code>..<code> # <comment: range-size name..name>

It is unchanged from the one in the UCD here:

<http://www.unicode.org/Public/UCD/latest/ucd/CompositionExclusions.txt>

A lot of the entries are commented out. I'll get into compositions and
their complications in future posts. In the meantime they are
described in "Unicode Standard Annex #15: Unicode Normalization Forms"
here:

-   <http://www.unicode.org/unicode/reports/tr15>

# `GraphemeBreakProperty.txt`<a id="orgheadline2"></a>

The file is mostly the same as the UCD version, but with comments and
blanks stripped out. It basically describes boundaries between
characters. Of course, necessary for counting letters, splitting, and
getting the next character from a string.

You can find the UCD file here:

-   <http://www.unicode.org/Public/UCD/latest/ucd/auxiliary/GraphemeBreakProperty.txt>

The format of the file is two columns and a comment. The first column
is a code point, or code point range. The second is a property
type. For a single charater the comment gives a category code and the
character name. For ranges, the comment gives a category code, the
range size and the first and last character names of the range
separated with the range separator.

    <code>; <property> # <comment: category name>
    <code>..<code>; <property> # <comment: category range-size name..name>

There are tests data here:

-   <http://www.unicode.org/Public/UCD/latest/ucd/auxiliary/GraphemeBreakTest.txt>

The Unicode document describing this and other types of breaks is
"Unicode Standard Annex #29: Unicode Text Segmentation" and can be
found here:

-   <http://www.unicode.org/reports/tr29/tr29-27.html>

# `SpecialCasing.txt`<a id="orgheadline3"></a>

Likewise, this file is the same as can be found in the UCD, but with
comments and blank lines stripped out. This file contains the data
necessary for case changes.

-   <http://www.unicode.org/Public/UCD/latest/ucd/SpecialCasing.txt>

Its format is four columns of code points, or code point lists
followed by one column of optional conditions, followed by a comment
with the character name. It's described in the UCD file like this:

    <code>; <lower>; <title>; <upper>; (<condition_list>;)? # <comment: name>

# `WhiteSpace.txt`<a id="orgheadline4"></a>

A comment in Elixir source here
`elixir/lib/elixir/unicode/unicode.ex:359` describes this as derived from
this UCD file of properties:

-   <http://www.unicode.org/Public/UCD/latest/ucd/PropList.txt>

It is only the whitespace properties, and is in the format of:

    <code> ; <property> # <comment: category name>
    <code>..<code> ; <property> # <comment; category range-size name>

Character properties are described in "Unicode Technical Report #23:
The Unicode Character Property Model":

-   <http://www.unicode.org/reports/tr23/>

and "Unicode Standard Annex #44: Unicode Character Database"

-   <http://www.unicode.org/reports/tr44/#PropList.txt>

# `UnicodeData.txt`<a id="orgheadline5"></a>

This is the big file of Unicode character definitions. It's in the
format of:

    <code>;<category>;<canonical-combining-class>;<bidi-class>;<decomposition <type> mapping>;<numeric-decimal-value>;<numeric-digit-value>;<numeric-numeric-value>;<bidi-mirrored>;<unicode-1-name>;<iso-comment>;<simple-uppercase-mapping>;<simple-lowercase-mapping>;<simple-titlecase-mapping>

More about each of the columns here:

-   <http://www.unicode.org/reports/tr44/#UnicodeData.txt>

Beginning on `elixir/lib/elixir/unicode/unicode.ex:248` you can see
where this file is parsed into data for use by the unicode
modules. Note that most of the feilds are ignored.

Next were going to take a look at where the unicode modules are used,
in Elixir and how.
