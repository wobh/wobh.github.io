---
layout: post
title: "Optionality in Common Lisp: Generalized Booleans"
tags: ["optionality", "common-lisp"]
date: "2022-05-11 19:00-0500"
---

Common Lisp has one "falsy" value: `nil`. Every non-`nil` value is
considered "truthy", but there is also a "true" value, `t`. This union
of two-valued "Boolean" logic and a more generic "nothing" vs "something"
logic is called the "generialized boolean".

There's one little wrinkle to this particular to Common Lisp, which is
that `nil` and the empty list `()` are considered the same
value. Mostly, this is a great convenience when dealing with
lists. Let's explore some other consequences.

<!-- more -->

One consequence is that there are two different functions which all,
basically do the same thing, and the main reason for chosing one over
the other has to do with the semantics of one's code.

<dl>
  <dt><code>not : generalized-boolean -> boolean</code></dt>
  <dd>used for logical inversion</dd>
  <dt><code>null : object -> generalized-boolean</code></dt>
  <dd>used for detecting <code>nil</code> values</dd>
</dl>

There's also `endp` which is strict about its input and does a little
extra work so it works with circular lists.

<dl>
  <dt><code>endp : list -> generalized-boolean</code></dt>
  <dd>used for detecting the end of a list</dd>
</dl>

It's interesting to think about `atom` in this context too, which
returns `t` for all non `cons` types. And if we're thinking of `atom`
we should probably consider `consp` and `listp` too. They all have the
same signature:

-   `atom : object -> generalized-boolean`
-   `consp : object -> generalized-boolean`
-   `listp : object -> generalized-boolean`

Here's a table showing the mapping of inputs and outputs:

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">
  <colgroup>
    <col class="org-left" />
    <col class="org-left" />
    <col class="org-left" />
    <col class="org-left" />
    <col class="org-left" />
  </colgroup>
  <thead>
    <tr>
      <th class="org-left">function</th>
      <th class="org-left"><code>nil</code></th>
      <th class="org-left"><code>'(foo bar)</code></th>
      <th class="org-left"><code>t</code></th>
      <th class="org-left"><code>'foo</code></th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td class="org-left"><code>not</code></td>
      <td class="org-left"><code>t</code></td>
      <td class="org-left"><code>nil</code></td>
      <td class="org-left"><code>nil</code></td>
      <td class="org-left"><code>nil</code></td>
    </tr>
    <tr>
      <td class="org-left"><code>null</code></td>
      <td class="org-left"><code>t</code></td>
      <td class="org-left"><code>nil</code></td>
      <td class="org-left"><code>nil</code></td>
      <td class="org-left"><code>nil</code></td>
    </tr>
    <tr>
      <td class="org-left"><code>endp</code></td>
      <td class="org-left"><code>t</code></td>
      <td class="org-left"><code>nil</code></td>
      <td class="org-left">error</td>
      <td class="org-left">error</td>
    </tr>
    <tr>
      <td class="org-left"><code>atom</code></td>
      <td class="org-left"><code>t</code></td>
      <td class="org-left"><code>nil</code></td>
      <td class="org-left"><code>t</code></td>
      <td class="org-left"><code>t</code></td>
    </tr>
    <tr>
      <td class="org-left"><code>consp</code></td>
      <td class="org-left"><code>nil</code></td>
      <td class="org-left"><code>t</code></td>
      <td class="org-left"><code>nil</code></td>
      <td class="org-left"><code>nil</code></td>
    </tr>
    <tr>
      <td class="org-left"><code>listp</code></td>
      <td class="org-left"><code>t</code></td>
      <td class="org-left"><code>t</code></td>
      <td class="org-left"><code>nil</code></td>
      <td class="org-left"><code>nil</code></td>
    </tr>
  </tbody>
</table>

There's system classes which these predicate functions
`null`, `consp`, `atom`, and `listp` are coorespondents:

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">
  <colgroup>
    <col class="org-left" />
    <col class="org-left" />
  </colgroup>
  <thead>
    <tr>
      <th scope="col" class="org-left">&#xa0;</th>
      <th scope="col" class="org-left">equiv</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td class="org-left"><code>null</code></td>
      <td class="org-left">&#xa0;</td>
    </tr>
    <tr>
      <td class="org-left"><code>cons</code></td>
      <td class="org-left">&#xa0;</td>
    </tr>
    <tr>
      <td class="org-left"><code>atom</code></td>
      <td class="org-left"><code>(not cons)</code></td>
    </tr>
    <tr>
      <td class="org-left"><code>list</code></td>
      <td class="org-left"><code>(or null cons)</code></td>
    </tr>
  </tbody>
</table>

This might seem like a snarl of logic, which is unfortunate, I don't
know if I can convey quite what's awesome about it. The True and False
of classical Boolean logic are represented by `t` and `nil`, but these
two values are members of the types `t` and `null`. The `null` type
only has one member, `nil`, but the `t` type has `t` and everything
else. This is the basis of the generalized logic of "nothing" vs
"something".

Now I feel like I have run out of space to discuss "conses" and the
construction of lists and trees, and `nil`'s double life as an empty
list. But if you've read the other articles in this series the concept
of optionality being represented by the identity element should have
been introduced, and that concept is, of course true in Common Lisp,
as well as everything else. If you're dealing with `cons` objects any
function of `cons` works with `nil` too.

There's much more to say, of course, but it'll have to wait for
another post.
