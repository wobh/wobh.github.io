---
layout: post
title: "Optionality and Shell Variable Expansion"
date: "2021-06-28T10:00-2210"
tags: ["optionality", "bash"]
---

Recently I've had to recreate my command-line environment at work,
and, naturally enough, this has lead to some reforms in my home
configuration as well. One of the things I made use of was the
parameter expansion features which I want to talk about.

If you aren't familiar with the parameter expansion feature you should
definitely check out this section of the Bash manual:

<https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html>

There's more info and usage suggestions from "Advanced Bash-Scripting
Guide" here:

<https://tldp.org/LDP/abs/html/parameter-substitution.html>

<!-- more -->

There are four basic operations to talk about, here's how they're
described in the Bash manual:

<blockquote>
  <dl>
    <dt><code>${<var>parameter</var>:-<var>word</var>}</code></dt>
    <dd>
      <p>If <var>parameter</var> is unset or null, the expansion of
        <var>word</var> is substituted. Otherwise, the value of
        <var>parameter</var> is substituted.</p>
    </dd>
    <dt><code>${<var>parameter</var>:=<var>word</var>}</code></dt>
    <dd>
      <p>If <var>parameter</var> is unset or null, the expansion
        of <var>word</var> is assigned to <var>parameter</var>.  The
        value of <var>parameter</var> is then substituted.  Positional
        parameters and special parameters may not be assigned to in this
        way.</p>
    </dd>
    <dt><code>${<var>parameter</var>:?<var>word</var>}</code></dt>
    <dd><p>If <var>parameter</var> is null or unset, the expansion
        of <var>word</var> (or a message to that effect
        if <var>word</var> is not present) is written to the standard
        error and the shell, if it is not interactive, exits.
        Otherwise, the value of <var>parameter</var> is substituted.</p>
    </dd>
    <dt><code>${<var>parameter</var>:+<var>word</var>}</code></dt>
    <dd><p>If <var>parameter</var> is null or unset, nothing is
        substituted, otherwise the expansion of <var>word</var> is
        substituted.</p>
    </dd>
  </dl>
</blockquote>

In my new setup I used a couple of these to ensure that my
`XDG_{CONFIG,DATA}_HOME` variables<sup><a id="fnr.1" class="footref" href="#fn.1">1</a></sup> were set in a couple of
contexts where it would bad if they weren't. For example, I want to
source a configuration file in my config home. I have to bootstrap the
variable assignment which I do like this:

    [ -f "${XDG_CONFIG_HOME:=$HOME/.config}/sh/include" ] &&\
        source "$XDG_CONFIG_HOME/sh/include"

If the variable is undefined, we define it. If the file exists it is
sourced, otherwise, nothing happens. Now, I'm not sure what the scope
of this definition is. Whatever it is, in the included configuration
the variables get defined and exported as you might expect:

    [ -z "${XDG_CONFIG_HOME}" ] &&\
        export XDG_CONFIG_HOME="$HOME/.config"

In another use-case I have to reference a location `$XDG_DATA_HOME`
and I want to make sure, in this instance, if the variable is
undefined, the script fails. Unfortunately, the file with this
reference is included in by software I'm using but don't control, and
it's not safe to use the directive `set -o nounset` / `set -u` since
that causes an irrelevant error to be raised. Here `:?`
comes in handy, and it looks something like this:

    setup_wat() {
        local wat_dir="${XDG_DATA_HOME:?is undefined}/wat"
    
        # rest of wat setup...
    }

And this works out great since I'm only interested in making this
particular function fail if the variable is undefined or null, and I
really don't want it to fallback to a default value. It's not this
function's job to make assumptions about resources it expects from the
environment.

I don't have examples for `:-` and `:+`, but they're basic enough,
merely providing an alternate value and forgoing side effects like
assigning variables or interrupting the program. Also, they're
inverses of each other.

It's interesting that these operators come in two modes: one for when
the variable is unset, one for which it's either unset or null, which,
in Bash, the null value is an empty string. This aspect of "falsiness"
in optional values is a topic I want to write more about later<sup><a id="fnr.2" class="footref" href="#fn.2">2</a></sup>.

When I think about it, these are the three basic behaviors I expect
for variables in any computing environment that may be unset or null:

1.  provide alternative value
2.  assign default value
3.  raise an error

I'm calling this basic notion "optionality" because it seems fun,
although I expect there's a proper name for it. That's all for now. I
wrote this to express some appreciation for this syntax in Bash. I
think it's pretty interesting and useful and it's inspired some other
thoughts about optionality I'll be writing about soon<sup><a id="fnr.3" class="footref" href="#fn.3">3</a></sup>.

Thanks for reading!


# Footnotes

<sup><a id="fn.1" href="#fnr.1">1</a></sup> I intend to talk about the XDG base directory spec "soon", but
in the meantime:
<https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html>

<sup><a id="fn.2" href="#fnr.2">2</a></sup> Probably won't be a very exciting post, but checkout the
definitions of `-z` and `-n` here:
<https://www.gnu.org/software/bash/manual/html_node/Bash-Conditional-Expressions.html>

<sup><a id="fn.3" href="#fnr.3">3</a></sup> Gosh, I sure am making a lot of promises to write more in this
first post in quite a while.
