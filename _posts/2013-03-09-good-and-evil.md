---
layout: post
title: "Good and Evil"
description: ""
category: 
tags: []
date: 2013-03-09 17:19:30 -0800
---

<figure>
<blockquote>
<p><span class="speaker">Faust:</span> <span class="speech">Well now, who are you, then?</span></p>
<p><span class="speaker">Mephisto:</span> <span class="speech">One of the party that ever wishes evil, yet ever yeilds good.</span></p>
</blockquote>
<figcaption>Goethe, <cite>Faust</cite></figcaption>
</figure>

This week at Portland Code School we found ourselves enmired in the
primal struggle of Good against Evil.

It began on Monday, during our pitch session for our team projects,
<a href="http://neilmakn.github.com/">Neilson</a> proposed a <a
href="http://cardsagainsthumanity.com/">Cards Against Humanity</a> app
where people could vote on plays they thought funniest. Just like the
game, but on the web. Our interest was to see what combinations would
prove to be the most popular and we may yet find this out, but not
before learning of some unsettling possibilities.

<!-- more -->

We ended up with other team projects, but the CAH app was popular and
on Tuesday, we discussed using it as a class learning project,
considering how we might create and combine cards in a purely Object
Oriented way using Ruby. On Wednesday morning we considered how we
might store and retrieve cards in a relational database like MySQL.

On Wednesday, for her lightning talk, <a
href="http://fayeishere.github.com/">Faye</a> brought to our attention
the epic fail of Solid Gold Bomb, a company which used an algorithm to
randomly generate slogans for T-shirts and generated several T-shirts
with highly charged sayings. Many people were outraged by the shirts,
the company's response aggravated those who were offended by the
shirts and Amazon.com, their primary outlet, began pulling the
items. The company quickly fell into a downward spiral after that.

On Thursday one of the lightning talks I heard part of was on
virtue. It featured a lot of Rumi quotes, and I can't remember the
details, but something the presenter set in motion a cascade of
thought that carried through to the next day, when the first talk was
on Punk Rock ethos and the Open Source movement.

I doubt I can articulate all the mental/emotional/ethical connections
I made about all this: about civil discourse and common courtesy,
about censorship (including self-censorship) and free expression,
about responsible behavior and "safe spaces," about power and
oppression, about input and output, about transgressive humor and what
it means to be a decent human being. But here's one thing I think
worth considering potentially harmful:

<pre>
  <code class="ruby">
def get_word(dictionary)
  dictionary.sample
end
  </code>
</pre>

Machines are amoral, asocial extensions of ourselves. Even words are
just sounds or figures until they are comprehended by a mind which
then associates them with whatever memories and feelings happen to
give the word meaning for that person. If we ever find ourselves
having to apologize for something the <code>get_word</code> function
returns, we can't blame the <code>sample</code> method. We have to
take responsibility for the <code>dictionary</code> provided. The
possibilities which make it useful, emerge from realities which may be
in the experience of those who use it. Among the many hazards, We may
find ourselves and our software speaking ignorantly on a subject about
which others have painful expertise.
