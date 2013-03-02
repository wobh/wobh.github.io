---
layout: post
title: A couple of minor bugs
date: 2013-02-28 14:33 -0800
---

#### Updates to StarMaze and Wizard's Castle  #

How embarrassing to show off one's code and find bugs right away, but there it was. 

In StarMaze, the <code>show-near</code> function didn't work. This turned out to be do to an experimental feature in the map legend and could be fixed by commenting out the experimental stuff. I'll put it back when the experimental feature are working. 

In Wizard's Castle, the bug was because of whenever the adventurer wielded a new weapon it couldn't generate the event of wielding the weapon. The Wizard's Castle Code is over-designed to the point of silliness, but in this case the problem was simply a couple of spelling mistakes.

Although these are trivial sorts of bugs, I feel there's something sort of serious about them too. We code to reduce things, but reducing multiplies the importance of the remaining things. No conclusion here, but whenever I think about it, I feel like there's a new question or test trying emerge.