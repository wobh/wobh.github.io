---
layout: post
title: "Retrospective of a trivial bugfix"
date: "2017-06-01 11:00:00"
---

My first programs were games<!-- more -->, meticulously typed in from
magazines and books. I didn't understand what I was typing, and, as
code goes, these programs were difficult to understand. My family's
first computer was the Commodore Vic 20 and the resource constraints
on that platform were what we would consider "severe"
nowadays. Programmers wrote "spaghetti code", so called because the
control flow of these programs was a tangle of `GOTO` and `GOSUB`
jumps with only a few function calls. As for data flow, most variables
were global; only the processor's attention was managed.

Code in books and magazines was roughly optimized to reduce the amount
of typing you would have to do perfectly to have a hypothetically
working program, when done. It was not optimized to reduce debugging
either transcription errors or bugs. However, one further optimization
was available for the novice programmer back then: don't bother typing
in lines that start with `REM`: those are comments.

In a way, not much has changed since then, except the power to fool
ourselves into thinking the computer works differently is vastly
greater.

What I like about revisiting these old programs, is they're
complicated enough to have required "design" compromises to support
the platforms they run on, but not so ambitious that they're
functionality can't be comprehended with a little study. I like
translating them into "modern" languages as a learning exercise but
also as an exercise in design.

Here's some setup code from the classic game, _Wumpus_. It sets up a
network of caverns as an table whose row index is the room number (1-20)
and whose column index is the passages to other rooms.

~~~~~basic
85 dim s(20,3)
90 for j = 1 to 20
95 for k = 1 to 3
100 read s(j,k)
105 next k
110 next j
115 data 2,5,8,1,3,10,2,4,12,3,5,14,1,4,6
120 data 5,7,15,6,8,17,1,7,9,8,10,18,2,9,11
125 data 10,12,19,3,11,13,12,14,20,4,13,15,6,14,16
130 data 15,17,20,7,16,18,9,17,19,11,18,20,13,16,19
~~~~~

`Region has 10 lines, 96 words, and 279 characters.`

Modern languages have modern conveniences we can use for "code golf"
and chief among these are structure literals; for example, in Clojure:

~~~~~clojure
(def s
  [[2 5 8] [1 3 10] [2 4 12] [3 5 14] [1 4 6]
   [5 7 15] [6 8 17] [1 7 9] [8 10 18] [2 9 11]
   [10 12 19] [3 11 13] [12 14 20] [4 13 15] [6 14 16]
   [15 17 20] [7 16 18] [9 17 19] [11 18 20] [13 16 19]])
~~~~~

`Region has 5 lines, 62 words, and 215 characters.`

This isn't just briefer, the structure is created at the time the
characters are read in. We've automated the looping initialization
code and eliminated a whole slew of potential bugs and errors.

Now, _Wumpus_, compared to some other early games I've read and
adapted, is well designed and written---much moreso than any of my
adaptions. It's humbling to think, despite my best efforts and
resources, I have made a hash of things, while trying to untangle
them.

I recently fixed this bug in my adaptation of _Wumpus_ to Common Lisp:

https://github.com/wobh/wumpus/commit/40731748af3b5ad18bc4dc9da46046b526033ad6

This code was written a few years ago when I was first learning CL, in
an effort to figure out some stuff about "declarative" and "eventful"
programming techniques I had been reading about---one reason why it's
such an overwrought mess. Clearly, in changing the name of something,
I overlooked the ONE place that name is actually used. _Quelle
Embarrassment!_

On one hand, it was a dumb mistake. Overthinking this program has
caused enough problems;
moreover
[greater minds than mine have thought about this already](https://vimeo.com/10837903). On
the other hand, every technique I can think of to have signaled a
problem earlier is something significant added either in time (play
testing) or space (tests, types, assertions). As far as I know,
nothing remotely like the structure literal code above exists for this
sort of thing. As Jim Weirich mentions in the video I linked above,
names of one sort or another are foundational to all abstraction.

Of the code level fixes, types seem like they're the least
intrusive. And, of course, it's wise for programmers in cooperative
project to exercise defensive skepticism about the symbols they
use. But fundamentally, I failed to really implement the kind of
"eventful" system I had desired. This function is exercising a
control---a conditional check which should have been the event
system's job. Fundamentally, this bug arose because I was trying to
write something profoundly different, and I didn't know how.

What would make this worth trying again? That's the sort of question
I'm asking myself. The days are long gone when common systems were so
resource poor that such "imperitive" programming like the original
_Wumpus_ game were necessary. Certainly I need learn and practice
the techniques for writing programs that "walk and talk at the same
time" in in non-procedural domains.

Perhaps the real question is how do I make myself more concerned about
more practical problems?
