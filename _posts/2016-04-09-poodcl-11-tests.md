---
layout: post
title: "POODCL part 11: Designing effective tests"
description: ""
category: 
tags: [lisp, clos, poodr]
date: "2016-04-09 15:30:00"
---
{% include JB/setup %}

This has been a long time in coming and this post is long in the
reading. Chapter 9, the last chapter of POODR, is called "Designing
Cost-Effective Tests" and might be my favorite chapter. 

We start with a summary of the three basic skills required for writing
well designed, easily changeable code.

1.  Design code with the principle of changeability. "Poorly designed
    code is naturally difficult to change. From a practical point of
    view, changeability is the only design metric that matters; code
    that's easy to change *is* well designed."
2.  Refactor existing code to improve it's design. "Good design
    preserves maximum flexibility at minimum cost by putting off
    decisions at every opportunity, deferring commitments until more
    specific requirements arrive. When that day comes, *refactoring* is
    how you morph current code structure into one that accomodates the
    new requirements."
3.  Write high value tests. "Tests give you confidence to refactor
    constantly. Efficient tests prove that altered code behaves
    correctly without raising overall costs. Good tests weather code
    refactorings with aplomb; they are written such that changes to the
    code do not force rewrites of the tests." (pg, 192)

In the earlier chapters we've been rehearsing the first two. In the
last we begin work on the third. Let's see how to write high value
tests in Common Lisp.

<!-- more -->

As in Ruby there are quite a few testing suites for Common Lisp. This
overview is from 2007 but reviewing a few of the tests suites,
mentioned it still seems relevant in 2016.

<http://aperiodic.net/phil/archives/Geekery/notes-on-lisp-testing-frameworks.html>

I'm not going to use any testing suite. I'm taking this occasion to
show a more primitive testing methodology, useful for "greenfield",
"spiking", and even certain kinds of refactoring. We observe the same
principles mentioned throughout this chapter, but observe thay they
require very few formalities. We just need Lisp (or Ruby, or Python,
or whatever).

#### Identifying intentions


I wish I could quote the entire section "Intentional testing". You'll
have to be content with this summary.

Tests should serve to reduce costs. Many new programmers and
experienced programmers who are new to testing, stuggle to write
effective and efficient tests. "Getting good value from tests requires
clarity of intention and knowing what, when and how to test."

You may intend any combination of several things in testing:

-   finding and preventing bugs
-   documenting interfaces and features
-   deferring design decisions
-   supporting abstractions
-   exposing design flaws

Some notes on the last three:

In order to use defer design decisions, tests must be oriented around
interfaces. Whenever a point of ambiguity emerges, as long as the the
interface is stable, you can hide your hacks around the ambiguity
behind the interface. When the ambiguity resolves the interface and
tests should be largely unaffected by the changes you make.

This process emerges into the supporting abstraction intention. 

> Good design naturally progresses toward small independent objects that
> rely on abstractions. The behavior of well-designed applications
> gradually becomes the result of the interaction among the
> abstractions.

Metz goes on to discuss the costs of this, that as the abstractions
and interactions increase in number and complexity, it becomes
increasingly difficult to understand. In this environment, tests
become vital.

This emerges into the last intention. If a test is difficult to write,
it is likely that the design of the abstractions and interactions is
wrong. "Tests are the canary in the coal mine; when the design is bad,
testing is hard."

To gain all the benefits of tests for the least cost, you must "write
loosely coupled tests about only the things that matter."

#### Discovering what matters


Much of Metz's discussion here uses the "message-passing" metaphor of
OOP, which I didn't understand very well for a long time, and even now
the concept is foreign to my thinking about programming. However, the
principles in this section don't require it.

-   test everything, once, in the proper place.
-   test intentionally.
-   do not entangle the test with the test subject.
-   test public interfaces, ignore private ones.

Metz distinguishes a "query" as a function or method whose return
value is of primary importance, from a "command" which is a function
or method that has a side effect or change of state that is of primary
importance. This breaks down into the following

-   "Incoming messages should be tested for the state they return."
-   "Outgoing command messages should be tested to ensure they get
    sent."
-   "Outgoing query messages should not be tested."

This is expanded on in one of Metz's talks, the slides of
which can be found here:

<https://speakerdeck.com/skmetz/magic-tricks-of-testing-railsconf>

We're going to review some of the issues in that talk, and for which
I'm copying this table:

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">message</th>
<th scope="col" class="org-left">query</th>
<th scope="col" class="org-left">command</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">incoming</td>
<td class="org-left">Assert result</td>
<td class="org-left">Assert direct, public side effects</td>
</tr>


<tr>
<td class="org-left">sent to self</td>
<td class="org-left">Ignore</td>
<td class="org-left">Ignore</td>
</tr>


<tr>
<td class="org-left">outgoing</td>
<td class="org-left">Ignore</td>
<td class="org-left">Expect to send</td>
</tr>
</tbody>
</table>

What does this mean for Common Lisp code?

Firstly, we can easily adapt the distinction between a stateless
function ("query") which has no side effects and only returns a value,
and a procedure ("command") which has a side effect, and may not
return anything relevant.

Secondly we should consider the kinds of the tests in discussion. To
"assert" here is to test that a function or proceedure returns a
particular value. To "expect" is to test that a function or procedure
is called under the assumed circumstances. Assertions are easily
enough written in native Common Lisp. 

However, the "incoming/internal/outgoing" distinction is going to
require some creativity. Consider this simple class and method:

~~~~~common-lisp
(defclass wheel ()
  ((rim  :initarg :rim  :reader rim)
   (tire :initarg :tire :reader tire)))

(defmethod diameter ((w wheel))
  (+ (rim w) (* 2 (tire w))))
~~~~~

In the Ruby version, `diameter` would be considered "incoming" and
despite inverted sense of this in CLOS, it remains sensible to test
it's correctness by testing it's return value.

We could likewise write tests for the reader methods `rim` and `tire`
and any other accessor methods which get automatically generated by
`defclass`, however it would be more like testing CLOS rather than the
`wheel` class, so we should consider them "internal" and figure out
other things to test.

Now let's look at this class and method:

~~~~~common-lisp
(defclass gear (observable)
  ((cog :initarg :cog :accessor cog)))

(defmethod (setf cog) :after ((gear gear) (cog integer))
  (dolist (observer (observers gear))
      (observes observer 
              (make-instance 'observation
                               :obj gear
                               :changed :cog
                               :to cog))))
~~~~~

Here we've setup an ancillary `:after` method to the `(setf cog)`
method. We use these to define what to notify observers about changes
to a `gear` object's `cog` slot. The implementation of the
`observable` mixin class, it's `observers` method, and the `observes`
method of an `observer` object, and the `observation` class, are left
as an exercise to the reader.

It's trivial to test if a gear object is a member of
`observable`. Metz suggests that `observes` and `observer` ought to
have their own tests, so preparing an observer for testing the
observability of gears is testing the wrong thing. So, the test case
for the observability of gears and cogs must depend on our desire that
`(setf cog)` actually triggers a call the `observe` method. Since
calling `observe` is a side-effect of the change in value, we don't
care about the return value of either `observe` or `(setf cog)`&#x2013;in
fact, we won't know what the return value of the ancillary method is,
when we call `(setf cog)` we'll only ever get what `(setf cog)`
returns.

To write this expectation we'll have to: 

1.  Verify that `observes` exists as a method for `observer` objects.
2.  Shadow the `observes` method with a definition that we can check
    the outcome of.

We don't want to do anything further as we're on the line of "testing
implementation rather than behavior". Even so, we specified that
`gear` was observable, we should make sure that what we want observed
is observed.

How do we do this?

For number 1 we can count on the system raising an error if the
`observes` method doesn't exist. For number 2:

~~~~~common-lisp
(let (outcome)
  (flet ((observes (observer &key &allow-other-keys)
             (setf outcome t)))
      (let ((gear (make-instance 'gear :cog 11 
                                       :observers (list '()))))
      (setf (cog gear) 12)
      (check-type outcome t))))
~~~~~
I would strongly prefer that casual testing be a lot less ceremonial
and complicated. It's the sort of thing is best for formal test suites
where they have abstracted things like this. For the casual tester,
testing the outcome as we just said we shouldn't do as it would be
covered by tests of the observer superclass, is probably sufficient,
and definitely simpler:

~~~~~common-lisp
(let* ((observer (make-instance observer))
       (gear (gear (make-instance 'gear
                                  :cog 11
                                  :observers (list observer)))))
  (setf (cog gear) 12)
  (assert (= 12 (latest-change observer :cog))))
~~~~~

In any case, the thing I come away with is that, despite the
differences in Ruby and Common Lisp's OOP models, Metz's advice on
testing OOP in either seems to apply equally well with few
adjustments.

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">&#xa0;</th>
<th scope="col" class="org-left">function</th>
<th scope="col" class="org-left">procedure</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">public</td>
<td class="org-left">assert return value</td>
<td class="org-left">expect effect outcome</td>
</tr>


<tr>
<td class="org-left">private or built-in</td>
<td class="org-left">ignore</td>
<td class="org-left">ignore</td>
</tr>
</tbody>
</table>

#### When to write tests

Metz cautions against being too dogmatic about the Test Driven
Development dictum, "write tests first," but she does strongly
recommend it for novice programmers. However well served by the
practice novices are, their output will show they have a lot to learn
about design. But their code will be testable, which is something it
might not otherwise be.

> It is an unfortunate truth that the most complex code is usually
> written by the least qualified person. This does not reflect an innate
> complexity of the underlying task, rather a lack of experience on the
> part of the programmer. Novice programmers don't yet have the skills
> to write simple code.

As novice myself, I've come to think it's pretty important for a
test-driven developer (novice or expert) to become familiar with
testing anti-patterns. There's any number of articles one can read of
skepticism of test-driven and behavior-driven development, and many of
them home in on the anti-patterns of TDD/BDD without coming to terms
with them as anti-patterns. Metz's earlier points about what **not** to
test will serve us well in avoiding some of them. Unfortunately, the
best way to quickly become really familiar with testing anti-patterns
seems to be to implement them and suffer the consequences.

Since Metz's approach to testing is centered on reducing the cost of
change, this chapter discusses some pragmatics. When developing
something new, it's common for a more experienced developers to
"spike"&#x2013;that is, flesh out the implementation to define the
interfaces before developing tests. Unfortunately the developer doing
the spike is at risk of designer hubris. However, "test first" comes
with it's own design risks. As has happened to me a few times, setting
up initial tests can calcify your initial interface designs. When you
realize the design faults you'll find yourself having to refactor both
tests and implementation. The second iteration might only fare a
little better.

I know when this happens to me, I find it pretty discouraging, and I
find myself sympathizing with those who feel TDD over-rated. There's
an apparent chicken-egg paradox here with design and testing. However
just like real chickens and eggs, the solution is not that either
initializes the other, it's that they emerged at the same time, from
the same processes.

#### Knowing how to test

As I mentioned earlier, I think you can do quite effective testing of
Common Lisp code using only Common Lisp. What follows is adapted from
a comment I made on exercism.io about it:

Use `assert` for all assertions positive or negative.

Use either or both of `assert` and `check-type` to check on stateful
things either before or after or both when a change is expected.

Use either `handler-bind` or `ignore-errors` to check for errors.

Testing this way means failing test will halts the program and runs
the debugger. There are no nice reports, as in a formal test, however
there's no need to repeat the test with a strategically placed `break`
statement or the equivalent.

Testing this way means every passing test returns `nil`. This will
make more sense considering the above alternative.

Use `flet` or `labels` to mock functions and methods. 

Use `macrolet` to mock macros.

Use `symbol-macrolet` to mock places.

Use `let` or `let*` to mock values either dynamic and lexical. This
deserves some additional words. 

CL's lexical scopes let you mask over any dynamic variable and work
with that value with no surprises or interference from the outside,
whatever else happens to it. When the form is done evaluating it can
be like nothing happened. In any given form you can control things
like the random-state, all of the standard input/output/query/terminal
streams, the readtable (the readtable!), and the many and various
states of the printer, probably other things, I'm forgetting).

There's so much more:

Use `time` for basic benchmarking and `room` to check on memory. 

Use `trace` to get a report about a function as it runs (and use
`untrace` to turn off reporting). 

Use `step` step through it a process and inspect it along the way.

Use `pprint` with `macroexpand` or `macroexpand-1` to debug macros.

Use `disassemble` to look at what machine code your program will
compile to. 

Use `describe` and `inspect` to report on and interact with your code.

Use readtime conditionals on your inline tests and setup a `:testing`
in `*features*` to make sure they only run when you want them enabled.

What I like about this, is that you don't have to download or
configure anything you can just do it. If you've used RSpec or some
other formal testing suite like it, I think you'll find the native CL
equivalents about on par in verbosity and ceremony, but generally
around error handling. It's more lightweight elsewhere.

There's definitely some things you get with a formal testing package
that are useful and good, but you can use this kind of thing to do a
kind of scrappy, quasi-formal TDD that's really helpful and powerful
for a creative "spike" at a problem or implementation, where the
formal constraints of a testing suite would otherwise seem
inappropriate.

#### Testing incoming messages

Here's the `wheel` and a method `diameter` for wheel objects we've
seen before.

~~~~~common-lisp
(defclass wheel ()
  ((rim  :reader rim  :initarg :rim)
   (tire :reader tire :initarg :tire)))

(defmethod diameter ((wheel wheel))
  (+ (rim wheel) (* 2 (tire wheel))))
~~~~~

Testing `diameter` is straightforward: it takes a wheel object and
computes it's diameter.

~~~~~common-lisp
(let ((wheel (make-instance 'wheel :rim 26 :tire 1.5)))
  (assert (typep (diameter wheel)
                 '(float 28.9 29.1))))
~~~~~

Here we create a `wheel` test object and uses `assert` on the computed
diameter that it's within a certain tolerance. The assert will raise
an error if it's not and return `nil` if it is. 

I could put this in a test function, but usually I just write tests
like this right in the file I'm working in. This has the effect of
running when I load the file, or when I evaluate the form in Emacs. In
this way, if an error occurs, I'm immediately thrust into the Lisp
debugger to try to figure out what when wrong. If it passes, nothing
else happens, and I go to work on what's next.

Now lets look at an implementation of the `gear` and some attendent
methods. Here we're introducing an entanglement for the purpose of
illustration.

~~~~~common-lisp
(defclass gear ()
    ((cog       :reader cog       :initarg :cog)
     (chainring :reader chainring :initarg :chainring)
     (rim       :reader rim       :initarg :rim)
     (tire      :reader tire      :initarg :tire)))

(defmethod gear-ratio ((gear gear))
    (with-accessors ((cog cog) (chainring chainring)) gear
    (/ chainring (float cog))))

(defmethod gear-inches ((gear gear))
    (* (gear-ratio gear)
     (diameter
      (make-instance 'wheel
                     :rim  (rim  gear)
                     :tire (tire gear)))))
~~~~~

Writing tests for both `gear-ratio` and `gear-inches` are as
straightforward as with `diameter` above. But this implementation of
`gear-inches` is coupled with the wheel object. We cannot write a test
for `gear-inches` that's independent of the implementation of
`wheel`. There are two main hazards Metz discusses with this kind of
coupling:

-   If `wheel` objects, or the computations of `diameter` were
    expensive, both application and tests would suffer without an
    obvious cause.
-   The correctness of `gear-inches` depends on the correctness of
    `wheel` and `diameter`. Both application and tests might fail in
    computing `gear-inches` in a area of code potentially far removed
    from the implementation of `gear-inches`.

We're going to consider the reader methods "internal" and ignore them
leaving `diameter`, `gear-ratio`, and `gear-inches`.

Metz's first advice to testing every incoming message, is to delete
unused interfaces, about which she encourages us to be ruthless. We
don't have any unused interfaces here, although we do have some
redundant ones. We're going to work through those in developing tests.

#### Isolating objects under test

We'll look at `gear` and it's methods, implemented with a `wheel`
object "injected" into the class.

~~~~~common-lisp
(defclass gear ()
  ((cog       :reader cog       :initarg :cog)
   (chainring :reader chainring :initarg :chainring)
   (wheel     :reader wheel     :initarg :wheel)))

(defmethod gear-ratio ((gear gear))
  (with-accessors ((cog cog) (chainring chainring)) gear
      (/ chainring (float cog))))

(defmethod gear-inches ((gear gear))
  (* (gear-ratio gear) (diameter (wheel gear))))
~~~~~

Here we have drawn off most of the concerns of the `wheel` class and
`gear-inches` deals with `wheel` as a value. Testing `gear-inches` is
straightforward:

~~~~~common-lisp
(let* ((wheel (make-instance 'wheel :rim 26 :tire 1.5))
       (gear (make-instance 'gear
                              :cog 11
                              :chainring 52
                              :wheel wheel)))
  (assert (typep (gear-inches gear)
                 '(float 137.09 137.11))))
~~~~~

However `gear-inches`, still calls upon `diameter` and `diameter` must
be a function or method that may work upon whatever the `wheel` object
is. 

Now, here is where the story of isolation and injection between Ruby
and Common Lisp diverge in practice, although the principles are still
the same. We've been defining the `gear` class so far such that any
particular gear is defined with a built-in relation to a particular
`wheel`. This coupling causes several problems with testing that are
nearly irrelevant in Common Lisp, since CLOS separates methods from
classes. Instead we should note that it's simpler to test gears and
wheels as independant objects and if they are related the relation
itself can be it's own test subject.

So lets stop defining `gear` objects in terms of `wheel` objects.

First here's `wheel`, `diameter`, and its test again:

~~~~~common-lisp
(defclass wheel ()
  ((rim  :reader rim  :initarg :rim)
   (tire :reader tire :initarg :tire)))

(defmethod diameter ((wheel wheel))
  (+ (rim wheel) (* 2 (tire wheel))))

(let ((wheel (make-instance 'wheel :rim 26 :tire 1.5)))
  (assert (typep (diameter wheel)
                 '(float 28.9 29.1))))
~~~~~

Now here's `gear`, `gear-ratio`, and its test:

~~~~~common-lisp
(defclass gear ()
  ((cog :reader cog :initarg :cog)
   (chainring: :reader chainring :initarg :chainring)))

(defmethod gear-ratio ((gear gear))
  (with-accessors ((cog cog) (chainring chainring)) gear
      (/ chainring (float cog))))

(let ((gear (make-instance 'gear :cog 11 :chainring 52)))
  (assert (typep (gear-ratio gear)
                 '(float 4.71 4.74))))
~~~~~

But now we obviously have to do something to the `gear-ratio` method,
and that's simply to add a `wheel` parameter and adjust the test
accordingly.

~~~~~common-lisp
(defmethod gear-inches ((gear gear) (wheel wheel))
  (* (gear-ratio gear) (diameter wheel)))

(let ((wheel (make-instance 'wheel :rim 26 :tire 1.5))
      (gear (make-instance 'gear :cog 11 :chainring 52)))
  (assert (typep (gear-inches gear wheel)
                 '(float 137.09 137.11))))
~~~~~

Now, of course, `gear-inches` is entirely ignorant of any other
relation that it's arguments have. Coorespondingly, for any object
that relates a `gear` and a `wheel` we need not be concerned that it
properly computes a `gear-inches` property, as we know the
`gear-inches` method is defined to do so for any `gear` and `wheel` we
provide.

While we're at it why, are `cog` and `chainwheel` conjoined in one
object? We'll let that go. On a working bicycle the cogs and
chainwheels are in communication through the chain, and treating that
as a single object could make sense.

That's a lot of words, and there's still lots to say about this
chapter and subject. I'll save that for another time.
