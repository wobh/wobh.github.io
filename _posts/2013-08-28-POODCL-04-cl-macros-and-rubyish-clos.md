---
layout: post
title: "POODCL part 4: Rubyish attr_accessors for CLOS"
description: ""
category: poodcl
tags: [common-lisp, clos, poodr, ruby]
date: 2013-08-28 20:16:01 -0700
---

[I mentioned]({% post_url 2013-07-27-practical-object-oriented-design-in-common-lisp-01 %}) that
we could write a Lisp macro to allow us to define class
instance-variables like Ruby does. I think it's worth seeing this, not
only to see some of the usefulness of Lisp macros but to see some of
the considerations you have to make when using them to extend Lisp.

<!-- more -->

First we'll need some functions to support creating slot definition lists.

~~~~~common-lisp
(defun defslot-accessible (accessor-type slot-name)
  "Make a slot definition from a slot-name and accessor-type keyword."
  (if (keywordp slot-name)
      (let ((slot (intern (symbol-name slot-name))))
        (list slot accessor-type slot :initarg slot-name))
      (list slot-name accessor-type slot-name)))

(defun defslots-accessible (accessor-type &rest slot-names)
  "Takes a list of keywords and outputs slot definition lines"
  (loop
     for slot-name in slot-names
     collect (defslot-accessible accessor-type slot-name)))

(defun get-accessor-keyword (attr)
  (ecase attr
    (attr-accessor :accessor)
    (attr-reader   :reader)
    (attr-writer   :writer)))
~~~~~

A couple of notes. 

Initargs are optional for object slots in CLOS, so I thought it would
was reasonable to reflect that in the kind of symbol that
`DEFSLOT-ACCESSIBLE` receives for a slot-name. If it's a keyword, it
makes a corresponding initarg, if not, it just makes a plain accessor
for the slot.

`GET-ACCESSOR-KEYWORD` maps a Ruby-like "attr\_" to CLOS's accessor
keywords. Note that it will error if the attr symbol given is not one
of the three types. We'll catch this error in our macro and assume
it's a regular CLOS slot definition.

Now we can build a macro that builds a `DEFCLASS` form from
Ruby-like attr-accessor expressions.

~~~~~common-lisp
(defmacro defclass-rubyish (name superclasses
                            &optional slots &rest options)
  `(defclass ,name ,superclasses
     ,(loop
         for indirect-slot in slots
         append (handler-case 
                    (apply 'defslots-accessible
                           (get-accessor-keyword (first indirect-slot))
                           (rest indirect-slot))
                  (type-error () (list indirect-slot))))
     ,@options))
~~~~~

We have just extended Common Lisp. There's probably a more elegant
way, but for now, let's just see it in action:

~~~~~common-lisp
CL-USER> (pprint
          (macroexpand-1
           '(defclass-rubyish rbcl ()
             ((attr-reader :foo bar)
              (attr-accessor baz :qux)
              (blerg :writer :blerg :type 'string)
              (pwomp :writer :pwomp :documentation "pwomp!"))
             (:default-initargs
              :foo 0
              :qux 1))))
(DEFCLASS RBCL ()
  ((FOO :READER FOO :INITARG :FOO) 
   (BAR :READER BAR) 
   (BAZ :ACCESSOR BAZ)
   (QUX :ACCESSOR QUX :INITARG :QUX) 
   (BLERG :WRITER :BLERG :TYPE 'STRING)
   (PWOMP :WRITER :PWOMP :DOCUMENTATION "pwomp!"))
  ((:DEFAULT-INITARGS :FOO 0 :QUX 1)))

CL-USER> (defclass-rubyish rbcl ()
           ((attr-reader :foo bar)
            (attr-accessor baz :qux)
            (blerg :writer :blerg :type 'string)
            (pwomp :writer :pwomp :documentation "pwomp!"))
           (:default-initargs
            :foo 0
            :qux 1))
#<STANDARD-CLASS RBCL>
~~~~~

The macro takes our Rubyish syntax and expands into a CLOS class
definition. I'm not going to explain how the macro code-building works
here. You can learn the basics from
[chapter 8 of Peter Seibel's *Practical Common Lisp*](http://www.gigamonkeys.com/book/macros-defining-your-own.html)

More advanced macro techniques can be learned from Paul Graham's *On
Lisp* and still more from Doug Hoyte's *Let Over Lambda*.

I do need to explain, why this may not be a good extension of CLOS
syntax, and why I don't expect to use it in my further code examples.

Consider that twist I added to `DEFSLOT-ACCESSIBLE`: if the argument to
slot-name is a keyword symbol it makes a slot with an initarg, but if
it's a regular symbol it does not. In fact, neither the accessor, nor
the initarg are necessary for defining a slot. Moreover, they may also
be defined more than once for a slot.

In the `DEFCLASS-RUBYISH` macro, I allowed one to define slots
traditionally, in addition to the Rubyish way. In Common Lisp we can
define lots of things about class slots. We should look at the Common
Lisp Hyperspec.

<http://www.lispworks.com/documentation/HyperSpec/Body/m_defcla.htm>

So, while this macro adds some convenience for common use cases, it's
functionally redundant, and adds some inconvenience if we have to
refactor, say to add type or documentation, or additional accessors or
initargs to any slots first defined this way. In Ruby these aren't
concerns, so in Ruby this is good and useful syntax.

Even if we don't do exactly this, we will probably write macros around
`DEFCLASS` in just this way, and as an example, it's not too bad.


[next]({% post_url 2013-09-08-poodcl-part-5-duckish-typing %})

[prev]({% post_url 2013-07-30-practical-object-oriented-design-in-common-lisp-part-3 %})
