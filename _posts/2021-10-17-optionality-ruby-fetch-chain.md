---
layout: post
title: "Optionality in Ruby: the fetch chain"
date: "2021-10-17T20:15"
tags: ["optionality", "ruby"]
---

This one is a simple enough.

Given a deeply nested structure, how could you conveniently fetch a
deeply nested value?

Let's harken back to Ruby `< 2.3` and talk about `fetch` chains!

<!-- more -->

Given:

    deep = {
      :foo => {
        :bar => [0,1],
        :baz => {
          :qux => -1,
          :yup => [2,3]
        },
        :zot => [5,7,11],
        :kak => [
          [Rational(1,2), Rational(1,3)],
          [Rational(1,5), Rational(1,7)]
        ]
      }
    }

You know `:qux` is in there. You know it's supposed to be a child of
`:baz` which is supposed to be a child of `:foo`. If you're confident
of your data you might just do:

    deep[:foo][:baz][:qux]

But if you aren't confident of your data, or have recently learned
it's untrustworthy what can you do?

You could introduce some logic and write:

    deep[:foo] &&
      deep[:foo][:baz] &&
      deep[:foo][:baz][:qux]

This is fairly robust, but if you need some delicacy, you can use a
chain of `fetch` calls like this:

    deep.
      fetch(:foo).
      fetch(:baz).
      fetch(:qux)

Behold the fetch chain!

This will raise `KeyError` if `:foo` or any subsequent key is
missing. If called on an `Array` with an index argument that's
out-of-bounds it raises `IndexError`. But maybe you don't want it to
be quite that delicate. Good thing `fetch` lets you pass a default
value, you can do this:

    deep.
      fetch(:foo, {}).
      fetch(:baz, {}).
      fetch(:qux)

Before Ruby 2.3, this was how you could nimbly step through a
structure of Hashes and Arrays and not trip over `nil` values from
missing keys or indexes.

Ruby 2.3 introduced `Hash#dig` to solve this problem with nested
`Hash`. I might as well mention here that `#dig` is also implemented
in ActiveSupport's `HashWithIndifferentAccess` and ActionController's
`Parameters`, and thank goodness for that! I notice, though, there's
no `Array#dig`, not even in Ruby 3.0. And fair enough, I
suppose. Maybe no one's really needed it.

If you did need it though, Ruby 2.6 introduced, `&.`, the "safe
navigation operator" which you can read about here:

<https://ruby-doc.org/core-2.6/doc/syntax/calling_methods_rdoc.html#label-Safe+navigation+operator>

This will solve a lot of problems not least with chaining accessors to
nested heterogenous collections. Oh, but, because of operator syntax,
we're still going to have to use `#fetch` and provide defaults aren't
we.

    deep.
      fetch(:foo, nil)&.
      fetch(:kap, nil)&.
      fetch(0, nil)

Well, anyway, I think it's important to understand the fetch chain as
a kind of optionality primitive. It's two main limits are:

1.  It still raises `NoMethodError` if a value is `nil` or some other
    object that doesn't implement `fetch`.
2.  It continues the chain of execution with redundant fetch calls on
    the default values.

The first is why ActiveSupport provides `Object#try` and
`NilClass#try`. The second is why Ruby added `&.` the "safe navigation
operator" and why, in days of yore, the sequence of `&&` and
successive `[]` would have been necessary.

But is this really a problem for syntax? Do we really need to add
methods to fundamental models? I think this looks more like an
interface problem. I think `#fetch` has some hitherto little
appreciated virtues here. Let's talk about `#fetch` as a legacy
protocol.

We can find `#fetch` defined the core objects of, `Hash`, `Array` and
`ENV`. In Ruby's standard library it's in `DBM` and `YAML::DBM`. There
may be others. It has a natural counterpart in `#store` which is also
implemented in all of these&#x2013;except `Array` for some reason. I think
the names of "fetch" and "store" might originate from the traditional
interface for DBM key-value stores (which included "delete").

So, with that in mind, lets imagine a generalized `Collection` and
`Collector` class like this:

    module Collection
      def [](key)
        fetch(key)
      end
    
      def []=(key, value)
        store(key, value)
      end
    
      # derived methods from #each here
    end
    
    class Collector
      include Collection
    
      attr_reader :items
    
      def initialize(**items)
        @items = items
      end
    
      def fetch(key, default=nil)
        items.fetch(key, default)
      end
    
      def store(key, value)
        items.store(key, value)
      end
    
      def delete(key)
        items.delete(key)
      end
    
      def each_pair(&block)
        if block_given?
          items.each_pair &block
        else
          items.each_pair
        end
      end
    end

By itself, it's a nice enough interface. I think it's good design to
support operator syntax (`[]`, and `[]=`) with vernacular
methods. Perhaps one may even think that `OpenStruct`, if not
`Struct`, should use it too. But, practically, there's basically no
reason to do this. If Ruby `< 4.0` wants to generalize this interface
for `ENV`-like objects, perhaps people will use them, but it's not a
big deal.

By itself.

If we had more of these generalized collections we could use fetch
chains on them. We could also use `&.` in fact, using both would be
terrific. But we have a coherent, if basic, interface. We don't really
need more syntax, we can create constructable navigation.

Constructable?

Yes. I mean we can turn what would be syntactic expressions into data
we can build. Like Arrays. If we want to derive a `#deep_fetch` and a
`#dig` to our `Collector`, all we need was a coherent interface.

    module Collection
      # ...
    
      def deep_fetch(*keys)
        items = {}
        each { |k,v| items[k] = v }
    
        keys.reduce(items) { |acc,elt|
          acc.fetch(elt)
        }
      end
    
      def dig(*keys)
        items = {}
        each { |k,v| items[k] = v }
    
        keys.reduce(items) { |acc,elt|
          break acc if acc.nil?
    
          acc.fetch(elt, nil)
        }
      end
    end

We can do more.

    module Collection
      # ...
    
      def values_at(*keys)
        items = {}
        each { |k,v| items[k] = v }
    
        keys.map { |elt|
          items.fetch(elt, nil)
        }
      end
    
      def slice(*keys)
        items = {}
        each { |k,v| items[k] = v }
    
        keys.reduce(self.class.new) { |acc,elt|
          if item = items.fetch(elt, nil)
    	acc.store(elt, item)
          end
          acc
        }
      end
    end

And even more, like `#deep_values_at` and `#deep_slice` which take
hashes for arguments and serve extremely niche use cases at best. We
haven't even taken a good look at what's possible with
`#store`. That's okay. This is a `#fetch` appreciation post.

Let's be honest, though, it's probably too late to make `#fetch`
happen for generic collections, like `#each` for `Enumerable` or `<=>`
for `Comparable`, and there's likely technical reasons I don't know
about.

