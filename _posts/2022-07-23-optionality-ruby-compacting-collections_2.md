---
layout: post
title: "Optionality in Ruby: compacting collections follow-up"
date: "2022-07-23T10:30"
tags: 
  - optionality
  - ruby
---

Write in haste, repent at leisure.

There's something, I find aesthetically unsatisfying about the example
I gave in the last post.

    { :foo => 1,
      :bar => 2,
      :baz => maybe,
      :qux => 4
    }.compact.
      merge(wat: 5)

Which is fine, I guess, as illustration of the principle, but I
realize that I put mixed the constants and the optional value in the
first hash and merged another constant. That's not what I would
actually do and what I would do deserves some words.

<!-- more -->

Here's an example essentially just like how I actually use this
technique:

    { :foo => 1,
      :bar => 2,
      :qux => 4,
      :wat => 5
    }.merge(
      { :baz => maybe
      }.compact
    )

And I've alluded to this before in this series, in [Modifying params
2](/2021/07/18/optionality-ror-modifying-params-2.html) but, reviewing
that code, I think realize that isn't quite right either. I'd do
something like this:

    class WatsController < ApplicationController
      # ...
    
      def new
        @wat = Wat.new(default_wat_params)
        @wat.assign_attributes(derived_wat_params)
      end
    
      def create
        @wat = Wat.create(wat_params)
        # ...
      end
    
      def edit
        @wat = Wat.find(params[:id])
      end
    
      def update
        @wat = Wat.find(params[:id])
        @wat.update(wat_params)
        # ...
      end
    
      # ...
    
      private
    
      def default_wat_params
        { :foo => default_foo,
          :bar => default_bar
        }
      end
    
      def derived_wat_params
        { :baz => derived_bar(@wat),
          :qux => derived_qux(@wat)
        }.compact
      end
    
      def wat_params
        params.require(:wat).
          with_defaults(
    	default_wat_params
          ).merge(
    	derived_wat_params
          ).permit(
    	:foo,
    	:bar,
    	:baz,
    	:qux
          )
      end
    end

And here you can see, in `derived_wat_params` the usage of `compact`
keeping the optional values out of the derived hash to be merged into
the params with their defaults. Presumably `default_foo` and
`default_bar` are never-nil, but `derived_baz` and `derived_qux` could
be, hence, using `compact` this way.

The practice of building parameters for methods instead of defining
objects such that defaults and derived values are determined by the
destination may need some defending, but that must be deferred.

In the meantime, I hope this follow-up seems useful enough.

