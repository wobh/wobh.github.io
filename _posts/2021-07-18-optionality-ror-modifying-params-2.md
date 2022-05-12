---
layout: post
title: "Optionality in Ruby on Rails: modifying params--why not don't"
date: "2021-07-18T10:15"
tags: ["optionality", "ruby", "ruby-on-rails"]
---

I talked with a couple of folks about my last post, and based on those
conversations and further experiments, I'm going to suggest a
different approach, one that doesn't require mutating the params
values.

For one thing, you really should make a solid effort at avoiding
it. The `params` is already exposed to lots of things, and while they
shouldn't be meddling with it, neither really, should we. Also, we
don't have to, since `ActionController::Parameters` has a pretty solid
interface, such that it can be avoided in most circumstances.

<!-- more -->

Here's a basic controller setup, lightly modified from scaffolding.

    class WatsController < ApplicationController


      # the usual ...

      # POST /wats
      def create
        @wat = Wat.create(wat_params)
      end

      # PATCH/PUT /wats/{id}
      def update
        @wat = Wat.find(params[:id])
        @wat.update(wat_params)
      end

      private

      def default_baz
        # ...
      end

      def derive_qux
        #...
      end

      def wat_params
        params.require(:wat).
          with_defaults(
            baz: default_baz
          ).merge(
            qux: derive_qux
          ).permit(
            :foo,
            :bar,
            :baz,
            :qux
          )
      end
    end

In a series about optionality, I have to take this moment to
appreciate `ActionController::Parameters#reverse_merge` and it's
contextual alias, `with_defaults` here. I like how it all chains
together, using `#reverse_merge` to set defaults, followed by `#merge`
to set derived values, then vetting the results with `#permit`. This
seems like an 80% solution for this sort of thing, if not a whole lot
more.

More here:

<https://api.rubyonrails.org/classes/ActionController/Parameters.html>

