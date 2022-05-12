---
layout: post
title: "Optionality in Ruby on Rails: modifying params"
date: "2021-07-14T20:00"
tags: ["optionality", "ruby", "ruby-on-rails"]
---

I'm under the impression that manipulating the incoming params to
Rails controller methods is considered something of a code smell if
not an antipattern. Rationales for avoiding this include:

-   Is it really the controller's job?
-   What about strong parameters?
-   General Hazards of mutability

I can't do anything about the first one. However, I can readily
believe that it may become necessary for Reasons. If you have to, you
have to. Below the fold I'll show a short pattern to deal with the
next two.

<!-- more -->

Without further ado:

    context = params.fetch(:context)
    context[:subject] ||= ActionController::Parameters.new
    
    context[:subject].merge! { 
      # ...
    }

In the first line we make sure any required parent parameters exist,
and assign them to a variable for convenience. If they don't exist,
`fetch` raises an error.

In the second line we use `||=` to optionally assign a default
`Parameters` object to the subject which may or may have been
submitted.

Lastly, we mutate the subject with derived parameters as necessary
(given). This mutates the underlying `params` object which can then be
passed to `#create`, `#update` using
`params.require(:context).permit(:subject)` as usual.

Should you do this? Perhaps not if you don't really, really have to.

But if you do have to, this basic pattern will prevent a lot of
problems and let you focus on building the parameters hash used in
`merge!` and I think that's pretty important.

