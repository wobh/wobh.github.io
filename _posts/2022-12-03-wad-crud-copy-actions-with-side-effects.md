---
layout: post
title: 'Web Application Design: CRUD copy actions with side effects'
date: 2022-12-03T14:30-0500
tags:
- ruby-on-rails
- web-application-design
--- 

```feature
Given a user visiting the view for Wat copy
When the user clicks on the button to submit the Wat copy form
Then the Wat copy is created
And something else happens
And the user redirected to Wat show view for the copied Wat
```

In this post I want to consider how to design around side effects that
might optionally occur in or as part of an action. I'll just examine
the copy feature from [the last post](/2022/11/14/wad-designing-copy-on-crud.html) since it's convenient to think
about, but this probably won't be the last time we examine optional
side effects. But it should introduce the problem well enough.

<!-- more -->

Here's the abstract flow view of the copy-as-view feature we've been
discussing. I arranged it a little differently from the last post.

<figure class="centered">
  <svg width="264pt" height="193pt"
       viewBox="0.00 0.00 264.00 192.63" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
    <g id="graph0" class="graph" transform="scale(1 1) rotate(0) translate(4 188.63)">
      <title>CRUD flow: copy&#45;as&#45;view</title>
      <polygon fill="white" stroke="none" points="-4,4 -4,-188.63 260,-188.63 260,4 -4,4"/>
      <!-- index -->
      <g id="node1" class="node">
	<title>index</title>
	<ellipse fill="none" stroke="black" cx="165" cy="-166.63" rx="30.59" ry="18"/>
	<text text-anchor="middle" x="165" y="-162.93" font-family="Times,serif" font-size="14.00">index</text>
      </g>
      <!-- new -->
      <g id="node2" class="node">
	<title>new</title>
	<ellipse fill="none" stroke="black" cx="27" cy="-94.63" rx="27" ry="18"/>
	<text text-anchor="middle" x="27" y="-90.93" font-family="Times,serif" font-size="14.00">new</text>
      </g>
      <!-- index&#45;&gt;new -->
      <g id="edge1" class="edge">
	<title>index:w&#45;&gt;new:n</title>
	<path fill="none" stroke="black" stroke-dasharray="1,5" d="M121.55,-166.6C76.73,-166.23 34.52,-162.4 27.9,-123.83"/>
	<polygon fill="none" stroke="black" points="121.48,-170.1 131.49,-166.63 121.5,-163.1 121.48,-170.1"/>
	<polygon fill="none" stroke="black" points="31.41,-123.83 27.12,-114.14 24.43,-124.39 31.41,-123.83"/>
      </g>
      <!-- show -->
      <g id="node3" class="node">
	<title>show</title>
	<ellipse fill="none" stroke="black" cx="165" cy="-22.63" rx="29.5" ry="18"/>
	<text text-anchor="middle" x="165" y="-18.93" font-family="Times,serif" font-size="14.00">show</text>
      </g>
      <!-- index&#45;&gt;show -->
      <g id="edge2" class="edge">
	<title>index&#45;&gt;show</title>
	<path fill="none" stroke="black" stroke-dasharray="1,5" d="M159.47,-137.16C157.84,-112.4 157.84,-76.71 159.48,-51.98"/>
	<polygon fill="none" stroke="black" points="155.97,-137.32 160.29,-147 162.95,-136.74 155.97,-137.32"/>
	<polygon fill="none" stroke="black" points="162.94,-52.59 160.28,-42.33 155.96,-52 162.94,-52.59"/>
      </g>
      <!-- new&#45;&gt;show -->
      <g id="edge7" class="edge">
	<title>new:s&#45;&gt;show:w</title>
	<path fill="none" stroke="black" d="M27,-76.63C27,-27.11 73.44,-23 122.78,-22.66"/>
	<polygon fill="black" stroke="black" points="122.5,-26.16 132.49,-22.64 122.48,-19.16 122.5,-26.16"/>
	<text text-anchor="middle" x="125.21" y="-3.8" font-family="Times,serif" font-size="14.00">create</text>
      </g>
      <!-- show&#45;&gt;index -->
      <g id="edge8" class="edge">
	<title>show&#45;&gt;index</title>
	<path fill="none" stroke="black" d="M169.59,-40.82C172.11,-64.74 172.43,-108.35 170.53,-137.16"/>
	<polygon fill="black" stroke="black" points="167.05,-136.74 169.71,-147 174.03,-137.32 167.05,-136.74"/>
	<text text-anchor="middle" x="195.17" y="-140.19" font-family="Times,serif" font-size="14.00">destroy</text>
      </g>
      <!-- edit -->
      <g id="node4" class="node">
	<title>edit</title>
	<ellipse fill="none" stroke="black" cx="229" cy="-94.63" rx="27" ry="18"/>
	<text text-anchor="middle" x="229" y="-90.93" font-family="Times,serif" font-size="14.00">edit</text>
      </g>
      <!-- show&#45;&gt;edit -->
      <g id="edge3" class="edge">
	<title>show&#45;&gt;edit</title>
	<path fill="none" stroke="black" stroke-dasharray="1,5" d="M186.63,-47.29C193.49,-54.79 201.06,-63.07 207.88,-70.54"/>
	<polygon fill="none" stroke="black" points="189.35,-45.08 180.02,-40.06 184.18,-49.8 189.35,-45.08"/>
	<polygon fill="none" stroke="black" points="205.08,-72.65 214.41,-77.67 210.24,-67.93 205.08,-72.65"/>
      </g>
      <!-- copy -->
      <g id="node5" class="node">
	<title>copy</title>
	<ellipse fill="none" stroke="black" cx="100" cy="-94.63" rx="27.9" ry="18"/>
	<text text-anchor="middle" x="100" y="-90.93" font-family="Times,serif" font-size="14.00">copy</text>
      </g>
      <!-- show&#45;&gt;copy -->
      <g id="edge4" class="edge">
	<title>show&#45;&gt;copy</title>
	<path fill="none" stroke="black" stroke-dasharray="1,5" d="M143.43,-46.87C136.39,-54.44 128.59,-62.84 121.56,-70.42"/>
	<polygon fill="none" stroke="black" points="145.97,-49.28 150.21,-39.57 140.84,-44.51 145.97,-49.28"/>
	<polygon fill="none" stroke="black" points="119.05,-67.97 114.81,-77.68 124.18,-72.73 119.05,-67.97"/>
      </g>
      <!-- edit&#45;&gt;index -->
      <g id="edge5" class="edge">
	<title>edit:n&#45;&gt;index:e</title>
	<path fill="none" stroke="black" stroke-dasharray="1,5" d="M229,-112.63C229,-136.28 226.05,-158.64 208.16,-164.9"/>
	<polygon fill="none" stroke="black" points="207.84,-161.41 198.5,-166.4 208.91,-168.33 207.84,-161.41"/>
      </g>
      <!-- edit&#45;&gt;show -->
      <g id="edge9" class="edge">
	<title>edit:s&#45;&gt;show:e</title>
	<path fill="none" stroke="black" d="M229,-76.63C229,-52.79 225.5,-30.57 207.3,-24.35"/>
	<polygon fill="black" stroke="black" points="207.91,-20.9 197.5,-22.86 206.86,-27.82 207.91,-20.9"/>
	<text text-anchor="middle" x="212.11" y="-7.08" font-family="Times,serif" font-size="14.00">update</text>
      </g>
      <!-- copy&#45;&gt;index -->
      <g id="edge6" class="edge">
	<title>copy:n&#45;&gt;index:w</title>
	<path fill="none" stroke="black" stroke-dasharray="1,5" d="M100,-112.63C100,-136.47 103.5,-158.7 121.7,-164.91"/>
	<polygon fill="none" stroke="black" points="121.09,-168.36 131.5,-166.41 122.14,-161.44 121.09,-168.36"/>
      </g>
      <!-- copy&#45;&gt;show -->
      <g id="edge10" class="edge">
	<title>copy&#45;&gt;show:w</title>
	<path fill="none" stroke="black" d="M100.91,-76.45C102.69,-59.04 107.91,-33.56 123.15,-25.31"/>
	<polygon fill="black" stroke="black" points="123.66,-28.79 132.53,-23 121.98,-22 123.66,-28.79"/>
      </g>
    </g>
  </svg>
</figure>

Here I've dotted and hollowed the edges that represent links between
views, and left solid the lines that represent regular actions. The
abstract flow diagram we've been using is a little too abstract for
further illustrations, but the idea is that both the "new" and "copy"
views share a "create" action that redirects to "show" and this shows
where we're at in this implementation.

With that in mind, now let's consider the features describing "new"
and "copy":

```feature
Given a user visiting the view for Wat new
When the user clicks on the button to submit the Wat new form
Then the new Wat is created
And the user redirected to Wat show view for the new Wat

Given a user visiting the view for Wat copy
When the user clicks on the button to submit the Wat copy form
Then the Wat copy is created
And something else happens
And the user redirected to Wat show view for the Wat copy
```

So our challenge comes from how "something else happens" during the
`Wat copy` action that does not happen for the regular create action when
the form on the `Wat new` view is submitted and our current implementation
has them using the same "create" action to get something done.

About the "something else" feature. I want to avoid specifying too
much about it, because we're looking for hints about design principles
and there will be other examples. Also, we're more interested in
uncovering questions rather than coming up with definitive answers. We
might ask:

-   How does the feature affect the representation of resources?
    -   For example, perhaps copied resources are represented with links
        back to the resource it was copied from.
-   How does the feature affect user interaction with resources?
    -   For example, perhaps a notification with a link to the copied
        resource is sent to interested parties.

But the only thing we need to expect for the design is that at least
one of those questions has some details similar to the example
answers, and that it happens in some circumstances but not others.

In the last post we considered two options for implementation:

-   parameterize the existing `new` view or
-   create a specialized `copy` view.

There I said that, if it were necessary to represent the original
resource we were copying from, it seemed better to create a member
route and specialized view for copying it from. We're looking for a
similiar requirement or conditions to suggest a preferences for any of
three implementations.

Two of the implementations will be familiar analogues of the last
post: parameterizing an existing controller method, or creating a
specialized method. The third will be a solution that delegates the
whole matter to the client, because we'll have a whole controller and
action just for carrying out the action.

Circling back to diagrams: as I mentioned, the abstract flow diagrams
I tried to come up with don't illustrate this as well as I hoped, so I
have switched to "sequence diagrams" which have the benefit of
focusing on a sequence of interactions in time.


# Side effect by parameterized action

Since the side effect is optional we could add a conditional to the
controller method that carries out the side effect when a parameter is
supplied by the client.

<figure class="centered">
  <svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" contentStyleType="text/css" height="266px" preserveAspectRatio="none" style="width:389px;height:266px;background:#FFFFFF;" version="1.1" viewBox="0 0 389 266" width="389px" zoomAndPan="magnify"><defs/><g><rect fill="none" height="59.6211" style="stroke:#000000;stroke-width:1.5;" width="236" x="147.5" y="125.1094"/><line style="stroke:#181818;stroke-width:0.5;stroke-dasharray:5.0,5.0;" x1="32" x2="32" y1="36.4883" y2="231.041"/><line style="stroke:#181818;stroke-width:0.5;stroke-dasharray:5.0,5.0;" x1="240.5" x2="240.5" y1="36.4883" y2="231.041"/><rect fill="#E2E2F0" height="30.4883" rx="2.5" ry="2.5" style="stroke:#181818;stroke-width:0.5;" width="54" x="5" y="5"/><text fill="#000000" font-family="sans-serif" font-size="14" lengthAdjust="spacing" textLength="40" x="12" y="25.5352">Client</text><rect fill="#E2E2F0" height="30.4883" rx="2.5" ry="2.5" style="stroke:#181818;stroke-width:0.5;" width="54" x="5" y="230.041"/><text fill="#000000" font-family="sans-serif" font-size="14" lengthAdjust="spacing" textLength="40" x="12" y="250.5762">Client</text><rect fill="#E2E2F0" height="30.4883" rx="2.5" ry="2.5" style="stroke:#181818;stroke-width:0.5;" width="167" x="157.5" y="5"/><text fill="#000000" font-family="sans-serif" font-size="14" lengthAdjust="spacing" textLength="153" x="164.5" y="25.5352">WatsController#create</text><rect fill="#E2E2F0" height="30.4883" rx="2.5" ry="2.5" style="stroke:#181818;stroke-width:0.5;" width="167" x="157.5" y="230.041"/><text fill="#000000" font-family="sans-serif" font-size="14" lengthAdjust="spacing" textLength="153" x="164.5" y="250.5762">WatsController#create</text><polygon fill="#181818" points="229,63.7988,239,67.7988,229,71.7988,233,67.7988" style="stroke:#181818;stroke-width:1.0;"/><line style="stroke:#181818;stroke-width:1.0;" x1="32" x2="235" y1="67.7988" y2="67.7988"/><text fill="#000000" font-family="sans-serif" font-size="13" lengthAdjust="spacing" textLength="185" x="39" y="63.0566">POST /wats?copy_id={wat_id}</text><line style="stroke:#181818;stroke-width:1.0;stroke-dasharray:2.0,2.0;" x1="241" x2="283" y1="97.1094" y2="97.1094"/><line style="stroke:#181818;stroke-width:1.0;stroke-dasharray:2.0,2.0;" x1="283" x2="283" y1="97.1094" y2="110.1094"/><line style="stroke:#181818;stroke-width:1.0;stroke-dasharray:2.0,2.0;" x1="242" x2="283" y1="110.1094" y2="110.1094"/><polygon fill="#181818" points="252,106.1094,242,110.1094,252,114.1094,248,110.1094" style="stroke:#181818;stroke-width:1.0;"/><text fill="#000000" font-family="sans-serif" font-size="13" lengthAdjust="spacing" textLength="64" x="248" y="92.3672">create wat</text><path d="M147.5,125.1094 L214.5,125.1094 L214.5,132.4199 L204.5,142.4199 L147.5,142.4199 L147.5,125.1094 " fill="#EEEEEE" style="stroke:#000000;stroke-width:1.5;"/><rect fill="none" height="59.6211" style="stroke:#000000;stroke-width:1.5;" width="236" x="147.5" y="125.1094"/><text fill="#000000" font-family="sans-serif" font-size="13" font-weight="bold" lengthAdjust="spacing" textLength="22" x="162.5" y="138.6777">opt</text><text fill="#000000" font-family="sans-serif" font-size="11" font-weight="bold" lengthAdjust="spacing" textLength="149" x="229.5" y="137.7441">[params[copy_id].present?]</text><line style="stroke:#181818;stroke-width:1.0;stroke-dasharray:2.0,2.0;" x1="241" x2="283" y1="163.7305" y2="163.7305"/><line style="stroke:#181818;stroke-width:1.0;stroke-dasharray:2.0,2.0;" x1="283" x2="283" y1="163.7305" y2="176.7305"/><line style="stroke:#181818;stroke-width:1.0;stroke-dasharray:2.0,2.0;" x1="242" x2="283" y1="176.7305" y2="176.7305"/><polygon fill="#181818" points="252,172.7305,242,176.7305,252,180.7305,248,176.7305" style="stroke:#181818;stroke-width:1.0;"/><text fill="#000000" font-family="sans-serif" font-size="13" lengthAdjust="spacing" textLength="96" x="248" y="158.9883">something else</text><polygon fill="#181818" points="43,191.7305,33,195.7305,43,199.7305,39,195.7305" style="stroke:#181818;stroke-width:1.0;"/><line style="stroke:#181818;stroke-width:1.0;" x1="37" x2="240" y1="195.7305" y2="195.7305"/><text fill="#000000" font-family="sans-serif" font-size="13" lengthAdjust="spacing" textLength="76" x="49" y="209.2988">201 Created</text><!--MD5=[688c238dd4cfd8bb30247960707ab76a]
@startuml
skinparam responseMessageBelowArrow true

participant Client
participant "WatsController#create" as Server

Client -> Server : POST /wats?copy_id={wat_id}
Server - -> Server : create wat
opt params[copy_id].present?
    Server - -> Server : something else
end
Server -> Client : 201 Created
@enduml

PlantUML version 1.2022.12(Sun Oct 23 14:12:26 EDT 2022)
(GPL source distribution)
Java Runtime: OpenJDK Runtime Environment
JVM: OpenJDK 64-Bit Server VM
Default Encoding: UTF-8
Language: en
Country: US
--></g></svg>
</figure>

What's nice about this is that we keep a single interface for creating
new `Wat` resources, and the differences in creating them, are matters
of the User interface. Error handling logic has been left to the
imagination of the reader.

What's less nice, maybe, is that it's up to the client to supply this
parameter. 

For every "golden path" there are several "stoney paths" that have to
be considered. What are the impacts if the client spuriously includes
or excludes the parameter? Obviously, the side effect happens or does
not happen spuriously as a consequence, but applications and their
users are depending on systems informing them appropriately, so the
analysis should extend to the organization, people, and processes that
depend on the application.

However, the parameter doesn't have to be someting explicitly supplied
by the user, as in the example&#x2013;it could be what I'm going to call
"implicit" by which I mean, it's still part of the request, just not
any part the user "controls". In this example, where `Wat copy` has
it's own view, the controller action could check `request.referer` to
determine if it should carry out the side effect. This mitigates the
issue somewhat.

If it seems a little eyebrow raising to use implicit parameters like
this, I think that's a good instinct. However most applications do
this kind of thing for access control, and maybe the side effect is
something that might be fairly considered "foundational" in the same
way. We'll have to come back to that.

If the server is more of an API and not rendering the views, you might
feel more constrained to use explicit parameters. If it's more
appropriate or important for the side effect to be managed on the
server then we may wish to consider using a specialized action.


# Side effect by specialized action

While I'm pretty comfortable creating new safe actions/views (per the
last post), I'm less confident about creating new unsafe actions, but
if the side effect really needs to be managed by the server, or if it
complicates methods inappropriately, a new action can be set up for
it.

What I came up with, is that the copy view will submit to a new
"member" route:

    POST /wats/{wat_id}/create_copy

and a new controller method will create the copy and invoke the side
effect. The sequence looks like this:

<figure class="centered">
  <svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" contentStyleType="text/css" height="235px" preserveAspectRatio="none" style="width:376px;height:235px;background:#FFFFFF;" version="1.1" viewBox="0 0 376 235" width="376px" zoomAndPan="magnify"><defs/><g><line style="stroke:#181818;stroke-width:0.5;stroke-dasharray:5.0,5.0;" x1="32" x2="32" y1="36.4883" y2="199.7305"/><line style="stroke:#181818;stroke-width:0.5;stroke-dasharray:5.0,5.0;" x1="266" x2="266" y1="36.4883" y2="199.7305"/><rect fill="#E2E2F0" height="30.4883" rx="2.5" ry="2.5" style="stroke:#181818;stroke-width:0.5;" width="54" x="5" y="5"/><text fill="#000000" font-family="sans-serif" font-size="14" lengthAdjust="spacing" textLength="40" x="12" y="25.5352">Client</text><rect fill="#E2E2F0" height="30.4883" rx="2.5" ry="2.5" style="stroke:#181818;stroke-width:0.5;" width="54" x="5" y="198.7305"/><text fill="#000000" font-family="sans-serif" font-size="14" lengthAdjust="spacing" textLength="40" x="12" y="219.2656">Client</text><rect fill="#E2E2F0" height="30.4883" rx="2.5" ry="2.5" style="stroke:#181818;stroke-width:0.5;" width="206" x="163" y="5"/><text fill="#000000" font-family="sans-serif" font-size="14" lengthAdjust="spacing" textLength="192" x="170" y="25.5352">WatsController#create_copy</text><rect fill="#E2E2F0" height="30.4883" rx="2.5" ry="2.5" style="stroke:#181818;stroke-width:0.5;" width="206" x="163" y="198.7305"/><text fill="#000000" font-family="sans-serif" font-size="14" lengthAdjust="spacing" textLength="192" x="170" y="219.2656">WatsController#create_copy</text><polygon fill="#181818" points="254,63.7988,264,67.7988,254,71.7988,258,67.7988" style="stroke:#181818;stroke-width:1.0;"/><line style="stroke:#181818;stroke-width:1.0;" x1="32" x2="260" y1="67.7988" y2="67.7988"/><text fill="#000000" font-family="sans-serif" font-size="13" lengthAdjust="spacing" textLength="210" x="39" y="63.0566">POST /wats/{wat_id}/create_copy</text><line style="stroke:#181818;stroke-width:1.0;stroke-dasharray:2.0,2.0;" x1="266" x2="308" y1="97.1094" y2="97.1094"/><line style="stroke:#181818;stroke-width:1.0;stroke-dasharray:2.0,2.0;" x1="308" x2="308" y1="97.1094" y2="110.1094"/><line style="stroke:#181818;stroke-width:1.0;stroke-dasharray:2.0,2.0;" x1="267" x2="308" y1="110.1094" y2="110.1094"/><polygon fill="#181818" points="277,106.1094,267,110.1094,277,114.1094,273,110.1094" style="stroke:#181818;stroke-width:1.0;"/><text fill="#000000" font-family="sans-serif" font-size="13" lengthAdjust="spacing" textLength="64" x="273" y="92.3672">create wat</text><line style="stroke:#181818;stroke-width:1.0;stroke-dasharray:2.0,2.0;" x1="266" x2="308" y1="139.4199" y2="139.4199"/><line style="stroke:#181818;stroke-width:1.0;stroke-dasharray:2.0,2.0;" x1="308" x2="308" y1="139.4199" y2="152.4199"/><line style="stroke:#181818;stroke-width:1.0;stroke-dasharray:2.0,2.0;" x1="267" x2="308" y1="152.4199" y2="152.4199"/><polygon fill="#181818" points="277,148.4199,267,152.4199,277,156.4199,273,152.4199" style="stroke:#181818;stroke-width:1.0;"/><text fill="#000000" font-family="sans-serif" font-size="13" lengthAdjust="spacing" textLength="96" x="273" y="134.6777">something else</text><polygon fill="#181818" points="43,160.4199,33,164.4199,43,168.4199,39,164.4199" style="stroke:#181818;stroke-width:1.0;"/><line style="stroke:#181818;stroke-width:1.0;" x1="37" x2="265" y1="164.4199" y2="164.4199"/><text fill="#000000" font-family="sans-serif" font-size="13" lengthAdjust="spacing" textLength="76" x="49" y="177.9883">201 Created</text><!--MD5=[86ab485252ae4837be814708ed093169]
@startuml
skinparam responseMessageBelowArrow true

participant Client
participant "WatsController#create_copy" as Server

Client -> Server : POST /wats/{wat_id}/create_copy
Server - -> Server : create wat
Server - -> Server : something else
Server -> Client : 201 Created
@enduml

PlantUML version 1.2022.12(Sun Oct 23 14:12:26 EDT 2022)
(GPL source distribution)
Java Runtime: OpenJDK Runtime Environment
JVM: OpenJDK 64-Bit Server VM
Default Encoding: UTF-8
Language: en
Country: US
--></g></svg>
</figure>

What's nice about this is that it's really simple (again, error
handling logic has been left to the imagination of the reader).

The client still has to choose the right interface, but, in the case
of the `copy` feature, it lines up with the two views:

-   `new` uses the basic `POST /wats` and
-   `copy` uses the new `POST /wats/{wat_id}/create_copy`.

I want to briefly examine what makes me uncomfortable about this
implementation. The basic unsafe actions &#x2013; `create`, `update`,
`delete` &#x2013; do four things:

1.  check their parameters
2.  carry out their action
3.  carry out any side effects
4.  redirect to a view (alternatively, render a response for an API client)

Controller methods should be marshalling resources and making certain
kinds of decisions about how to fulfill the request. It's not at all
obvious to me when we should consider making a new controller action
to fulfill a request. Any aspect of the four items could reasonably be
subject to a parametric conditions.

It's only because of the requirement that the side effect be mediated
by the server, and as we've seen, that's not really a strong
distinction. Clients have to be configured to use either appropriate
parameters or appropriate interfaces in both cases.


## Alternate names

It was recently suggested to me that a good alternate name for the
"copy" view would be "template". So,

    GET /wats/{id}/template

This would free up "copy" to be used as the name of the action, so
instead of `create_copy` it would be, just `copy` and the request
would be:

    POST /wats/{id}/copy

Although, I like the semantics of this a lot, I'm going to stick with
calling the action `create_copy`. For one thing, I expect to get into
examples which may not have good semantic names. If you only have one
word for the action and it will serve as a view, I think a convention
of naming specialized actions after one of the basic actions like
this:

-   `create_*`
-   `update_*`
-   `destroy_*`

will probably work well to corral our interfaces into ones that
better abide by REST constraints.


# Side effect by delegated client action

If the side effect is something that can be independently invoked,
then delegating the invokation to the client may be justified. This
could allow us to use the standard `create` interface for copying the
objects while the side effect interface is used when the client needs
that side effect.

<figure class="centered">
  <svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" contentStyleType="text/css" height="529px" preserveAspectRatio="none" style="width:473px;height:529px;background:#FFFFFF;" version="1.1" viewBox="0 0 473 529" width="473px" zoomAndPan="magnify"><defs/><g><rect fill="none" height="352.0156" style="stroke:#000000;stroke-width:1.5;" width="456.5" x="10" y="125.1094"/><rect fill="none" height="133.1973" style="stroke:#000000;stroke-width:1.5;" width="402.5" x="20" y="250.3516"/><line style="stroke:#181818;stroke-width:0.5;stroke-dasharray:5.0,5.0;" x1="57" x2="57" y1="36.4883" y2="494.125"/><line style="stroke:#181818;stroke-width:0.5;stroke-dasharray:5.0,5.0;" x1="196.5" x2="196.5" y1="36.4883" y2="494.125"/><line style="stroke:#181818;stroke-width:0.5;stroke-dasharray:5.0,5.0;" x1="351.5" x2="351.5" y1="36.4883" y2="494.125"/><rect fill="#E2E2F0" height="30.4883" rx="2.5" ry="2.5" style="stroke:#181818;stroke-width:0.5;" width="54" x="30" y="5"/><text fill="#000000" font-family="sans-serif" font-size="14" lengthAdjust="spacing" textLength="40" x="37" y="25.5352">Client</text><rect fill="#E2E2F0" height="30.4883" rx="2.5" ry="2.5" style="stroke:#181818;stroke-width:0.5;" width="54" x="30" y="493.125"/><text fill="#000000" font-family="sans-serif" font-size="14" lengthAdjust="spacing" textLength="40" x="37" y="513.6602">Client</text><rect fill="#E2E2F0" height="30.4883" rx="2.5" ry="2.5" style="stroke:#181818;stroke-width:0.5;" width="167" x="113.5" y="5"/><text fill="#000000" font-family="sans-serif" font-size="14" lengthAdjust="spacing" textLength="153" x="120.5" y="25.5352">WatsController#create</text><rect fill="#E2E2F0" height="30.4883" rx="2.5" ry="2.5" style="stroke:#181818;stroke-width:0.5;" width="167" x="113.5" y="493.125"/><text fill="#000000" font-family="sans-serif" font-size="14" lengthAdjust="spacing" textLength="153" x="120.5" y="513.6602">WatsController#create</text><rect fill="#E2E2F0" height="30.4883" rx="2.5" ry="2.5" style="stroke:#181818;stroke-width:0.5;" width="122" x="290.5" y="5"/><text fill="#000000" font-family="sans-serif" font-size="14" lengthAdjust="spacing" textLength="108" x="297.5" y="25.5352">SomethingsElse</text><rect fill="#E2E2F0" height="30.4883" rx="2.5" ry="2.5" style="stroke:#181818;stroke-width:0.5;" width="122" x="290.5" y="493.125"/><text fill="#000000" font-family="sans-serif" font-size="14" lengthAdjust="spacing" textLength="108" x="297.5" y="513.6602">SomethingsElse</text><polygon fill="#181818" points="185,63.7988,195,67.7988,185,71.7988,189,67.7988" style="stroke:#181818;stroke-width:1.0;"/><line style="stroke:#181818;stroke-width:1.0;" x1="57" x2="191" y1="67.7988" y2="67.7988"/><text fill="#000000" font-family="sans-serif" font-size="13" lengthAdjust="spacing" textLength="72" x="64" y="63.0566">POST /wats</text><line style="stroke:#181818;stroke-width:1.0;stroke-dasharray:2.0,2.0;" x1="197" x2="239" y1="97.1094" y2="97.1094"/><line style="stroke:#181818;stroke-width:1.0;stroke-dasharray:2.0,2.0;" x1="239" x2="239" y1="97.1094" y2="110.1094"/><line style="stroke:#181818;stroke-width:1.0;stroke-dasharray:2.0,2.0;" x1="198" x2="239" y1="110.1094" y2="110.1094"/><polygon fill="#181818" points="208,106.1094,198,110.1094,208,114.1094,204,110.1094" style="stroke:#181818;stroke-width:1.0;"/><text fill="#000000" font-family="sans-serif" font-size="13" lengthAdjust="spacing" textLength="64" x="204" y="92.3672">create wat</text><path d="M10,125.1094 L72,125.1094 L72,132.4199 L62,142.4199 L10,142.4199 L10,125.1094 " fill="#EEEEEE" style="stroke:#000000;stroke-width:1.5;"/><rect fill="none" height="352.0156" style="stroke:#000000;stroke-width:1.5;" width="456.5" x="10" y="125.1094"/><text fill="#000000" font-family="sans-serif" font-size="13" font-weight="bold" lengthAdjust="spacing" textLength="17" x="25" y="138.6777">alt</text><text fill="#000000" font-family="sans-serif" font-size="11" font-weight="bold" lengthAdjust="spacing" textLength="51" x="87" y="137.7441">[success]</text><polygon fill="#181818" points="68,142.4199,58,146.4199,68,150.4199,64,146.4199" style="stroke:#181818;stroke-width:1.0;"/><line style="stroke:#181818;stroke-width:1.0;" x1="62" x2="196" y1="146.4199" y2="146.4199"/><text fill="#000000" font-family="sans-serif" font-size="13" lengthAdjust="spacing" textLength="76" x="74" y="159.9883">201 Created</text><polygon fill="#181818" points="339.5,189.041,349.5,193.041,339.5,197.041,343.5,193.041" style="stroke:#181818;stroke-width:1.0;"/><line style="stroke:#181818;stroke-width:1.0;" x1="57" x2="345.5" y1="193.041" y2="193.041"/><text fill="#000000" font-family="sans-serif" font-size="13" lengthAdjust="spacing" textLength="96" x="64" y="188.2988">something else</text><line style="stroke:#181818;stroke-width:1.0;stroke-dasharray:2.0,2.0;" x1="351.5" x2="393.5" y1="222.3516" y2="222.3516"/><line style="stroke:#181818;stroke-width:1.0;stroke-dasharray:2.0,2.0;" x1="393.5" x2="393.5" y1="222.3516" y2="235.3516"/><line style="stroke:#181818;stroke-width:1.0;stroke-dasharray:2.0,2.0;" x1="352.5" x2="393.5" y1="235.3516" y2="235.3516"/><polygon fill="#181818" points="362.5,231.3516,352.5,235.3516,362.5,239.3516,358.5,235.3516" style="stroke:#181818;stroke-width:1.0;"/><text fill="#000000" font-family="sans-serif" font-size="13" lengthAdjust="spacing" textLength="96" x="358.5" y="217.6094">something else</text><path d="M20,250.3516 L82,250.3516 L82,257.6621 L72,267.6621 L20,267.6621 L20,250.3516 " fill="#EEEEEE" style="stroke:#000000;stroke-width:1.5;"/><rect fill="none" height="133.1973" style="stroke:#000000;stroke-width:1.5;" width="402.5" x="20" y="250.3516"/><text fill="#000000" font-family="sans-serif" font-size="13" font-weight="bold" lengthAdjust="spacing" textLength="17" x="35" y="263.9199">alt</text><text fill="#000000" font-family="sans-serif" font-size="11" font-weight="bold" lengthAdjust="spacing" textLength="51" x="97" y="262.9863">[success]</text><polygon fill="#181818" points="68,267.6621,58,271.6621,68,275.6621,64,271.6621" style="stroke:#181818;stroke-width:1.0;"/><line style="stroke:#181818;stroke-width:1.0;" x1="62" x2="350.5" y1="271.6621" y2="271.6621"/><text fill="#000000" font-family="sans-serif" font-size="13" lengthAdjust="spacing" textLength="24" x="74" y="285.2305">2xx</text><line style="stroke:#000000;stroke-width:1.0;stroke-dasharray:2.0,2.0;" x1="20" x2="422.5" y1="297.9727" y2="297.9727"/><text fill="#000000" font-family="sans-serif" font-size="11" font-weight="bold" lengthAdjust="spacing" textLength="45" x="25" y="308.6074">[failure]</text><polygon fill="#181818" points="68,311.9277,58,315.9277,68,319.9277,64,315.9277" style="stroke:#181818;stroke-width:1.0;"/><line style="stroke:#181818;stroke-width:1.0;" x1="62" x2="350.5" y1="315.9277" y2="315.9277"/><text fill="#000000" font-family="sans-serif" font-size="13" lengthAdjust="spacing" textLength="61" x="74" y="329.4961">4xx | 5xx</text><line style="stroke:#181818;stroke-width:1.0;stroke-dasharray:2.0,2.0;" x1="57" x2="99" y1="362.5488" y2="362.5488"/><line style="stroke:#181818;stroke-width:1.0;stroke-dasharray:2.0,2.0;" x1="99" x2="99" y1="362.5488" y2="375.5488"/><line style="stroke:#181818;stroke-width:1.0;stroke-dasharray:2.0,2.0;" x1="58" x2="99" y1="375.5488" y2="375.5488"/><polygon fill="#181818" points="68,371.5488,58,375.5488,68,379.5488,64,375.5488" style="stroke:#181818;stroke-width:1.0;"/><text fill="#000000" font-family="sans-serif" font-size="13" lengthAdjust="spacing" textLength="126" x="64" y="357.8066">show error message</text><line style="stroke:#000000;stroke-width:1.0;stroke-dasharray:2.0,2.0;" x1="10" x2="466.5" y1="391.5488" y2="391.5488"/><text fill="#000000" font-family="sans-serif" font-size="11" font-weight="bold" lengthAdjust="spacing" textLength="45" x="15" y="402.1836">[failure]</text><polygon fill="#181818" points="68,405.5039,58,409.5039,68,413.5039,64,409.5039" style="stroke:#181818;stroke-width:1.0;"/><line style="stroke:#181818;stroke-width:1.0;" x1="62" x2="196" y1="409.5039" y2="409.5039"/><text fill="#000000" font-family="sans-serif" font-size="13" lengthAdjust="spacing" textLength="61" x="74" y="423.0723">4xx | 5xx</text><line style="stroke:#181818;stroke-width:1.0;stroke-dasharray:2.0,2.0;" x1="57" x2="99" y1="456.125" y2="456.125"/><line style="stroke:#181818;stroke-width:1.0;stroke-dasharray:2.0,2.0;" x1="99" x2="99" y1="456.125" y2="469.125"/><line style="stroke:#181818;stroke-width:1.0;stroke-dasharray:2.0,2.0;" x1="58" x2="99" y1="469.125" y2="469.125"/><polygon fill="#181818" points="68,465.125,58,469.125,68,473.125,64,469.125" style="stroke:#181818;stroke-width:1.0;"/><text fill="#000000" font-family="sans-serif" font-size="13" lengthAdjust="spacing" textLength="126" x="64" y="451.3828">show error message</text><!--MD5=[8bfc1f473156805ea54ff7ef95eadef0]
@startuml
skinparam responseMessageBelowArrow true

participant Client
participant "WatsController#create" as Server1
participant "SomethingsElse" as Server2

Client -> Server1 : POST /wats
Server1 - -> Server1 : create wat
alt success
  Server1 -> Client : 201 Created
  Client -> Server2 : something else
  Server2 - -> Server2 : something else
  alt success
    Server2 -> Client : 2xx
  else failure
    Server2 -> Client : 4xx | 5xx
    Client - -> Client : show error message
  end
else failure
  Server1 -> Client : 4xx | 5xx
  Client - -> Client : show error message
end
@enduml

PlantUML version 1.2022.12(Sun Oct 23 14:12:26 EDT 2022)
(GPL source distribution)
Java Runtime: OpenJDK Runtime Environment
JVM: OpenJDK 64-Bit Server VM
Default Encoding: UTF-8
Language: en
Country: US
--></g></svg>
</figure>

What's nice about this is that it relies on abilities client already
has, so the development effort lies in configuring the client to use
it. I did add some error handling for this one to give a flavor of the
kind of planning one would actually have to do.

However, with this solution great responsibility falls upon the
client. This raises a lot of questions about how clients are
configured, where business logic is defined, and how it is
communicated to the entities that carry it out. We'll have to come
back to this too.


# Summary and conclusion

Reviewing the implementations and commentary I see a kind of decision
tree emerging from some of the questions:

-   Who should be responsible for triggering the side effect?
    -   server: maybe use specialized action
    -   client:
        -   can the side effect be triggered independently?
            -   yes: create a separate action for the effect
            -   no: parameterize an existing action

Other questions asked:

-   How does a side effect feature:
    -   effect representation of resources?
    -   effect user interaction with resources?
-   How shall spurious uses of the interfaces and parameters be handled?
-   For parameterized interfaces, what would suggest that "implicit"
    rather than "explicit" parameters be used?
    -   nomenclature of "implicit/explicit" seems weird, but here's what I
        mean by them:
        -   "explicit" parameters are options the user controls, examples:
            -   query params
            -   request body
        -   "implicit" parameters are more like session states the user
            doesn't control, examples:
            -   referer (or other) header
            -   client or user information encoded in an access token
-   How can we discover useful semantics for the interfaces or
    parameters?
    -   (Note to self, I should work on my "Lexicode" project some more.)
-   How can we configure and/or communicate business level requirements
    to clients?

That's a lot of future posts!

Writing this has been really hard. One thing I keep stumbling over in
the revision process has been what I'm calling the "nomenclature
knot". There's a lot of jargon, and even though I'm trying to be
careful with it, I may not be using it as well as I should, so I think
I'm going to take a crack at that next.

This is complicated, nuanced stuff, if I can't come up with simple
language for writing about it, I will have to strive to be clear about
the language I use, as coherent about it as I can, and provide context
if consistency is a problem&#x2013;for example if two domains share some
terminology.

We'll see what I come up with.

