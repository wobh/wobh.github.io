---
layout: post
title: 'Web Application Design: Deletes resource, hard and soft'
date: 2023-03-06T12:45-0500
tags:
- ruby-on-rails
- web-application-design
--- 

I talked about the "nomenlature knot" in [the last post](file:///2022/12/03/wad-crud-copy-actions-with-side-effects.html) but after
trying to write about it a few times, I came to realize this topic has
more rabbit holes than <span class="underline">Watership Down</span>. So, I'm deferring my post(s)
and maybe starting a new series on it. Since I would rather move
forward with the next posts in this series, lets go on with variations
on delete actions.

Lots of applications complicate or even replace the action of deleting
resources but preserve for the user the notional interface of
deletion. There's many good reasons for this&#x2013;mainly to smooth recovery
from accidents with an "undoable" delete. I usually hear this called a
"soft delete". Lets examine a "soft delete" feature, discuss what
makes this different from a "hide" feature, and the setup for a "hard
delete" which actually deletes something.

<!-- more -->

Here's a feature description for "soft delete"

```feature
Given a user visiting the view for Wat
When the user clicks on the delete button to submit the Wat delete request
Then the Wat property "deleted_at" is updated with the current datetime
And the user redirected to Wat index view
And the "deleted" Wat is not shown on the index
And the user is presented with an alert informing that the Wat resource was deleted
```

This is the same feature we might write for a normal delete but
instead of deleting the feature we are updating a property of the
resource: in our example, `deleted_at` is updated with a current
datetime. 

This feature description could be fairly criticized as being too bound
up in the implementation details of the model for a proper "behavior
driven development" feature definition. But the effects have to be
specified somewhere and the justification for that implementation
also. For this discussion, we have to know that instead of deleting a
record in a DB, we're updating a property of that record.

We'll examing two options for implementing it:

-   change the "delete" button to send a hidden form updating the
    `deleted_at`
-   change the `WatsController#delete` action to update `Wat#deleted_at`
    instead

Let's go.


# Delete button sends update request

 <figure class="centered">
  <svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" contentStyleType="text/css" height="223px" preserveAspectRatio="none" style="width:365px;height:223px;background:#FFFFFF;" version="1.1" viewBox="0 0 365 223" width="365px" zoomAndPan="magnify"><defs/><g><line style="stroke:#181818;stroke-width:0.5;stroke-dasharray:5.0,5.0;" x1="32" x2="32" y1="36.4883" y2="188.041"/><line style="stroke:#181818;stroke-width:0.5;stroke-dasharray:5.0,5.0;" x1="272.5" x2="272.5" y1="36.4883" y2="188.041"/><rect fill="#E2E2F0" height="30.4883" rx="2.5" ry="2.5" style="stroke:#181818;stroke-width:0.5;" width="54" x="5" y="5"/><text fill="#000000" font-family="sans-serif" font-size="14" lengthAdjust="spacing" textLength="40" x="12" y="25.5352">Client</text><rect fill="#E2E2F0" height="30.4883" rx="2.5" ry="2.5" style="stroke:#181818;stroke-width:0.5;" width="54" x="5" y="187.041"/><text fill="#000000" font-family="sans-serif" font-size="14" lengthAdjust="spacing" textLength="40" x="12" y="207.5762">Client</text><rect fill="#E2E2F0" height="30.4883" rx="2.5" ry="2.5" style="stroke:#181818;stroke-width:0.5;" width="173" x="186.5" y="5"/><text fill="#000000" font-family="sans-serif" font-size="14" lengthAdjust="spacing" textLength="159" x="193.5" y="25.5352">WatsController#update</text><rect fill="#E2E2F0" height="30.4883" rx="2.5" ry="2.5" style="stroke:#181818;stroke-width:0.5;" width="173" x="186.5" y="187.041"/><text fill="#000000" font-family="sans-serif" font-size="14" lengthAdjust="spacing" textLength="159" x="193.5" y="207.5762">WatsController#update</text><polygon fill="#181818" points="261,79.1094,271,83.1094,261,87.1094,265,83.1094" style="stroke:#181818;stroke-width:1.0;"/><line style="stroke:#181818;stroke-width:1.0;" x1="32" x2="267" y1="83.1094" y2="83.1094"/><text fill="#000000" font-family="sans-serif" font-size="13" lengthAdjust="spacing" textLength="139" x="39" y="63.0566">PATCH /wats/{wat_id}</text><text fill="#000000" font-family="sans-serif" font-size="13" lengthAdjust="spacing" textLength="217" x="39" y="78.3672">deleted_at={current_datetime_utc}</text><line style="stroke:#181818;stroke-width:1.0;stroke-dasharray:2.0,2.0;" x1="273" x2="315" y1="112.4199" y2="112.4199"/><line style="stroke:#181818;stroke-width:1.0;stroke-dasharray:2.0,2.0;" x1="315" x2="315" y1="112.4199" y2="125.4199"/><line style="stroke:#181818;stroke-width:1.0;stroke-dasharray:2.0,2.0;" x1="274" x2="315" y1="125.4199" y2="125.4199"/><polygon fill="#181818" points="284,121.4199,274,125.4199,284,129.4199,280,125.4199" style="stroke:#181818;stroke-width:1.0;"/><text fill="#000000" font-family="sans-serif" font-size="13" lengthAdjust="spacing" textLength="69" x="280" y="107.6777">update wat</text><polygon fill="#181818" points="43,133.4199,33,137.4199,43,141.4199,39,137.4199" style="stroke:#181818;stroke-width:1.0;"/><line style="stroke:#181818;stroke-width:1.0;" x1="37" x2="272" y1="137.4199" y2="137.4199"/><text fill="#000000" font-family="sans-serif" font-size="13" lengthAdjust="spacing" textLength="88" x="49" y="150.9883">303 See Other</text><text fill="#000000" font-family="sans-serif" font-size="13" lengthAdjust="spacing" textLength="104" x="49" y="166.2988">Location: /index</text><!--SRC=[LOwz2i9048JxUuf5QubW9oXe6Wj5GS5624iowE7bBknjZ22-koS6qMQB-VbcuzNwXeLh4iGc-8WDOkGpbd2XMuY4ZbHQ69DIQalRi5VAdONNFsbmP8rvy2h1ESYmRIfM38WZxI4tY36V5etclK9JsYqE-OgoBdMpHxg5hPudNy51KHMiiqVPYgHMyQQfhL6qMZvD3nZziZwBb2XVUzv_cjp9U9AYeArU82U_3YMh3NvAcKrxznS0]--></g></svg>
</figure>

What's nice about this implentation is that, the pretense of deletion
is implemented by client labeling, and the API action is "normal" even
though it's not what the user interface says is happening. This
implementation is most amenable if the "deleted" resources are
exposable and manageable by users, in which case, maybe the semantics
of the whole feature should be reconsidered. Perhaps instead of
"delete" and "undelete" the actions should be labeled with "hide" and
"show" or "remove" and "restore" or something like that. 

This sort of "conceal/reveal" semantics should probably be reflected
in the model, which may mean rewriting the feature description. There
could be different kinds perhaps implemented different ways for
different properties. That'll have to be discussed in a different post
though. For this post, allow, that, if we're not updated a property
called `deleted_at`, we could just as easily be updating a property
with more semantically appropriate name.


# Delete action updates property

<figure class="centered">
  <svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" contentStyleType="text/css" height="208px" preserveAspectRatio="none" style="width:488px;height:208px;background:#FFFFFF;" version="1.1" viewBox="0 0 488 208" width="488px" zoomAndPan="magnify"><defs/><g><line style="stroke:#181818;stroke-width:0.5;stroke-dasharray:5.0,5.0;" x1="32" x2="32" y1="36.4883" y2="172.7305"/><line style="stroke:#181818;stroke-width:0.5;stroke-dasharray:5.0,5.0;" x1="197.5" x2="197.5" y1="36.4883" y2="172.7305"/><rect fill="#E2E2F0" height="30.4883" rx="2.5" ry="2.5" style="stroke:#181818;stroke-width:0.5;" width="54" x="5" y="5"/><text fill="#000000" font-family="sans-serif" font-size="14" lengthAdjust="spacing" textLength="40" x="12" y="25.5352">Client</text><rect fill="#E2E2F0" height="30.4883" rx="2.5" ry="2.5" style="stroke:#181818;stroke-width:0.5;" width="54" x="5" y="171.7305"/><text fill="#000000" font-family="sans-serif" font-size="14" lengthAdjust="spacing" textLength="40" x="12" y="192.2656">Client</text><rect fill="#E2E2F0" height="30.4883" rx="2.5" ry="2.5" style="stroke:#181818;stroke-width:0.5;" width="167" x="114.5" y="5"/><text fill="#000000" font-family="sans-serif" font-size="14" lengthAdjust="spacing" textLength="153" x="121.5" y="25.5352">WatsController#delete</text><rect fill="#E2E2F0" height="30.4883" rx="2.5" ry="2.5" style="stroke:#181818;stroke-width:0.5;" width="167" x="114.5" y="171.7305"/><text fill="#000000" font-family="sans-serif" font-size="14" lengthAdjust="spacing" textLength="153" x="121.5" y="192.2656">WatsController#delete</text><polygon fill="#181818" points="186,63.7988,196,67.7988,186,71.7988,190,67.7988" style="stroke:#181818;stroke-width:1.0;"/><line style="stroke:#181818;stroke-width:1.0;" x1="32" x2="192" y1="67.7988" y2="67.7988"/><text fill="#000000" font-family="sans-serif" font-size="13" lengthAdjust="spacing" textLength="142" x="39" y="63.0566">DELETE /wats/{wat_id}</text><line style="stroke:#181818;stroke-width:1.0;stroke-dasharray:2.0,2.0;" x1="198" x2="240" y1="97.1094" y2="97.1094"/><line style="stroke:#181818;stroke-width:1.0;stroke-dasharray:2.0,2.0;" x1="240" x2="240" y1="97.1094" y2="110.1094"/><line style="stroke:#181818;stroke-width:1.0;stroke-dasharray:2.0,2.0;" x1="199" x2="240" y1="110.1094" y2="110.1094"/><polygon fill="#181818" points="209,106.1094,199,110.1094,209,114.1094,205,110.1094" style="stroke:#181818;stroke-width:1.0;"/><text fill="#000000" font-family="sans-serif" font-size="13" lengthAdjust="spacing" textLength="276" x="205" y="92.3672">update wat deleted_at current_datetime_utc</text><polygon fill="#181818" points="43,118.1094,33,122.1094,43,126.1094,39,122.1094" style="stroke:#181818;stroke-width:1.0;"/><line style="stroke:#181818;stroke-width:1.0;" x1="37" x2="197" y1="122.1094" y2="122.1094"/><text fill="#000000" font-family="sans-serif" font-size="13" lengthAdjust="spacing" textLength="88" x="49" y="135.6777">303 See Other</text><text fill="#000000" font-family="sans-serif" font-size="13" lengthAdjust="spacing" textLength="104" x="49" y="150.9883">Location: /index</text><!--SRC=[LOxD2i8m48JlUOf5pwL2RpqK_EbDyQ3WHIYX6JGOaxBPMa5yToCMqSkopCmtJBnOtsdMLsB4BlY83MBK9opWmZ1d3WC9zr0gfSIsjjDUQEailFn9auEMk0nUE3W7dXeu22Qa8-t0Dx1I7ugoQbIef5MzhlSrvKE2yqUwZJLFDThPRxBlZ1PGYj2dsZJfRNlcrDcyFR5NDBsqNxmQToQwc1Mf2hILC_Zerw7LOeClARVUuFu2]--></g></svg>
</figure>

Here the server owns the feature, and client's actions are co-opted to
implement the "soft delete". This is good if the feature should be
hidden from the users and clients. However if so, we require other
sorts of management features for dealing with the "deleted" features.


# Summary and conclusion

The soft delete works through filtering&#x2013;the index view automatically
excludes the deletion property we called `deleted_at`. Some questions
to ask are:

-   When is exposing the deletion property to the client desirable?
-   How can users see a list of "deleted" resources?
-   How can users reverse or "undelete" deleted resources?
-   How can a resource be "hard deleted"?
    -   automatic "garbage collection" process
    -   administrative delete action
-   How can "lifecycle" changes like this be logged, reported on, or
    audited?

If soft deleted resources are managable by users, I think it's worth
considering whether the semantics should be shifted to a "hide/show"
sort of labelling. The property to be updated would be changed, and
the controls labeled for concealing and revealing resources.

This feature's dependency on what I'm just going to call "application
semantics" really stands out to me in a way that didn't with the copy
features. I'm planning to write about "conceal/reveal" features
soon-if-not-next. But I'm going to have to come up with a way to talk
oabout this "application semantics" thing also.

