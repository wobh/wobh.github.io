---
layout: post
title: "Web Application Design: Ruby on Rails scaffolded interfaces"
date: "2022-11-01T08:0-0400"
tags: 
  - web-application-design
  - ruby-on-rails
---

A while back I started writing a post about web application design and
it got too big and weird for a single post or even introducing what I
wanted to say. So I'm going to start with something short, simple, and
obvious: what you get using a Rails scaffold.

<!-- more -->

Ruby on Rails has a pretty good generator system and that includes a
couple "scaffold" generators which are helpful to illustrate, if not
automate, "the Rails way" of developing a basic CRUD interface.

If you run:

    rails generate scaffold wat name:string

A bunch of things will get setup in the application to support the new
model and interface. After a little more set up you can run the Rails
app, visit `/wats` and start creating `Wat` objects.

Here's a table based on the one in the rails routes for this example
resource:

<table>
  <thead>
    <tr>
      <th>HTTP method</th>
      <th>resource path</th>
      <th>controller action</th>
    </tr>
  </thead>

  <tbody>
    <tr>
      <td><code>GET</code></td>
      <td><code>/wats</code></td>
      <td><code>WatsController#index</code></td>
    </tr>
    <tr>
      <td><code>GET</code></td>
      <td><code>/wats/new</code></td>
      <td><code>WatsController#new</code></td>
    </tr>
    <tr>
      <td><code>GET</code></td>
      <td><code>/wats/{id}</code></td>
      <td><code>WatsController#show</code></td>
    </tr>
    <tr>
      <td><code>GET</code></td>
      <td><code>/wats/{id}/edit</code></td>
      <td><code>WatsController#edit</code></td>
    </tr>
    <tr>
      <td><code>POST</code></td>
      <td><code>/wats</code></td>
      <td><code>WatsController#create</code></td>
    </tr>
    <tr>
      <td><code>PUT/PATCH</code></td>
      <td><code>/wats/{id}</code></td>
      <td><code>WatsController#update</code></td>
    </tr>
    <tr>
      <td><code>DELETE</code></td>
      <td><code>/wats/{id}</code></td>
      <td><code>WatsController#destroy</code></td>
    </tr>
  </tbody>
</table>

That's worth looking at closely, but what I'm sharing today is a
diagram I made of the links and actions of the scaffold generated
views. The idea is to show the workflow that's created by the
scaffolding.

<figure>
  <svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" contentStyleType="text/css" height="503px" preserveAspectRatio="none" style="width:521px;height:503px;background:#FFFFFF;" version="1.1" viewBox="0 0 521 503" width="521px" zoomAndPan="magnify"><defs/><g><ellipse cx="214.5" cy="16" fill="#222222" rx="10" ry="10" style="stroke:none;stroke-width:1.0;"/><rect fill="#F1F1F1" height="132" rx="12.5" ry="12.5" style="stroke:#181818;stroke-width:0.5;" width="168" x="130.5" y="67"/><image height="113" width="149" x="140.5" xlink:href="data:image/svg+xml;base64,PHN2ZyBoZWlnaHQ9IjExMyIgd2lkdGg9IjE0OSIgeG1sbnM6eGxpbms9Imh0dHA6Ly93d3cudzMub3JnLzE5OTkveGxpbmsiIHhtbG5zPSJodHRwOi8vd3d3LnczLm9yZy8yMDAwL3N2ZyIgPjxkZWZzLz48Zz48cmVjdCBmaWxsPSIjRkZGRkZGIiBzdHlsZT0id2lkdGg6MTQ5cHg7aGVpZ2h0OjExM3B4O2JhY2tncm91bmQ6I0ZGRkZGRjsiIC8+IDx0ZXh0IGZpbGw9IiMwMDAwMDAiIGZvbnQtZmFtaWx5PSJzYW5zLXNlcmlmIiBmb250LXNpemU9IjE2IiBsZW5ndGhBZGp1c3Q9InNwYWNpbmciIHRleHRMZW5ndGg9IjM3IiB4PSI2IiB5PSIyMS40Njg4Ij5XYXRzPC90ZXh0Pjx0ZXh0IGZpbGw9IiMwMDAwMDAiIGZvbnQtZmFtaWx5PSJzYW5zLXNlcmlmIiBmb250LXNpemU9IjEyIiBmb250LXdlaWdodD0iYm9sZCIgbGVuZ3RoQWRqdXN0PSJzcGFjaW5nIiB0ZXh0TGVuZ3RoPSIzNSIgeD0iNyIgeT0iMzkuNDQ1MyI+TmFtZTwvdGV4dD48dGV4dCBmaWxsPSIjMDAwMDAwIiBmb250LWZhbWlseT0ic2Fucy1zZXJpZiIgZm9udC1zaXplPSIxMiIgbGVuZ3RoQWRqdXN0PSJzcGFjaW5nIiB0ZXh0TGVuZ3RoPSIyMCIgeD0iNyIgeT0iNTUuNTc4MSI+d2F0PC90ZXh0Pjx0ZXh0IGZpbGw9IiMwMDAwMDAiIGZvbnQtZmFtaWx5PSJzYW5zLXNlcmlmIiBmb250LXNpemU9IjEyIiBsZW5ndGhBZGp1c3Q9InNwYWNpbmciIHRleHQtZGVjb3JhdGlvbj0idW5kZXJsaW5lIiB0ZXh0TGVuZ3RoPSIyOSIgeD0iNDQiIHk9IjU1LjU3ODEiPnNob3c8L3RleHQ+PHRleHQgZmlsbD0iIzAwMDAwMCIgZm9udC1mYW1pbHk9InNhbnMtc2VyaWYiIGZvbnQtc2l6ZT0iMTIiIGxlbmd0aEFkanVzdD0ic3BhY2luZyIgdGV4dC1kZWNvcmF0aW9uPSJ1bmRlcmxpbmUiIHRleHRMZW5ndGg9IjIyIiB4PSI3NSIgeT0iNTUuNTc4MSI+ZWRpdDwvdGV4dD48dGV4dCBmaWxsPSIjMDAwMDAwIiBmb250LWZhbWlseT0ic2Fucy1zZXJpZiIgZm9udC1zaXplPSIxMiIgbGVuZ3RoQWRqdXN0PSJzcGFjaW5nIiB0ZXh0LWRlY29yYXRpb249InVuZGVybGluZSIgdGV4dExlbmd0aD0iNDMiIHg9Ijk5IiB5PSI1NS41NzgxIj5kZXN0cm95PC90ZXh0Pjx0ZXh0IGZpbGw9IiMwMDAwMDAiIGZvbnQtZmFtaWx5PSJzYW5zLXNlcmlmIiBmb250LXNpemU9IjEyIiBsZW5ndGhBZGp1c3Q9InNwYWNpbmciIHRleHRMZW5ndGg9IjI3IiB4PSI3IiB5PSI3MS43MTA5Ij53YWF0PC90ZXh0Pjx0ZXh0IGZpbGw9IiMwMDAwMDAiIGZvbnQtZmFtaWx5PSJzYW5zLXNlcmlmIiBmb250LXNpemU9IjEyIiBsZW5ndGhBZGp1c3Q9InNwYWNpbmciIHRleHQtZGVjb3JhdGlvbj0idW5kZXJsaW5lIiB0ZXh0TGVuZ3RoPSIyOSIgeD0iNDQiIHk9IjcxLjcxMDkiPnNob3c8L3RleHQ+PHRleHQgZmlsbD0iIzAwMDAwMCIgZm9udC1mYW1pbHk9InNhbnMtc2VyaWYiIGZvbnQtc2l6ZT0iMTIiIGxlbmd0aEFkanVzdD0ic3BhY2luZyIgdGV4dC1kZWNvcmF0aW9uPSJ1bmRlcmxpbmUiIHRleHRMZW5ndGg9IjIyIiB4PSI3NSIgeT0iNzEuNzEwOSI+ZWRpdDwvdGV4dD48dGV4dCBmaWxsPSIjMDAwMDAwIiBmb250LWZhbWlseT0ic2Fucy1zZXJpZiIgZm9udC1zaXplPSIxMiIgbGVuZ3RoQWRqdXN0PSJzcGFjaW5nIiB0ZXh0LWRlY29yYXRpb249InVuZGVybGluZSIgdGV4dExlbmd0aD0iNDMiIHg9Ijk5IiB5PSI3MS43MTA5Ij5kZXN0cm95PC90ZXh0Pjx0ZXh0IGZpbGw9IiMwMDAwMDAiIGZvbnQtZmFtaWx5PSJzYW5zLXNlcmlmIiBmb250LXNpemU9IjEyIiBsZW5ndGhBZGp1c3Q9InNwYWNpbmciIHRleHRMZW5ndGg9IjM0IiB4PSI3IiB5PSI4Ny44NDM4Ij53YWFhdDwvdGV4dD48dGV4dCBmaWxsPSIjMDAwMDAwIiBmb250LWZhbWlseT0ic2Fucy1zZXJpZiIgZm9udC1zaXplPSIxMiIgbGVuZ3RoQWRqdXN0PSJzcGFjaW5nIiB0ZXh0LWRlY29yYXRpb249InVuZGVybGluZSIgdGV4dExlbmd0aD0iMjkiIHg9IjQ0IiB5PSI4Ny44NDM4Ij5zaG93PC90ZXh0Pjx0ZXh0IGZpbGw9IiMwMDAwMDAiIGZvbnQtZmFtaWx5PSJzYW5zLXNlcmlmIiBmb250LXNpemU9IjEyIiBsZW5ndGhBZGp1c3Q9InNwYWNpbmciIHRleHQtZGVjb3JhdGlvbj0idW5kZXJsaW5lIiB0ZXh0TGVuZ3RoPSIyMiIgeD0iNzUiIHk9Ijg3Ljg0MzgiPmVkaXQ8L3RleHQ+PHRleHQgZmlsbD0iIzAwMDAwMCIgZm9udC1mYW1pbHk9InNhbnMtc2VyaWYiIGZvbnQtc2l6ZT0iMTIiIGxlbmd0aEFkanVzdD0ic3BhY2luZyIgdGV4dC1kZWNvcmF0aW9uPSJ1bmRlcmxpbmUiIHRleHRMZW5ndGg9IjQzIiB4PSI5OSIgeT0iODcuODQzOCI+ZGVzdHJveTwvdGV4dD48dGV4dCBmaWxsPSIjMDAwMDAwIiBmb250LWZhbWlseT0ic2Fucy1zZXJpZiIgZm9udC1zaXplPSIxMiIgbGVuZ3RoQWRqdXN0PSJzcGFjaW5nIiB0ZXh0LWRlY29yYXRpb249InVuZGVybGluZSIgdGV4dExlbmd0aD0iNTAiIHg9IjYiIHk9IjEwNC45NzY2Ij5OZXcgV2F0PC90ZXh0PjwhLS1NRDU9WzU1N2MxZTBkNzFkNDI1NDgzZDIwNGIwMjc2ODNkMjY5XQpAc3RhcnR1bWwNCnNhbHQNCnsNCjxzaXplOjE2PldhdHM8L3NpemU+DQp7DQo8Yj5OYW1lIHwgLiB8IC4gfCAuDQp3YXQgfCA8dT5zaG93PC91PiB8IDx1PmVkaXQ8L3U+IHwgPHU+ZGVzdHJveTwvdT4NCndhYXQgfCA8dT5zaG93PC91PiB8IDx1PmVkaXQ8L3U+IHwgPHU+ZGVzdHJveTwvdT4NCndhYWF0IHwgPHU+c2hvdzwvdT4gfCA8dT5lZGl0PC91PiB8IDx1PmRlc3Ryb3k8L3U+DQp9DQo8dT5OZXcgV2F0PC91Pg0KfQ0KQGVuZHVtbA0KClBsYW50VU1MIHZlcnNpb24gMS4yMDIyLjEyKFN1biBPY3QgMjMgMTQ6MTI6MjYgRURUIDIwMjIpCihHUEwgc291cmNlIGRpc3RyaWJ1dGlvbikKSmF2YSBSdW50aW1lOiBPcGVuSkRLIFJ1bnRpbWUgRW52aXJvbm1lbnQKSlZNOiBPcGVuSkRLIDY0LUJpdCBTZXJ2ZXIgVk0KRGVmYXVsdCBFbmNvZGluZzogVVRGLTgKTGFuZ3VhZ2U6IGVuCkNvdW50cnk6IFVTCi0tPjwvZz48L3N2Zz4=" y="77"/><rect fill="#F1F1F1" height="63" rx="12.5" ry="12.5" style="stroke:#181818;stroke-width:0.5;" width="93" x="204" y="434"/><image height="44" width="74" x="214" xlink:href="data:image/svg+xml;base64,PHN2ZyBoZWlnaHQ9IjQ0IiB3aWR0aD0iNzQiIHhtbG5zOnhsaW5rPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5L3hsaW5rIiB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciID48ZGVmcy8+PGc+PHJlY3QgZmlsbD0iI0ZGRkZGRiIgc3R5bGU9IndpZHRoOjc0cHg7aGVpZ2h0OjQ0cHg7YmFja2dyb3VuZDojRkZGRkZGOyIgLz4gPHRleHQgZmlsbD0iIzAwMDAwMCIgZm9udC1mYW1pbHk9InNhbnMtc2VyaWYiIGZvbnQtc2l6ZT0iMTIiIGZvbnQtd2VpZ2h0PSJib2xkIiBsZW5ndGhBZGp1c3Q9InNwYWNpbmciIHRleHRMZW5ndGg9IjM4IiB4PSI3IiB5PSIxOC42MDE2Ij5OYW1lOjwvdGV4dD48dGV4dCBmaWxsPSIjMDAwMDAwIiBmb250LWZhbWlseT0ic2Fucy1zZXJpZiIgZm9udC1zaXplPSIxMiIgbGVuZ3RoQWRqdXN0PSJzcGFjaW5nIiB0ZXh0TGVuZ3RoPSIyMCIgeD0iNDciIHk9IjE4LjYwMTYiPndhdDwvdGV4dD48dGV4dCBmaWxsPSIjMDAwMDAwIiBmb250LWZhbWlseT0ic2Fucy1zZXJpZiIgZm9udC1zaXplPSIxMiIgbGVuZ3RoQWRqdXN0PSJzcGFjaW5nIiB0ZXh0LWRlY29yYXRpb249InVuZGVybGluZSIgdGV4dExlbmd0aD0iMjIiIHg9IjYiIHk9IjM1LjczNDQiPkVkaXQ8L3RleHQ+PHRleHQgZmlsbD0iIzAwMDAwMCIgZm9udC1mYW1pbHk9InNhbnMtc2VyaWYiIGZvbnQtc2l6ZT0iMTIiIGxlbmd0aEFkanVzdD0ic3BhY2luZyIgdGV4dExlbmd0aD0iNCIgeD0iMzIiIHk9IjM1LjczNDQiPnw8L3RleHQ+PHRleHQgZmlsbD0iIzAwMDAwMCIgZm9udC1mYW1pbHk9InNhbnMtc2VyaWYiIGZvbnQtc2l6ZT0iMTIiIGxlbmd0aEFkanVzdD0ic3BhY2luZyIgdGV4dC1kZWNvcmF0aW9uPSJ1bmRlcmxpbmUiIHRleHRMZW5ndGg9IjI3IiB4PSI0MCIgeT0iMzUuNzM0NCI+QmFjazwvdGV4dD48IS0tTUQ1PVs2MTVmNjBmZmZkYmE0ZjI3NzZjMTRkZjA4OGIxMzVlNF0KQHN0YXJ0dW1sDQpzYWx0DQp7DQp7DQo8Yj5OYW1lOjwvYj4gfCB3YXQNCn0NCjx1PkVkaXQ8L3U+IDxVKzAwN2M+IDx1PkJhY2s8L3U+DQp9DQpAZW5kdW1sDQoKUGxhbnRVTUwgdmVyc2lvbiAxLjIwMjIuMTIoU3VuIE9jdCAyMyAxNDoxMjoyNiBFRFQgMjAyMikKKEdQTCBzb3VyY2UgZGlzdHJpYnV0aW9uKQpKYXZhIFJ1bnRpbWU6IE9wZW5KREsgUnVudGltZSBFbnZpcm9ubWVudApKVk06IE9wZW5KREsgNjQtQml0IFNlcnZlciBWTQpEZWZhdWx0IEVuY29kaW5nOiBVVEYtOApMYW5ndWFnZTogZW4KQ291bnRyeTogVVMKLS0+PC9nPjwvc3ZnPg==" y="444"/><rect fill="#F1F1F1" height="127" rx="12.5" ry="12.5" style="stroke:#181818;stroke-width:0.5;" width="117" x="22" y="253"/><image height="108" width="98" x="32" xlink:href="data:image/svg+xml;base64,PHN2ZyBoZWlnaHQ9IjEwOCIgd2lkdGg9Ijk4IiB4bWxuczp4bGluaz0iaHR0cDovL3d3dy53My5vcmcvMTk5OS94bGluayIgeG1sbnM9Imh0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnIiA+PGRlZnMvPjxnPjxyZWN0IGZpbGw9IiNGRkZGRkYiIHN0eWxlPSJ3aWR0aDo5OHB4O2hlaWdodDoxMDhweDtiYWNrZ3JvdW5kOiNGRkZGRkY7IiAvPiA8dGV4dCBmaWxsPSIjMDAwMDAwIiBmb250LWZhbWlseT0ic2Fucy1zZXJpZiIgZm9udC1zaXplPSIxNiIgbGVuZ3RoQWRqdXN0PSJzcGFjaW5nIiB0ZXh0TGVuZ3RoPSI2NyIgeD0iNiIgeT0iMjEuNDY4OCI+TmV3IFdhdDwvdGV4dD48dGV4dCBmaWxsPSIjMDAwMDAwIiBmb250LWZhbWlseT0ic2Fucy1zZXJpZiIgZm9udC1zaXplPSIxMiIgbGVuZ3RoQWRqdXN0PSJzcGFjaW5nIiB0ZXh0TGVuZ3RoPSIzNCIgeD0iNyIgeT0iMzkuNDQ1MyI+TmFtZTwvdGV4dD48dGV4dCBmaWxsPSIjMDAwMDAwIiBmb250LWZhbWlseT0ic2Fucy1zZXJpZiIgZm9udC1zaXplPSIxMiIgbGVuZ3RoQWRqdXN0PSJzcGFjaW5nIiB0ZXh0TGVuZ3RoPSI0IiB4PSIxMCIgeT0iNTUuNTc4MSI+wqA8L3RleHQ+PGxpbmUgc3R5bGU9InN0cm9rZTojMDAwMDAwO3N0cm9rZS13aWR0aDoxLjA7IiB4MT0iOCIgeDI9IjkxIiB5MT0iNTguMTA5NCIgeTI9IjU4LjEwOTQiLz48bGluZSBzdHlsZT0ic3Ryb2tlOiMwMDAwMDA7c3Ryb2tlLXdpZHRoOjEuMDsiIHgxPSI4IiB4Mj0iOCIgeTE9IjU1LjEwOTQiIHkyPSI1Ny4xMDk0Ii8+PGxpbmUgc3R5bGU9InN0cm9rZTojMDAwMDAwO3N0cm9rZS13aWR0aDoxLjA7IiB4MT0iOTEiIHgyPSI5MSIgeTE9IjU1LjEwOTQiIHkyPSI1Ny4xMDk0Ii8+PHJlY3QgZmlsbD0iI0VFRUVFRSIgaGVpZ2h0PSIxOC4xMzI4IiByeD0iNSIgcnk9IjUiIHN0eWxlPSJzdHJva2U6IzAwMDAwMDtzdHJva2Utd2lkdGg6Mi41OyIgd2lkdGg9Ijg0IiB4PSI4LjUiIHk9IjY1LjYwOTQiLz48dGV4dCBmaWxsPSIjMDAwMDAwIiBmb250LWZhbWlseT0ic2Fucy1zZXJpZiIgZm9udC1zaXplPSIxMiIgbGVuZ3RoQWRqdXN0PSJzcGFjaW5nIiB0ZXh0TGVuZ3RoPSI2MyIgeD0iMTkiIHk9Ijc5LjIxMDkiPkNyZWF0ZSBXYXQ8L3RleHQ+PHRleHQgZmlsbD0iIzAwMDAwMCIgZm9udC1mYW1pbHk9InNhbnMtc2VyaWYiIGZvbnQtc2l6ZT0iMTIiIGxlbmd0aEFkanVzdD0ic3BhY2luZyIgdGV4dC1kZWNvcmF0aW9uPSJ1bmRlcmxpbmUiIHRleHRMZW5ndGg9IjI3IiB4PSI2IiB5PSI5OS44NDM4Ij5CYWNrPC90ZXh0PjwhLS1NRDU9WzA1YzkzY2I4MTNhYzBiZDIwOTkwMjQ1ZTk3NDEzM2RlXQpAc3RhcnR1bWwNCnNhbHQNCnsNCjxzaXplOjE2Pk5ldyBXYXQ8L3NpemU+DQp7DQpOYW1lDQoiICAgICAgICAgICINCn0NCltDcmVhdGUgV2F0XQ0KPHU+QmFjazwvdT4NCn0NCkBlbmR1bWwNCgpQbGFudFVNTCB2ZXJzaW9uIDEuMjAyMi4xMihTdW4gT2N0IDIzIDE0OjEyOjI2IEVEVCAyMDIyKQooR1BMIHNvdXJjZSBkaXN0cmlidXRpb24pCkphdmEgUnVudGltZTogT3BlbkpESyBSdW50aW1lIEVudmlyb25tZW50CkpWTTogT3BlbkpESyA2NC1CaXQgU2VydmVyIFZNCkRlZmF1bHQgRW5jb2Rpbmc6IFVURi04Ckxhbmd1YWdlOiBlbgpDb3VudHJ5OiBVUwotLT48L2c+PC9zdmc+" y="263"/><rect fill="#F1F1F1" height="127" rx="12.5" ry="12.5" style="stroke:#181818;stroke-width:0.5;" width="121" x="297" y="253"/><image height="108" width="102" x="307" xlink:href="data:image/svg+xml;base64,PHN2ZyBoZWlnaHQ9IjEwOCIgd2lkdGg9IjEwMiIgeG1sbnM6eGxpbms9Imh0dHA6Ly93d3cudzMub3JnLzE5OTkveGxpbmsiIHhtbG5zPSJodHRwOi8vd3d3LnczLm9yZy8yMDAwL3N2ZyIgPjxkZWZzLz48Zz48cmVjdCBmaWxsPSIjRkZGRkZGIiBzdHlsZT0id2lkdGg6MTAycHg7aGVpZ2h0OjEwOHB4O2JhY2tncm91bmQ6I0ZGRkZGRjsiIC8+IDx0ZXh0IGZpbGw9IiMwMDAwMDAiIGZvbnQtZmFtaWx5PSJzYW5zLXNlcmlmIiBmb250LXNpemU9IjE2IiBsZW5ndGhBZGp1c3Q9InNwYWNpbmciIHRleHRMZW5ndGg9Ijg5IiB4PSI2IiB5PSIyMS40Njg4Ij5FZGl0aW5nIFdhdDwvdGV4dD48dGV4dCBmaWxsPSIjMDAwMDAwIiBmb250LWZhbWlseT0ic2Fucy1zZXJpZiIgZm9udC1zaXplPSIxMiIgbGVuZ3RoQWRqdXN0PSJzcGFjaW5nIiB0ZXh0TGVuZ3RoPSIzNCIgeD0iNyIgeT0iMzkuNDQ1MyI+TmFtZTwvdGV4dD48dGV4dCBmaWxsPSIjMDAwMDAwIiBmb250LWZhbWlseT0ic2Fucy1zZXJpZiIgZm9udC1zaXplPSIxMiIgbGVuZ3RoQWRqdXN0PSJzcGFjaW5nIiB0ZXh0TGVuZ3RoPSIyMCIgeD0iMTAiIHk9IjU1LjU3ODEiPndhdDwvdGV4dD48bGluZSBzdHlsZT0ic3Ryb2tlOiMwMDAwMDA7c3Ryb2tlLXdpZHRoOjEuMDsiIHgxPSI4IiB4Mj0iOTEiIHkxPSI1OC4xMDk0IiB5Mj0iNTguMTA5NCIvPjxsaW5lIHN0eWxlPSJzdHJva2U6IzAwMDAwMDtzdHJva2Utd2lkdGg6MS4wOyIgeDE9IjgiIHgyPSI4IiB5MT0iNTUuMTA5NCIgeTI9IjU3LjEwOTQiLz48bGluZSBzdHlsZT0ic3Ryb2tlOiMwMDAwMDA7c3Ryb2tlLXdpZHRoOjEuMDsiIHgxPSI5MSIgeDI9IjkxIiB5MT0iNTUuMTA5NCIgeTI9IjU3LjEwOTQiLz48cmVjdCBmaWxsPSIjRUVFRUVFIiBoZWlnaHQ9IjE4LjEzMjgiIHJ4PSI1IiByeT0iNSIgc3R5bGU9InN0cm9rZTojMDAwMDAwO3N0cm9rZS13aWR0aDoyLjU7IiB3aWR0aD0iODQiIHg9IjguNSIgeT0iNjUuNjA5NCIvPjx0ZXh0IGZpbGw9IiMwMDAwMDAiIGZvbnQtZmFtaWx5PSJzYW5zLXNlcmlmIiBmb250LXNpemU9IjEyIiBsZW5ndGhBZGp1c3Q9InNwYWNpbmciIHRleHRMZW5ndGg9IjY3IiB4PSIxNyIgeT0iNzkuMjEwOSI+VXBkYXRlIFdhdDwvdGV4dD48dGV4dCBmaWxsPSIjMDAwMDAwIiBmb250LWZhbWlseT0ic2Fucy1zZXJpZiIgZm9udC1zaXplPSIxMiIgbGVuZ3RoQWRqdXN0PSJzcGFjaW5nIiB0ZXh0LWRlY29yYXRpb249InVuZGVybGluZSIgdGV4dExlbmd0aD0iMjkiIHg9IjYiIHk9Ijk5Ljg0MzgiPlNob3c8L3RleHQ+PHRleHQgZmlsbD0iIzAwMDAwMCIgZm9udC1mYW1pbHk9InNhbnMtc2VyaWYiIGZvbnQtc2l6ZT0iMTIiIGxlbmd0aEFkanVzdD0ic3BhY2luZyIgdGV4dExlbmd0aD0iNCIgeD0iMzkiIHk9Ijk5Ljg0MzgiPnw8L3RleHQ+PHRleHQgZmlsbD0iIzAwMDAwMCIgZm9udC1mYW1pbHk9InNhbnMtc2VyaWYiIGZvbnQtc2l6ZT0iMTIiIGxlbmd0aEFkanVzdD0ic3BhY2luZyIgdGV4dC1kZWNvcmF0aW9uPSJ1bmRlcmxpbmUiIHRleHRMZW5ndGg9IjI3IiB4PSI0NyIgeT0iOTkuODQzOCI+QmFjazwvdGV4dD48IS0tTUQ1PVs1Y2I2NTYxOTM2ZTc0YmFiNzFkYWM2M2ZkZTRmOGNjYl0KQHN0YXJ0dW1sDQpzYWx0DQp7DQo8c2l6ZToxNj5FZGl0aW5nIFdhdDwvc2l6ZT4NCnsNCk5hbWUNCiJ3YXQgICAgICAgIg0KfQ0KW1VwZGF0ZSBXYXRdDQo8dT5TaG93PC91PiA8VSswMDdjPiA8dT5CYWNrPC91Pg0KfQ0KQGVuZHVtbA0KClBsYW50VU1MIHZlcnNpb24gMS4yMDIyLjEyKFN1biBPY3QgMjMgMTQ6MTI6MjYgRURUIDIwMjIpCihHUEwgc291cmNlIGRpc3RyaWJ1dGlvbikKSmF2YSBSdW50aW1lOiBPcGVuSkRLIFJ1bnRpbWUgRW52aXJvbm1lbnQKSlZNOiBPcGVuSkRLIDY0LUJpdCBTZXJ2ZXIgVk0KRGVmYXVsdCBFbmNvZGluZzogVVRGLTgKTGFuZ3VhZ2U6IGVuCkNvdW50cnk6IFVTCi0tPjwvZz48L3N2Zz4=" y="263"/><rect fill="#F1F1F1" height="48.2656" rx="12.5" ry="12.5" style="stroke:#181818;stroke-width:0.5;" width="154" x="361.5" y="109"/><text fill="#000000" font-family="sans-serif" font-size="12" lengthAdjust="spacing" textLength="134" x="371.5" y="130.6016">WatsController#destroy</text><text fill="#000000" font-family="sans-serif" font-size="12" lengthAdjust="spacing" textLength="98" x="371.5" y="144.7344">redirect_to index</text><rect fill="#F1F1F1" height="48.2656" rx="12.5" ry="12.5" style="stroke:#181818;stroke-width:0.5;" width="147" x="7" y="441.5"/><text fill="#000000" font-family="sans-serif" font-size="12" lengthAdjust="spacing" textLength="127" x="17" y="463.1016">WatsController#create</text><text fill="#000000" font-family="sans-serif" font-size="12" lengthAdjust="spacing" textLength="95" x="17" y="477.2344">redirect_to show</text><rect fill="#F1F1F1" height="48.2656" rx="12.5" ry="12.5" style="stroke:#181818;stroke-width:0.5;" width="152" x="346.5" y="441.5"/><text fill="#000000" font-family="sans-serif" font-size="12" lengthAdjust="spacing" textLength="132" x="356.5" y="463.1016">WatsController#update</text><text fill="#000000" font-family="sans-serif" font-size="12" lengthAdjust="spacing" textLength="95" x="356.5" y="477.2344">redirect_to show</text><!--MD5=[552613f025e83fd74f2ded4ec6498215]
link start to index--><g id="link_start_index"><path d="M214.5,26.41 C214.5,34.46 214.5,46.86 214.5,60.36 " fill="none" id="start-to-index" style="stroke:#181818;stroke-width:1.0;"/><polygon fill="#181818" points="214.5,65.12,218.5,56.12,214.5,60.12,210.5,56.12,214.5,65.12" style="stroke:#181818;stroke-width:1.0;"/></g><!--MD5=[d60135a85fe9e8d8fe27515af6f095ad]
link index to show--><g id="link_index_show"><path d="M196.31,199.35 C185.34,249.74 176.61,320.61 194.5,380 C199.75,397.41 209.88,414.53 220.1,428.76 " fill="none" id="index-to-show" style="stroke:#181818;stroke-width:1.0;"/><polygon fill="#181818" points="222.96,432.65,220.8205,423.0363,219.9852,428.6312,214.3904,427.796,222.96,432.65" style="stroke:#181818;stroke-width:1.0;"/><text fill="#000000" font-family="sans-serif" font-size="11" lengthAdjust="spacing" textLength="28" x="195.5" y="321.1348">show</text></g><!--MD5=[4626843acef478dc7e3b7780ac9b5ba1]
reverse link index to show--><g id="link_index_show"><path d="M222.3,205.63 C230.04,276.63 241.52,382.04 247.15,433.7 " fill="none" id="index-backto-show" style="stroke:#181818;stroke-width:1.0;"/><polygon fill="#181818" points="221.78,200.8,218.7806,210.181,222.3228,205.7705,226.7333,209.3126,221.78,200.8" style="stroke:#181818;stroke-width:1.0;"/><text fill="#000000" font-family="sans-serif" font-size="11" lengthAdjust="spacing" textLength="25" x="241.5" y="321.1348">back</text></g><!--MD5=[6f049cd4aa77a477c6904fe8c2e62b7d]
link index to new--><g id="link_index_new"><path d="M130.32,173.77 C113.05,185.86 96.97,200.86 86.5,219 C81.72,227.29 78.77,236.67 77.07,246.25 " fill="none" id="index-to-new" style="stroke:#181818;stroke-width:1.0;"/><polygon fill="#181818" points="76.35,251.07,81.6432,242.7644,77.0931,246.1255,73.732,241.5755,76.35,251.07" style="stroke:#181818;stroke-width:1.0;"/><text fill="#000000" font-family="sans-serif" font-size="11" lengthAdjust="spacing" textLength="21" x="87.5" y="230.6348">new</text></g><!--MD5=[1c1a96ba1d0e4573be4287ece55c0694]
reverse link index to new--><g id="link_index_new"><path d="M148.08,204.09 C144.05,209.04 140.15,214.04 136.5,219 C128.75,229.53 121.2,241.22 114.27,252.72 " fill="none" id="index-backto-new" style="stroke:#181818;stroke-width:1.0;"/><polygon fill="#181818" points="151.19,200.33,142.3746,204.722,148.006,204.1851,148.5428,209.8164,151.19,200.33" style="stroke:#181818;stroke-width:1.0;"/><text fill="#000000" font-family="sans-serif" font-size="11" lengthAdjust="spacing" textLength="25" x="137.5" y="230.6348">back</text></g><!--MD5=[d9b189f20c49232791e04e042e54c1d9]
link index to edit--><g id="link_index_edit"><path d="M265.92,199.27 C278.33,215.02 291.62,231.88 304.12,247.74 " fill="none" id="index-to-edit" style="stroke:#181818;stroke-width:1.0;"/><polygon fill="#181818" points="307.03,251.44,304.595,241.8969,303.9327,247.5149,298.3148,246.8526,307.03,251.44" style="stroke:#181818;stroke-width:1.0;"/><text fill="#000000" font-family="sans-serif" font-size="11" lengthAdjust="spacing" textLength="20" x="290.5" y="230.6348">edit</text></g><!--MD5=[481b7ef10163829e27359c2d0eab7dc1]
reverse link index to edit--><g id="link_index_edit"><path d="M303.24,203.07 C307.98,208.22 312.45,213.55 316.5,219 C324.04,229.16 330.51,240.91 335.94,252.65 " fill="none" id="index-backto-edit" style="stroke:#181818;stroke-width:1.0;"/><polygon fill="#181818" points="299.85,199.48,303.1055,208.7753,303.2768,203.121,308.9311,203.2923,299.85,199.48" style="stroke:#181818;stroke-width:1.0;"/><text fill="#000000" font-family="sans-serif" font-size="11" lengthAdjust="spacing" textLength="25" x="324.5" y="230.6348">back</text></g><!--MD5=[5c24189927eff948a0d43fee09e5c954]
link index to destroy--><g id="link_index_destroy"><path d="M298.87,133 C317.25,133 336.69,133 355.04,133 " fill="none" id="index-to-destroy" style="stroke:#181818;stroke-width:1.0;"/><polygon fill="#181818" points="359.71,133,350.71,129,354.71,133,350.71,137,359.71,133" style="stroke:#181818;stroke-width:1.0;"/><text fill="#000000" font-family="sans-serif" font-size="11" lengthAdjust="spacing" textLength="41" x="309.5" y="127.6348">destroy</text></g><!--MD5=[297e511babf8be0e6e1e8046a259f254]
reverse link index to destroy--><g id="link_index_destroy"><path d="M305.18,149.97 C318.84,151 332.78,151.2 346,150 C350.96,149.55 356.05,148.96 361.17,148.28 " fill="none" id="index-backto-destroy" style="stroke:#181818;stroke-width:1.0;"/><polygon fill="#181818" points="300.44,149.57,309.0632,154.3282,305.4215,149.9994,309.7503,146.3578,300.44,149.57" style="stroke:#181818;stroke-width:1.0;"/><text fill="#000000" font-family="sans-serif" font-size="11" lengthAdjust="spacing" textLength="30" x="315" y="147.6348">index</text></g><!--MD5=[6cddfb4084259a1bd964848322e1f2a6]
link new to create--><g id="link_new_create"><path d="M80.5,380.27 C80.5,399.11 80.5,418.92 80.5,434.59 " fill="none" id="new-to-create" style="stroke:#181818;stroke-width:1.0;"/><polygon fill="#181818" points="80.5,439.52,84.5,430.52,80.5,434.52,76.5,430.52,80.5,439.52" style="stroke:#181818;stroke-width:1.0;"/><text fill="#000000" font-family="sans-serif" font-size="11" lengthAdjust="spacing" textLength="33" x="81.5" y="411.6348">create</text></g><!--MD5=[0b2425b8578287f4e2f0561628404ad9]
link create to show--><g id="link_create_show"><path d="M154.08,465.5 C168.71,465.5 183.79,465.5 197.57,465.5 " fill="none" id="create-to-show" style="stroke:#181818;stroke-width:1.0;"/><polygon fill="#181818" points="202.28,465.5,193.28,461.5,197.28,465.5,193.28,469.5,202.28,465.5" style="stroke:#181818;stroke-width:1.0;"/><text fill="#000000" font-family="sans-serif" font-size="11" lengthAdjust="spacing" textLength="28" x="165" y="460.1348">show</text></g><!--MD5=[2e619884978e139b57dc4508f5bda552]
link edit to update--><g id="link_edit_update"><path d="M385.25,380.27 C393.74,399.45 402.66,419.63 409.64,435.43 " fill="none" id="edit-to-update" style="stroke:#181818;stroke-width:1.0;"/><polygon fill="#181818" points="411.51,439.65,411.5157,429.8011,409.482,435.0798,404.2033,433.046,411.51,439.65" style="stroke:#181818;stroke-width:1.0;"/><text fill="#000000" font-family="sans-serif" font-size="11" lengthAdjust="spacing" textLength="37" x="400.5" y="411.6348">update</text></g><!--MD5=[6780bdf8607217fa8168538b105faceb]
reverse link show to update--><g id="link_show_update"><path d="M303.95,465.5 C317.3,465.5 331.88,465.5 346.1,465.5 " fill="none" id="show-backto-update" style="stroke:#181818;stroke-width:1.0;"/><polygon fill="#181818" points="299.01,465.5,308.01,469.5,304.01,465.5,308.01,461.5,299.01,465.5" style="stroke:#181818;stroke-width:1.0;"/><text fill="#000000" font-family="sans-serif" font-size="11" lengthAdjust="spacing" textLength="28" x="307.75" y="460.1348">show</text></g><!--MD5=[e1824fd4ea3a73870af0494bca17db3e]
link edit to show--><g id="link_edit_show"><path d="M311.81,380.27 C299.87,396.67 287.4,413.8 276.85,428.3 " fill="none" id="edit-to-show" style="stroke:#181818;stroke-width:1.0;"/><polygon fill="#181818" points="273.95,432.29,282.4806,427.3676,276.8926,428.2476,276.0126,422.6596,273.95,432.29" style="stroke:#181818;stroke-width:1.0;"/><text fill="#000000" font-family="sans-serif" font-size="11" lengthAdjust="spacing" textLength="28" x="294.5" y="411.6348">show</text></g><!--MD5=[ca84e10651d6f05f1a32fba289aa9d0c]
reverse link edit to show--><g id="link_edit_show"><path d="M343.49,386.15 C339.64,396.06 334.73,405.7 328.5,414 C320.27,424.97 308.9,434.22 297.39,441.68 " fill="none" id="edit-backto-show" style="stroke:#181818;stroke-width:1.0;"/><polygon fill="#181818" points="345.12,381.71,338.2593,388.7761,343.3941,386.4027,345.7676,391.5375,345.12,381.71" style="stroke:#181818;stroke-width:1.0;"/><text fill="#000000" font-family="sans-serif" font-size="11" lengthAdjust="spacing" textLength="20" x="337.5" y="411.6348">edit</text></g><!--MD5=[e16778d89b09776c5ce38ac0d3b4c611]
@startuml
!unquoted procedure VIEW($view)
"{ {
salt
%invoke_procedure("_"+$view)
} }" as $view
!endprocedure

!unquoted procedure FORM($action, $field)
{
Name
$field
}
[$action Wat]
!endprocedure

!procedure _index()
{
<size:16>Wats</size>
{
<b>Name | . | . | .
wat | <u>show</u> | <u>edit</u> | <u>destroy</u>
waat | <u>show</u> | <u>edit</u> | <u>destroy</u>
waaat | <u>show</u> | <u>edit</u> | <u>destroy</u>
}
<u>New Wat</u>
}
!endprocedure

!procedure _new()
{
<size:16>New Wat</size>
FORM("Create", '"          "')
<u>Back</u>
}
!endprocedure

!procedure _edit()
{
<size:16>Editing Wat</size>
FORM("Update", '"wat       "')
<u>Show</u> <U+007c> <u>Back</u>
}
!endprocedure

!procedure _show()
{
{
<b>Name:</b> | wat
}
<u>Edit</u> <U+007c> <u>Back</u>
}
!endprocedure

(*) - -> VIEW(index)

index - -> [show] VIEW(show)
index - -> [new] VIEW(new)
index - -> [edit] VIEW(edit)

index -right-> [destroy] "WatsController#destroy
redirect_to index" as destroy
destroy -left-> [index] index

new - -> [create] "WatsController#create
redirect_to show" as create
create -right-> [show] show
new -up-> [back] index

edit - -> [update] "WatsController#update
redirect_to show" as update
update -left-> [show] show
edit - -> [show] show
edit -up-> [back] index

show -up-> [edit] edit
show -up-> [back] index
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

In the diagram are wireframes of the views with arrows for links and
actions take you, and giving a sense of the basic workflow.

Here's an abstraction of the same, distilling the interface to an
essential structure:

<figure>
  <!-- Generated by graphviz version 6.0.1 (20220911.1526)
    -->
  <!-- Title: wats_flow Pages: 1 -->
  <svg width="172pt" height="189pt"
       viewBox="0.00 0.00 172.00 188.99" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
    <g id="graph0" class="graph" transform="scale(1 1) rotate(0) translate(4 184.99)">
      <title>wats_flow</title>
      <polygon fill="white" stroke="none" points="-4,4 -4,-184.99 168,-184.99 168,4 -4,4"/>
      <!-- index -->
      <g id="node1" class="node">
	<title>index</title>
	<ellipse fill="none" stroke="black" cx="82" cy="-162.99" rx="30.59" ry="18"/>
	<text text-anchor="middle" x="82" y="-159.29" font-family="Times,serif" font-size="14.00">index</text>
      </g>
      <!-- index&#45;&gt;index -->
      <g id="edge1" class="edge">
	<title>index&#45;&gt;index</title>
	<path fill="none" stroke="black" d="M103.97,-175.53C117.59,-178.56 130.55,-174.38 130.55,-162.99 130.55,-154.53 123.41,-150.05 114.16,-149.54"/>
	<polygon fill="black" stroke="black" points="113.62,-146.07 103.97,-150.44 114.24,-153.04 113.62,-146.07"/>
	<text text-anchor="middle" x="127.52" y="-135.73" font-family="Times,serif" font-size="14.00">destroy</text>
      </g>
      <!-- new -->
      <g id="node2" class="node">
	<title>new</title>
	<ellipse fill="none" stroke="black" cx="27" cy="-90.99" rx="27" ry="18"/>
	<text text-anchor="middle" x="27" y="-87.29" font-family="Times,serif" font-size="14.00">new</text>
      </g>
      <!-- index&#45;&gt;new -->
      <g id="edge2" class="edge">
	<title>index&#45;&gt;new</title>
	<path fill="none" stroke="black" d="M69.79,-146.45C62.67,-137.39 53.55,-125.77 45.55,-115.6"/>
	<polygon fill="black" stroke="black" points="48.1,-113.18 39.17,-107.48 42.6,-117.5 48.1,-113.18"/>
      </g>
      <!-- show -->
      <g id="node3" class="node">
	<title>show</title>
	<ellipse fill="none" stroke="black" cx="82" cy="-18.99" rx="29.5" ry="18"/>
	<text text-anchor="middle" x="82" y="-15.29" font-family="Times,serif" font-size="14.00">show</text>
      </g>
      <!-- index&#45;&gt;show -->
      <g id="edge8" class="edge">
	<title>index&#45;&gt;show</title>
	<path fill="none" stroke="black" d="M82,-144.86C82,-120.66 82,-76.2 82,-47.38"/>
	<polygon fill="black" stroke="black" points="85.5,-47.18 82,-37.18 78.5,-47.18 85.5,-47.18"/>
      </g>
      <!-- edit -->
      <g id="node4" class="node">
	<title>edit</title>
	<ellipse fill="none" stroke="black" cx="137" cy="-90.99" rx="27" ry="18"/>
	<text text-anchor="middle" x="137" y="-87.29" font-family="Times,serif" font-size="14.00">edit</text>
      </g>
      <!-- index&#45;&gt;edit -->
      <g id="edge5" class="edge">
	<title>index&#45;&gt;edit</title>
	<path fill="none" stroke="black" d="M94.21,-146.45C101.33,-137.39 110.45,-125.77 118.45,-115.6"/>
	<polygon fill="black" stroke="black" points="121.4,-117.5 124.83,-107.48 115.9,-113.18 121.4,-117.5"/>
      </g>
      <!-- new&#45;&gt;show -->
      <g id="edge3" class="edge">
	<title>new:s&#45;&gt;show:w</title>
	<path fill="none" stroke="black" d="M27,-72.99C27,-50.62 25.36,-27.19 41.09,-20.72"/>
	<polygon fill="black" stroke="black" points="41.75,-24.16 51,-18.99 40.55,-17.27 41.75,-24.16"/>
	<text text-anchor="middle" x="34.63" y="-3.8" font-family="Times,serif" font-size="14.00">create</text>
      </g>
      <!-- show&#45;&gt;new -->
      <!-- show&#45;&gt;edit -->
      <g id="edge7" class="edge">
	<title>show&#45;&gt;edit</title>
	<path fill="none" stroke="black" d="M94.29,-35.63C101.47,-44.76 110.66,-56.46 118.68,-66.67"/>
	<polygon fill="black" stroke="black" points="116.14,-69.1 125.07,-74.8 121.64,-64.77 116.14,-69.1"/>
      </g>
      <!-- edit&#45;&gt;show -->
      <g id="edge6" class="edge">
	<title>edit:s&#45;&gt;show:e</title>
	<path fill="none" stroke="black" d="M137,-72.99C137,-50.62 138.64,-27.19 122.91,-20.72"/>
	<polygon fill="black" stroke="black" points="123.45,-17.27 113,-18.99 122.25,-24.16 123.45,-17.27"/>
	<text text-anchor="middle" x="129.37" y="-3.8" font-family="Times,serif" font-size="14.00">update</text>
      </g>
    </g>
  </svg>
</figure>

Having worked in Rails for a goodly while now, and having stared at
this setup and it's underlying code a lot, and having been required to
build new features, fix old bugs on many things built this way. I've
come to have some opinions of my own about things. If a picture is
worth a thousand words, then I'm hoping the table and diagrams will
give the mind some things to pattern match on and ask questions about.

Here's some thoughts and questions I've had to puzzle over which I
hope to explore in future posts in this series:

1.  Starting from the index the flow has two short paths into the
    "update" cycle. What alternative paths could be made? What ways are
    there out of the update cycle?
2.  Over time, the `wats` table fills up with entries. How can we setup
    searching and sorting features? What about aggregates and archives?
3.  If we need more specialized interfaces and actions, to support
    complex processes, how can we find "natural" ways of extending the
    structure we find here?

For the next post I want to explore the questions what ways are there
to add a feature for copying `Wat` entries, and how do we choose
between implementations?

