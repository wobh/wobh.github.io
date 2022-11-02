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
  <svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" contentStyleType="text/css" height="503px" preserveAspectRatio="none" style="width:521px;height:503px;background:#FFFFFF;" version="1.1" viewBox="0 0 521 503" width="521px" zoomAndPan="magnify"><defs/><g><ellipse cx="214.5" cy="16" fill="#222222" rx="10" ry="10" style="stroke:none;stroke-width:1.0;"/><rect fill="#F1F1F1" height="132" rx="12.5" ry="12.5" style="stroke:#181818;stroke-width:0.5;" width="168" x="130.5" y="67"/><image height="113" width="149" x="140.5" xlink:href="data:image/svg+xml;base64,PHN2ZyBoZWlnaHQ9IjExMyIgd2lkdGg9IjE0OSIgeG1sbnM6eGxpbms9Imh0dHA6Ly93d3cudzMub3JnLzE5OTkveGxpbmsiIHhtbG5zPSJodHRwOi8vd3d3LnczLm9yZy8yMDAwL3N2ZyIgPjxkZWZzLz48Zz48cmVjdCBmaWxsPSIjRkZGRkZGIiBzdHlsZT0id2lkdGg6MTQ5cHg7aGVpZ2h0OjExM3B4O2JhY2tncm91bmQ6I0ZGRkZGRjsiIC8+IDx0ZXh0IGZpbGw9IiMwMDAwMDAiIGZvbnQtZmFtaWx5PSJzYW5zLXNlcmlmIiBmb250LXNpemU9IjE2IiBsZW5ndGhBZGp1c3Q9InNwYWNpbmciIHRleHRMZW5ndGg9IjM3IiB4PSI2IiB5PSIyMS40Njg4Ij5XYXRzPC90ZXh0Pjx0ZXh0IGZpbGw9IiMwMDAwMDAiIGZvbnQtZmFtaWx5PSJzYW5zLXNlcmlmIiBmb250LXNpemU9IjEyIiBmb250LXdlaWdodD0iYm9sZCIgbGVuZ3RoQWRqdXN0PSJzcGFjaW5nIiB0ZXh0TGVuZ3RoPSIzNSIgeD0iNyIgeT0iMzkuNDQ1MyI+TmFtZTwvdGV4dD48dGV4dCBmaWxsPSIjMDAwMDAwIiBmb250LWZhbWlseT0ic2Fucy1zZXJpZiIgZm9udC1zaXplPSIxMiIgbGVuZ3RoQWRqdXN0PSJzcGFjaW5nIiB0ZXh0TGVuZ3RoPSIyMCIgeD0iNyIgeT0iNTUuNTc4MSI+d2F0PC90ZXh0Pjx0ZXh0IGZpbGw9IiMwMDAwMDAiIGZvbnQtZmFtaWx5PSJzYW5zLXNlcmlmIiBmb250LXNpemU9IjEyIiBsZW5ndGhBZGp1c3Q9InNwYWNpbmciIHRleHQtZGVjb3JhdGlvbj0idW5kZXJsaW5lIiB0ZXh0TGVuZ3RoPSIyOSIgeD0iNDQiIHk9IjU1LjU3ODEiPnNob3c8L3RleHQ+PHRleHQgZmlsbD0iIzAwMDAwMCIgZm9udC1mYW1pbHk9InNhbnMtc2VyaWYiIGZvbnQtc2l6ZT0iMTIiIGxlbmd0aEFkanVzdD0ic3BhY2luZyIgdGV4dC1kZWNvcmF0aW9uPSJ1bmRlcmxpbmUiIHRleHRMZW5ndGg9IjIyIiB4PSI3NSIgeT0iNTUuNTc4MSI+ZWRpdDwvdGV4dD48dGV4dCBmaWxsPSIjMDAwMDAwIiBmb250LWZhbWlseT0ic2Fucy1zZXJpZiIgZm9udC1zaXplPSIxMiIgbGVuZ3RoQWRqdXN0PSJzcGFjaW5nIiB0ZXh0LWRlY29yYXRpb249InVuZGVybGluZSIgdGV4dExlbmd0aD0iNDMiIHg9Ijk5IiB5PSI1NS41NzgxIj5kZXN0cm95PC90ZXh0Pjx0ZXh0IGZpbGw9IiMwMDAwMDAiIGZvbnQtZmFtaWx5PSJzYW5zLXNlcmlmIiBmb250LXNpemU9IjEyIiBsZW5ndGhBZGp1c3Q9InNwYWNpbmciIHRleHRMZW5ndGg9IjI3IiB4PSI3IiB5PSI3MS43MTA5Ij53YWF0PC90ZXh0Pjx0ZXh0IGZpbGw9IiMwMDAwMDAiIGZvbnQtZmFtaWx5PSJzYW5zLXNlcmlmIiBmb250LXNpemU9IjEyIiBsZW5ndGhBZGp1c3Q9InNwYWNpbmciIHRleHQtZGVjb3JhdGlvbj0idW5kZXJsaW5lIiB0ZXh0TGVuZ3RoPSIyOSIgeD0iNDQiIHk9IjcxLjcxMDkiPnNob3c8L3RleHQ+PHRleHQgZmlsbD0iIzAwMDAwMCIgZm9udC1mYW1pbHk9InNhbnMtc2VyaWYiIGZvbnQtc2l6ZT0iMTIiIGxlbmd0aEFkanVzdD0ic3BhY2luZyIgdGV4dC1kZWNvcmF0aW9uPSJ1bmRlcmxpbmUiIHRleHRMZW5ndGg9IjIyIiB4PSI3NSIgeT0iNzEuNzEwOSI+ZWRpdDwvdGV4dD48dGV4dCBmaWxsPSIjMDAwMDAwIiBmb250LWZhbWlseT0ic2Fucy1zZXJpZiIgZm9udC1zaXplPSIxMiIgbGVuZ3RoQWRqdXN0PSJzcGFjaW5nIiB0ZXh0LWRlY29yYXRpb249InVuZGVybGluZSIgdGV4dExlbmd0aD0iNDMiIHg9Ijk5IiB5PSI3MS43MTA5Ij5kZXN0cm95PC90ZXh0Pjx0ZXh0IGZpbGw9IiMwMDAwMDAiIGZvbnQtZmFtaWx5PSJzYW5zLXNlcmlmIiBmb250LXNpemU9IjEyIiBsZW5ndGhBZGp1c3Q9InNwYWNpbmciIHRleHRMZW5ndGg9IjM0IiB4PSI3IiB5PSI4Ny44NDM4Ij53YWFhdDwvdGV4dD48dGV4dCBmaWxsPSIjMDAwMDAwIiBmb250LWZhbWlseT0ic2Fucy1zZXJpZiIgZm9udC1zaXplPSIxMiIgbGVuZ3RoQWRqdXN0PSJzcGFjaW5nIiB0ZXh0LWRlY29yYXRpb249InVuZGVybGluZSIgdGV4dExlbmd0aD0iMjkiIHg9IjQ0IiB5PSI4Ny44NDM4Ij5zaG93PC90ZXh0Pjx0ZXh0IGZpbGw9IiMwMDAwMDAiIGZvbnQtZmFtaWx5PSJzYW5zLXNlcmlmIiBmb250LXNpemU9IjEyIiBsZW5ndGhBZGp1c3Q9InNwYWNpbmciIHRleHQtZGVjb3JhdGlvbj0idW5kZXJsaW5lIiB0ZXh0TGVuZ3RoPSIyMiIgeD0iNzUiIHk9Ijg3Ljg0MzgiPmVkaXQ8L3RleHQ+PHRleHQgZmlsbD0iIzAwMDAwMCIgZm9udC1mYW1pbHk9InNhbnMtc2VyaWYiIGZvbnQtc2l6ZT0iMTIiIGxlbmd0aEFkanVzdD0ic3BhY2luZyIgdGV4dC1kZWNvcmF0aW9uPSJ1bmRlcmxpbmUiIHRleHRMZW5ndGg9IjQzIiB4PSI5OSIgeT0iODcuODQzOCI+ZGVzdHJveTwvdGV4dD48dGV4dCBmaWxsPSIjMDAwMDAwIiBmb250LWZhbWlseT0ic2Fucy1zZXJpZiIgZm9udC1zaXplPSIxMiIgbGVuZ3RoQWRqdXN0PSJzcGFjaW5nIiB0ZXh0LWRlY29yYXRpb249InVuZGVybGluZSIgdGV4dExlbmd0aD0iNTAiIHg9IjYiIHk9IjEwNC45NzY2Ij5OZXcgV2F0PC90ZXh0PjwhLS1NRDU9WzlkOGU3OWI3YTMzODQ4Njc5ODJjOTNhODliZGE1NDQwXQpAc3RhcnR1bWwNCnNhbHQNCnsNCjxzaXplOjE2PldhdHM8L3NpemU+DQp7DQo8Yj5OYW1lIHwgLiB8IC4gfCAuDQp3YXQgfCA8dT5zaG93PC91PiB8IDx1PmVkaXQ8L3U+IHwgPHU+ZGVzdHJveTwvdT4NCndhYXQgfCA8dT5zaG93PC91PiB8IDx1PmVkaXQ8L3U+IHwgPHU+ZGVzdHJveTwvdT4NCndhYWF0IHwgPHU+c2hvdzwvdT4gfCA8dT5lZGl0PC91PiB8IDx1PmRlc3Ryb3k8L3U+DQp9DQo8dT5OZXcgV2F0PC91Pg0KfQ0KQGVuZHVtbA0KClBsYW50VU1MIHZlcnNpb24gMS4yMDIyLjcoTW9uIEF1ZyAyMiAxMzowMTozMCBFRFQgMjAyMikKKEdQTCBzb3VyY2UgZGlzdHJpYnV0aW9uKQpKYXZhIFJ1bnRpbWU6IE9wZW5KREsgUnVudGltZSBFbnZpcm9ubWVudApKVk06IE9wZW5KREsgNjQtQml0IFNlcnZlciBWTQpEZWZhdWx0IEVuY29kaW5nOiBVVEYtOApMYW5ndWFnZTogZW4KQ291bnRyeTogVVMKLS0+PC9nPjwvc3ZnPg==" y="77"/><rect fill="#F1F1F1" height="63" rx="12.5" ry="12.5" style="stroke:#181818;stroke-width:0.5;" width="94" x="203.5" y="434"/><image height="44" width="75" x="213.5" xlink:href="data:image/svg+xml;base64,PHN2ZyBoZWlnaHQ9IjQ0IiB3aWR0aD0iNzUiIHhtbG5zOnhsaW5rPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5L3hsaW5rIiB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciID48ZGVmcy8+PGc+PHJlY3QgZmlsbD0iI0ZGRkZGRiIgc3R5bGU9IndpZHRoOjc1cHg7aGVpZ2h0OjQ0cHg7YmFja2dyb3VuZDojRkZGRkZGOyIgLz4gPHRleHQgZmlsbD0iIzAwMDAwMCIgZm9udC1mYW1pbHk9InNhbnMtc2VyaWYiIGZvbnQtc2l6ZT0iMTIiIGZvbnQtd2VpZ2h0PSJib2xkIiBsZW5ndGhBZGp1c3Q9InNwYWNpbmciIHRleHRMZW5ndGg9IjM1IiB4PSI3IiB5PSIxOC42MDE2Ij5OYW1lPC90ZXh0Pjx0ZXh0IGZpbGw9IiMwMDAwMDAiIGZvbnQtZmFtaWx5PSJzYW5zLXNlcmlmIiBmb250LXNpemU9IjEyIiBsZW5ndGhBZGp1c3Q9InNwYWNpbmciIHRleHRMZW5ndGg9IjQiIHg9IjQyIiB5PSIxOC42MDE2Ij46PC90ZXh0Pjx0ZXh0IGZpbGw9IiMwMDAwMDAiIGZvbnQtZmFtaWx5PSJzYW5zLXNlcmlmIiBmb250LXNpemU9IjEyIiBsZW5ndGhBZGp1c3Q9InNwYWNpbmciIHRleHRMZW5ndGg9IjIwIiB4PSI0OCIgeT0iMTguNjAxNiI+d2F0PC90ZXh0Pjx0ZXh0IGZpbGw9IiMwMDAwMDAiIGZvbnQtZmFtaWx5PSJzYW5zLXNlcmlmIiBmb250LXNpemU9IjEyIiBsZW5ndGhBZGp1c3Q9InNwYWNpbmciIHRleHQtZGVjb3JhdGlvbj0idW5kZXJsaW5lIiB0ZXh0TGVuZ3RoPSIyMiIgeD0iNiIgeT0iMzUuNzM0NCI+RWRpdDwvdGV4dD48dGV4dCBmaWxsPSIjMDAwMDAwIiBmb250LWZhbWlseT0ic2Fucy1zZXJpZiIgZm9udC1zaXplPSIxMiIgbGVuZ3RoQWRqdXN0PSJzcGFjaW5nIiB0ZXh0TGVuZ3RoPSI0IiB4PSIyOCIgeT0iMzUuNzM0NCI+wqA8L3RleHQ+PHRleHQgZmlsbD0iIzAwMDAwMCIgZm9udC1mYW1pbHk9InNhbnMtc2VyaWYiIGZvbnQtc2l6ZT0iMTIiIGxlbmd0aEFkanVzdD0ic3BhY2luZyIgdGV4dC1kZWNvcmF0aW9uPSJ1bmRlcmxpbmUiIHRleHRMZW5ndGg9IjI3IiB4PSIzMiIgeT0iMzUuNzM0NCI+QmFjazwvdGV4dD48IS0tTUQ1PVsxZmUxYzVjMWEzMThhOGY4ZTkyOWVhOTdiMzBhMGRhNF0KQHN0YXJ0dW1sDQpzYWx0DQp7DQp7DQo8Yj5OYW1lPC9iPjogfCB3YXQNCn0NCjx1PkVkaXQ8L3U+IDx1PkJhY2s8L3U+DQp9DQpAZW5kdW1sDQoKUGxhbnRVTUwgdmVyc2lvbiAxLjIwMjIuNyhNb24gQXVnIDIyIDEzOjAxOjMwIEVEVCAyMDIyKQooR1BMIHNvdXJjZSBkaXN0cmlidXRpb24pCkphdmEgUnVudGltZTogT3BlbkpESyBSdW50aW1lIEVudmlyb25tZW50CkpWTTogT3BlbkpESyA2NC1CaXQgU2VydmVyIFZNCkRlZmF1bHQgRW5jb2Rpbmc6IFVURi04Ckxhbmd1YWdlOiBlbgpDb3VudHJ5OiBVUwotLT48L2c+PC9zdmc+" y="444"/><rect fill="#F1F1F1" height="127" rx="12.5" ry="12.5" style="stroke:#181818;stroke-width:0.5;" width="117" x="22" y="253"/><image height="108" width="98" x="32" xlink:href="data:image/svg+xml;base64,PHN2ZyBoZWlnaHQ9IjEwOCIgd2lkdGg9Ijk4IiB4bWxuczp4bGluaz0iaHR0cDovL3d3dy53My5vcmcvMTk5OS94bGluayIgeG1sbnM9Imh0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnIiA+PGRlZnMvPjxnPjxyZWN0IGZpbGw9IiNGRkZGRkYiIHN0eWxlPSJ3aWR0aDo5OHB4O2hlaWdodDoxMDhweDtiYWNrZ3JvdW5kOiNGRkZGRkY7IiAvPiA8dGV4dCBmaWxsPSIjMDAwMDAwIiBmb250LWZhbWlseT0ic2Fucy1zZXJpZiIgZm9udC1zaXplPSIxNiIgbGVuZ3RoQWRqdXN0PSJzcGFjaW5nIiB0ZXh0TGVuZ3RoPSI2NyIgeD0iNiIgeT0iMjEuNDY4OCI+TmV3IFdhdDwvdGV4dD48dGV4dCBmaWxsPSIjMDAwMDAwIiBmb250LWZhbWlseT0ic2Fucy1zZXJpZiIgZm9udC1zaXplPSIxMiIgbGVuZ3RoQWRqdXN0PSJzcGFjaW5nIiB0ZXh0TGVuZ3RoPSIzNCIgeD0iNyIgeT0iMzkuNDQ1MyI+TmFtZTwvdGV4dD48dGV4dCBmaWxsPSIjMDAwMDAwIiBmb250LWZhbWlseT0ic2Fucy1zZXJpZiIgZm9udC1zaXplPSIxMiIgbGVuZ3RoQWRqdXN0PSJzcGFjaW5nIiB0ZXh0TGVuZ3RoPSI0IiB4PSIxMCIgeT0iNTUuNTc4MSI+wqA8L3RleHQ+PGxpbmUgc3R5bGU9InN0cm9rZTojMDAwMDAwO3N0cm9rZS13aWR0aDoxLjA7IiB4MT0iOCIgeDI9IjkxIiB5MT0iNTguMTA5NCIgeTI9IjU4LjEwOTQiLz48bGluZSBzdHlsZT0ic3Ryb2tlOiMwMDAwMDA7c3Ryb2tlLXdpZHRoOjEuMDsiIHgxPSI4IiB4Mj0iOCIgeTE9IjU1LjEwOTQiIHkyPSI1Ny4xMDk0Ii8+PGxpbmUgc3R5bGU9InN0cm9rZTojMDAwMDAwO3N0cm9rZS13aWR0aDoxLjA7IiB4MT0iOTEiIHgyPSI5MSIgeTE9IjU1LjEwOTQiIHkyPSI1Ny4xMDk0Ii8+PHJlY3QgZmlsbD0iI0VFRUVFRSIgaGVpZ2h0PSIxOC4xMzI4IiByeD0iNSIgcnk9IjUiIHN0eWxlPSJzdHJva2U6IzAwMDAwMDtzdHJva2Utd2lkdGg6Mi41OyIgd2lkdGg9Ijg0IiB4PSI4LjUiIHk9IjY1LjYwOTQiLz48dGV4dCBmaWxsPSIjMDAwMDAwIiBmb250LWZhbWlseT0ic2Fucy1zZXJpZiIgZm9udC1zaXplPSIxMiIgbGVuZ3RoQWRqdXN0PSJzcGFjaW5nIiB0ZXh0TGVuZ3RoPSI2MyIgeD0iMTkiIHk9Ijc5LjIxMDkiPkNyZWF0ZSBXYXQ8L3RleHQ+PHRleHQgZmlsbD0iIzAwMDAwMCIgZm9udC1mYW1pbHk9InNhbnMtc2VyaWYiIGZvbnQtc2l6ZT0iMTIiIGxlbmd0aEFkanVzdD0ic3BhY2luZyIgdGV4dC1kZWNvcmF0aW9uPSJ1bmRlcmxpbmUiIHRleHRMZW5ndGg9IjI3IiB4PSI2IiB5PSI5OS44NDM4Ij5CYWNrPC90ZXh0PjwhLS1NRDU9WzgyZjg1MjZmODVlZTY4MGQ3ZDk3ODVjMTMzZjcwNjAzXQpAc3RhcnR1bWwNCnNhbHQNCnsNCjxzaXplOjE2Pk5ldyBXYXQ8L3NpemU+DQp7DQpOYW1lDQoiICAgICAgICAgICINCn0NCltDcmVhdGUgV2F0XQ0KPHU+QmFjazwvdT4NCn0NCkBlbmR1bWwNCgpQbGFudFVNTCB2ZXJzaW9uIDEuMjAyMi43KE1vbiBBdWcgMjIgMTM6MDE6MzAgRURUIDIwMjIpCihHUEwgc291cmNlIGRpc3RyaWJ1dGlvbikKSmF2YSBSdW50aW1lOiBPcGVuSkRLIFJ1bnRpbWUgRW52aXJvbm1lbnQKSlZNOiBPcGVuSkRLIDY0LUJpdCBTZXJ2ZXIgVk0KRGVmYXVsdCBFbmNvZGluZzogVVRGLTgKTGFuZ3VhZ2U6IGVuCkNvdW50cnk6IFVTCi0tPjwvZz48L3N2Zz4=" y="263"/><rect fill="#F1F1F1" height="127" rx="12.5" ry="12.5" style="stroke:#181818;stroke-width:0.5;" width="121" x="297" y="253"/><image height="108" width="102" x="307" xlink:href="data:image/svg+xml;base64,PHN2ZyBoZWlnaHQ9IjEwOCIgd2lkdGg9IjEwMiIgeG1sbnM6eGxpbms9Imh0dHA6Ly93d3cudzMub3JnLzE5OTkveGxpbmsiIHhtbG5zPSJodHRwOi8vd3d3LnczLm9yZy8yMDAwL3N2ZyIgPjxkZWZzLz48Zz48cmVjdCBmaWxsPSIjRkZGRkZGIiBzdHlsZT0id2lkdGg6MTAycHg7aGVpZ2h0OjEwOHB4O2JhY2tncm91bmQ6I0ZGRkZGRjsiIC8+IDx0ZXh0IGZpbGw9IiMwMDAwMDAiIGZvbnQtZmFtaWx5PSJzYW5zLXNlcmlmIiBmb250LXNpemU9IjE2IiBsZW5ndGhBZGp1c3Q9InNwYWNpbmciIHRleHRMZW5ndGg9Ijg5IiB4PSI2IiB5PSIyMS40Njg4Ij5FZGl0aW5nIFdhdDwvdGV4dD48dGV4dCBmaWxsPSIjMDAwMDAwIiBmb250LWZhbWlseT0ic2Fucy1zZXJpZiIgZm9udC1zaXplPSIxMiIgbGVuZ3RoQWRqdXN0PSJzcGFjaW5nIiB0ZXh0TGVuZ3RoPSIzNCIgeD0iNyIgeT0iMzkuNDQ1MyI+TmFtZTwvdGV4dD48dGV4dCBmaWxsPSIjMDAwMDAwIiBmb250LWZhbWlseT0ic2Fucy1zZXJpZiIgZm9udC1zaXplPSIxMiIgbGVuZ3RoQWRqdXN0PSJzcGFjaW5nIiB0ZXh0TGVuZ3RoPSIyMCIgeD0iMTAiIHk9IjU1LjU3ODEiPndhdDwvdGV4dD48bGluZSBzdHlsZT0ic3Ryb2tlOiMwMDAwMDA7c3Ryb2tlLXdpZHRoOjEuMDsiIHgxPSI4IiB4Mj0iOTEiIHkxPSI1OC4xMDk0IiB5Mj0iNTguMTA5NCIvPjxsaW5lIHN0eWxlPSJzdHJva2U6IzAwMDAwMDtzdHJva2Utd2lkdGg6MS4wOyIgeDE9IjgiIHgyPSI4IiB5MT0iNTUuMTA5NCIgeTI9IjU3LjEwOTQiLz48bGluZSBzdHlsZT0ic3Ryb2tlOiMwMDAwMDA7c3Ryb2tlLXdpZHRoOjEuMDsiIHgxPSI5MSIgeDI9IjkxIiB5MT0iNTUuMTA5NCIgeTI9IjU3LjEwOTQiLz48cmVjdCBmaWxsPSIjRUVFRUVFIiBoZWlnaHQ9IjE4LjEzMjgiIHJ4PSI1IiByeT0iNSIgc3R5bGU9InN0cm9rZTojMDAwMDAwO3N0cm9rZS13aWR0aDoyLjU7IiB3aWR0aD0iODQiIHg9IjguNSIgeT0iNjUuNjA5NCIvPjx0ZXh0IGZpbGw9IiMwMDAwMDAiIGZvbnQtZmFtaWx5PSJzYW5zLXNlcmlmIiBmb250LXNpemU9IjEyIiBsZW5ndGhBZGp1c3Q9InNwYWNpbmciIHRleHRMZW5ndGg9IjY3IiB4PSIxNyIgeT0iNzkuMjEwOSI+VXBkYXRlIFdhdDwvdGV4dD48dGV4dCBmaWxsPSIjMDAwMDAwIiBmb250LWZhbWlseT0ic2Fucy1zZXJpZiIgZm9udC1zaXplPSIxMiIgbGVuZ3RoQWRqdXN0PSJzcGFjaW5nIiB0ZXh0LWRlY29yYXRpb249InVuZGVybGluZSIgdGV4dExlbmd0aD0iMjkiIHg9IjYiIHk9Ijk5Ljg0MzgiPlNob3c8L3RleHQ+PHRleHQgZmlsbD0iIzAwMDAwMCIgZm9udC1mYW1pbHk9InNhbnMtc2VyaWYiIGZvbnQtc2l6ZT0iMTIiIGxlbmd0aEFkanVzdD0ic3BhY2luZyIgdGV4dExlbmd0aD0iNCIgeD0iMzUiIHk9Ijk5Ljg0MzgiPsKgPC90ZXh0Pjx0ZXh0IGZpbGw9IiMwMDAwMDAiIGZvbnQtZmFtaWx5PSJzYW5zLXNlcmlmIiBmb250LXNpemU9IjEyIiBsZW5ndGhBZGp1c3Q9InNwYWNpbmciIHRleHQtZGVjb3JhdGlvbj0idW5kZXJsaW5lIiB0ZXh0TGVuZ3RoPSIyNyIgeD0iMzkiIHk9Ijk5Ljg0MzgiPkJhY2s8L3RleHQ+PCEtLU1ENT1bNDljYTM0NzJhZGY1YTgwOThjMDg1Yzc1YTA3MWIzZDFdCkBzdGFydHVtbA0Kc2FsdA0Kew0KPHNpemU6MTY+RWRpdGluZyBXYXQ8L3NpemU+DQp7DQpOYW1lDQoid2F0ICAgICAgICINCn0NCltVcGRhdGUgV2F0XQ0KPHU+U2hvdzwvdT4gPHU+QmFjazwvdT4NCn0NCkBlbmR1bWwNCgpQbGFudFVNTCB2ZXJzaW9uIDEuMjAyMi43KE1vbiBBdWcgMjIgMTM6MDE6MzAgRURUIDIwMjIpCihHUEwgc291cmNlIGRpc3RyaWJ1dGlvbikKSmF2YSBSdW50aW1lOiBPcGVuSkRLIFJ1bnRpbWUgRW52aXJvbm1lbnQKSlZNOiBPcGVuSkRLIDY0LUJpdCBTZXJ2ZXIgVk0KRGVmYXVsdCBFbmNvZGluZzogVVRGLTgKTGFuZ3VhZ2U6IGVuCkNvdW50cnk6IFVTCi0tPjwvZz48L3N2Zz4=" y="263"/><rect fill="#F1F1F1" height="48.2656" rx="12.5" ry="12.5" style="stroke:#181818;stroke-width:0.5;" width="154" x="361.5" y="109"/><text fill="#000000" font-family="sans-serif" font-size="12" lengthAdjust="spacing" textLength="134" x="371.5" y="130.6016">WatsController#destroy</text><text fill="#000000" font-family="sans-serif" font-size="12" lengthAdjust="spacing" textLength="98" x="371.5" y="144.7344">redirect_to index</text><rect fill="#F1F1F1" height="48.2656" rx="12.5" ry="12.5" style="stroke:#181818;stroke-width:0.5;" width="147" x="7" y="441.5"/><text fill="#000000" font-family="sans-serif" font-size="12" lengthAdjust="spacing" textLength="127" x="17" y="463.1016">WatsController#create</text><text fill="#000000" font-family="sans-serif" font-size="12" lengthAdjust="spacing" textLength="95" x="17" y="477.2344">redirect_to show</text><rect fill="#F1F1F1" height="48.2656" rx="12.5" ry="12.5" style="stroke:#181818;stroke-width:0.5;" width="152" x="347.5" y="441.5"/><text fill="#000000" font-family="sans-serif" font-size="12" lengthAdjust="spacing" textLength="132" x="357.5" y="463.1016">WatsController#update</text><text fill="#000000" font-family="sans-serif" font-size="12" lengthAdjust="spacing" textLength="95" x="357.5" y="477.2344">redirect_to show</text><!--MD5=[552613f025e83fd74f2ded4ec6498215]
link start to index--><g id="link_start_index"><path d="M214.5,26.02 C214.5,34.28 214.5,47.33 214.5,61.49 " fill="none" id="start-to-index" style="stroke:#181818;stroke-width:1.0;"/><polygon fill="#181818" points="214.5,66.75,218.5,57.75,214.5,61.75,210.5,57.75,214.5,66.75" style="stroke:#181818;stroke-width:1.0;"/></g><!--MD5=[d60135a85fe9e8d8fe27515af6f095ad]
link index to show--><g id="link_index_show"><path d="M196.37,199.09 C185.37,249.49 176.58,320.51 194.5,380 C199.84,397.71 210.23,415.13 220.64,429.5 " fill="none" id="index-to-show" style="stroke:#181818;stroke-width:1.0;"/><polygon fill="#181818" points="223.86,433.86,221.7205,424.2463,220.8852,429.8412,215.2904,429.006,223.86,433.86" style="stroke:#181818;stroke-width:1.0;"/><text fill="#000000" font-family="sans-serif" font-size="11" lengthAdjust="spacing" textLength="28" x="195.5" y="321.1348">show</text></g><!--MD5=[4626843acef478dc7e3b7780ac9b5ba1]
reverse link index to show--><g id="link_index_show"><path d="M222.16,204.35 C229.9,275.36 241.48,381.73 247.15,433.7 " fill="none" id="index-backto-show" style="stroke:#181818;stroke-width:1.0;"/><polygon fill="#181818" points="221.61,199.3,218.6106,208.681,222.1528,204.2705,226.5633,207.8126,221.61,199.3" style="stroke:#181818;stroke-width:1.0;"/><text fill="#000000" font-family="sans-serif" font-size="11" lengthAdjust="spacing" textLength="25" x="241.5" y="321.1348">back</text></g><!--MD5=[6f049cd4aa77a477c6904fe8c2e62b7d]
link index to new--><g id="link_index_new"><path d="M130.32,173.77 C113.05,185.86 96.97,200.86 86.5,219 C81.45,227.75 78.45,237.71 76.8,247.84 " fill="none" id="index-to-new" style="stroke:#181818;stroke-width:1.0;"/><polygon fill="#181818" points="76.1,252.82,81.3191,244.4677,76.7991,247.8691,73.3977,243.3491,76.1,252.82" style="stroke:#181818;stroke-width:1.0;"/><text fill="#000000" font-family="sans-serif" font-size="11" lengthAdjust="spacing" textLength="21" x="87.5" y="230.6348">new</text></g><!--MD5=[1c1a96ba1d0e4573be4287ece55c0694]
reverse link index to new--><g id="link_index_new"><path d="M148.84,203.16 C144.54,208.42 140.38,213.73 136.5,219 C128.7,229.61 121.09,241.39 114.12,252.98 " fill="none" id="index-backto-new" style="stroke:#181818;stroke-width:1.0;"/><polygon fill="#181818" points="152.16,199.16,143.327,203.5165,148.9605,203.0023,149.4747,208.6357,152.16,199.16" style="stroke:#181818;stroke-width:1.0;"/><text fill="#000000" font-family="sans-serif" font-size="11" lengthAdjust="spacing" textLength="25" x="137.5" y="230.6348">back</text></g><!--MD5=[d9b189f20c49232791e04e042e54c1d9]
link index to edit--><g id="link_index_edit"><path d="M265.72,199.01 C278.43,215.14 292.07,232.45 304.84,248.67 " fill="none" id="index-to-edit" style="stroke:#181818;stroke-width:1.0;"/><polygon fill="#181818" points="308.07,252.76,305.635,243.2169,304.9727,248.8349,299.3548,248.1726,308.07,252.76" style="stroke:#181818;stroke-width:1.0;"/><text fill="#000000" font-family="sans-serif" font-size="11" lengthAdjust="spacing" textLength="20" x="290.5" y="230.6348">edit</text></g><!--MD5=[481b7ef10163829e27359c2d0eab7dc1]
reverse link index to edit--><g id="link_index_edit"><path d="M302.17,201.91 C307.3,207.42 312.14,213.14 316.5,219 C324.1,229.23 330.61,241.08 336.05,252.9 " fill="none" id="index-backto-edit" style="stroke:#181818;stroke-width:1.0;"/><polygon fill="#181818" points="298.56,198.13,301.853,207.412,302.0015,201.7571,307.6564,201.9056,298.56,198.13" style="stroke:#181818;stroke-width:1.0;"/><text fill="#000000" font-family="sans-serif" font-size="11" lengthAdjust="spacing" textLength="25" x="324.5" y="230.6348">back</text></g><!--MD5=[5c24189927eff948a0d43fee09e5c954]
link index to destroy--><g id="link_index_destroy"><path d="M298.55,133 C317.38,133 337.35,133 356.12,133 " fill="none" id="index-to-destroy" style="stroke:#181818;stroke-width:1.0;"/><polygon fill="#181818" points="361.38,133,352.38,129,356.38,133,352.38,137,361.38,133" style="stroke:#181818;stroke-width:1.0;"/><text fill="#000000" font-family="sans-serif" font-size="11" lengthAdjust="spacing" textLength="41" x="309.5" y="127.6348">destroy</text></g><!--MD5=[297e511babf8be0e6e1e8046a259f254]
reverse link index to destroy--><g id="link_index_destroy"><path d="M303.76,149.86 C317.87,150.99 332.32,151.25 346,150 C351.04,149.54 356.22,148.94 361.42,148.25 " fill="none" id="index-backto-destroy" style="stroke:#181818;stroke-width:1.0;"/><polygon fill="#181818" points="298.58,149.4,307.2032,154.1582,303.5615,149.8294,307.8903,146.1878,298.58,149.4" style="stroke:#181818;stroke-width:1.0;"/><text fill="#000000" font-family="sans-serif" font-size="11" lengthAdjust="spacing" textLength="30" x="315" y="147.6348">index</text></g><!--MD5=[6cddfb4084259a1bd964848322e1f2a6]
link new to create--><g id="link_new_create"><path d="M80.5,380.27 C80.5,399.76 80.5,420.28 80.5,436.18 " fill="none" id="new-to-create" style="stroke:#181818;stroke-width:1.0;"/><polygon fill="#181818" points="80.5,441.39,84.5,432.39,80.5,436.39,76.5,432.39,80.5,441.39" style="stroke:#181818;stroke-width:1.0;"/><text fill="#000000" font-family="sans-serif" font-size="11" lengthAdjust="spacing" textLength="33" x="81.5" y="411.6348">create</text></g><!--MD5=[0b2425b8578287f4e2f0561628404ad9]
link create to show--><g id="link_create_show"><path d="M154.08,465.5 C168.94,465.5 184.28,465.5 198.23,465.5 " fill="none" id="create-to-show" style="stroke:#181818;stroke-width:1.0;"/><polygon fill="#181818" points="203.31,465.5,194.31,461.5,198.31,465.5,194.31,469.5,203.31,465.5" style="stroke:#181818;stroke-width:1.0;"/><text fill="#000000" font-family="sans-serif" font-size="11" lengthAdjust="spacing" textLength="28" x="164.75" y="460.1348">show</text></g><!--MD5=[2e619884978e139b57dc4508f5bda552]
link edit to update--><g id="link_edit_update"><path d="M385.68,380.27 C394.51,399.93 403.81,420.63 410.97,436.6 " fill="none" id="edit-to-update" style="stroke:#181818;stroke-width:1.0;"/><polygon fill="#181818" points="413.13,441.39,413.0985,431.5412,411.0847,436.8275,405.7985,434.8136,413.13,441.39" style="stroke:#181818;stroke-width:1.0;"/><text fill="#000000" font-family="sans-serif" font-size="11" lengthAdjust="spacing" textLength="37" x="400.5" y="411.6348">update</text></g><!--MD5=[6780bdf8607217fa8168538b105faceb]
reverse link show to update--><g id="link_show_update"><path d="M302.87,465.5 C316.88,465.5 332.31,465.5 347.32,465.5 " fill="none" id="show-backto-update" style="stroke:#181818;stroke-width:1.0;"/><polygon fill="#181818" points="297.77,465.5,306.77,469.5,302.77,465.5,306.77,461.5,297.77,465.5" style="stroke:#181818;stroke-width:1.0;"/><text fill="#000000" font-family="sans-serif" font-size="11" lengthAdjust="spacing" textLength="28" x="308.5" y="460.1348">show</text></g><!--MD5=[e1824fd4ea3a73870af0494bca17db3e]
link edit to show--><g id="link_edit_show"><path d="M311.81,380.27 C299.46,397.24 286.53,415 275.75,429.81 " fill="none" id="edit-to-show" style="stroke:#181818;stroke-width:1.0;"/><polygon fill="#181818" points="272.77,433.91,281.3006,428.9876,275.7126,429.8676,274.8326,424.2796,272.77,433.91" style="stroke:#181818;stroke-width:1.0;"/><text fill="#000000" font-family="sans-serif" font-size="11" lengthAdjust="spacing" textLength="28" x="294.5" y="411.6348">show</text></g><!--MD5=[ca84e10651d6f05f1a32fba289aa9d0c]
reverse link edit to show--><g id="link_edit_show"><path d="M344,384.82 C340.07,395.19 335.01,405.33 328.5,414 C320.36,424.84 309.17,434 297.8,441.42 " fill="none" id="edit-backto-show" style="stroke:#181818;stroke-width:1.0;"/><polygon fill="#181818" points="345.73,380.04,338.9073,387.1428,344.0293,384.7419,346.4303,389.8639,345.73,380.04" style="stroke:#181818;stroke-width:1.0;"/><text fill="#000000" font-family="sans-serif" font-size="11" lengthAdjust="spacing" textLength="20" x="337.5" y="411.6348">edit</text></g><!--MD5=[1654364a824132ed299af053ee7da178]
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
<u>Show</u> <u>Back</u>
}
!endprocedure

!procedure _show()
{
{
<b>Name</b>: | wat
}
<u>Edit</u> <u>Back</u>
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

PlantUML version 1.2022.7(Mon Aug 22 13:01:30 EDT 2022)
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

