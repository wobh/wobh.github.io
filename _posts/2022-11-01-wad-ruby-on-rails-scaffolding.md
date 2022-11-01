---
layout: post
title: "Web Application Design: Ruby on Rails scaffolded interfaces"
date: "2022-11-01T08:30-0400"
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
  <svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" contentStyleType="text/css" height="471px" preserveAspectRatio="none" style="width:524px;height:471px;background:#FFFFFF;" version="1.1" viewBox="0 0 524 471" width="524px" zoomAndPan="magnify"><defs/><g><ellipse cx="217.5" cy="16" fill="#222222" rx="10" ry="10" style="stroke:none;stroke-width:1.0;"/><rect fill="#F1F1F1" height="116" rx="12.5" ry="12.5" style="stroke:#181818;stroke-width:0.5;" width="168" x="133.5" y="67"/><image height="97" width="149" x="143.5" xlink:href="data:image/svg+xml;base64,PHN2ZyBoZWlnaHQ9Ijk3IiB3aWR0aD0iMTQ5IiB4bWxuczp4bGluaz0iaHR0cDovL3d3dy53My5vcmcvMTk5OS94bGluayIgeG1sbnM9Imh0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnIiA+PGRlZnMvPjxnPjxyZWN0IGZpbGw9IiNGRkZGRkYiIHN0eWxlPSJ3aWR0aDoxNDlweDtoZWlnaHQ6OTdweDtiYWNrZ3JvdW5kOiNGRkZGRkY7IiAvPiA8dGV4dCBmaWxsPSIjMDAwMDAwIiBmb250LWZhbWlseT0ic2Fucy1zZXJpZiIgZm9udC1zaXplPSIxNiIgbGVuZ3RoQWRqdXN0PSJzcGFjaW5nIiB0ZXh0TGVuZ3RoPSIzNyIgeD0iNiIgeT0iMjEuNDY4OCI+V2F0czwvdGV4dD48dGV4dCBmaWxsPSIjMDAwMDAwIiBmb250LWZhbWlseT0ic2Fucy1zZXJpZiIgZm9udC1zaXplPSIxMiIgZm9udC13ZWlnaHQ9ImJvbGQiIGxlbmd0aEFkanVzdD0ic3BhY2luZyIgdGV4dExlbmd0aD0iMzUiIHg9IjciIHk9IjM5LjQ0NTMiPk5hbWU8L3RleHQ+PHRleHQgZmlsbD0iIzAwMDAwMCIgZm9udC1mYW1pbHk9InNhbnMtc2VyaWYiIGZvbnQtc2l6ZT0iMTIiIGxlbmd0aEFkanVzdD0ic3BhY2luZyIgdGV4dExlbmd0aD0iMjAiIHg9IjciIHk9IjU1LjU3ODEiPndhdDwvdGV4dD48dGV4dCBmaWxsPSIjMDAwMDAwIiBmb250LWZhbWlseT0ic2Fucy1zZXJpZiIgZm9udC1zaXplPSIxMiIgbGVuZ3RoQWRqdXN0PSJzcGFjaW5nIiB0ZXh0LWRlY29yYXRpb249InVuZGVybGluZSIgdGV4dExlbmd0aD0iMjkiIHg9IjQ0IiB5PSI1NS41NzgxIj5zaG93PC90ZXh0Pjx0ZXh0IGZpbGw9IiMwMDAwMDAiIGZvbnQtZmFtaWx5PSJzYW5zLXNlcmlmIiBmb250LXNpemU9IjEyIiBsZW5ndGhBZGp1c3Q9InNwYWNpbmciIHRleHQtZGVjb3JhdGlvbj0idW5kZXJsaW5lIiB0ZXh0TGVuZ3RoPSIyMiIgeD0iNzUiIHk9IjU1LjU3ODEiPmVkaXQ8L3RleHQ+PHRleHQgZmlsbD0iIzAwMDAwMCIgZm9udC1mYW1pbHk9InNhbnMtc2VyaWYiIGZvbnQtc2l6ZT0iMTIiIGxlbmd0aEFkanVzdD0ic3BhY2luZyIgdGV4dC1kZWNvcmF0aW9uPSJ1bmRlcmxpbmUiIHRleHRMZW5ndGg9IjQzIiB4PSI5OSIgeT0iNTUuNTc4MSI+ZGVzdHJveTwvdGV4dD48dGV4dCBmaWxsPSIjMDAwMDAwIiBmb250LWZhbWlseT0ic2Fucy1zZXJpZiIgZm9udC1zaXplPSIxMiIgbGVuZ3RoQWRqdXN0PSJzcGFjaW5nIiB0ZXh0TGVuZ3RoPSIyNyIgeD0iNyIgeT0iNzEuNzEwOSI+d2FhdDwvdGV4dD48dGV4dCBmaWxsPSIjMDAwMDAwIiBmb250LWZhbWlseT0ic2Fucy1zZXJpZiIgZm9udC1zaXplPSIxMiIgbGVuZ3RoQWRqdXN0PSJzcGFjaW5nIiB0ZXh0LWRlY29yYXRpb249InVuZGVybGluZSIgdGV4dExlbmd0aD0iMjkiIHg9IjQ0IiB5PSI3MS43MTA5Ij5zaG93PC90ZXh0Pjx0ZXh0IGZpbGw9IiMwMDAwMDAiIGZvbnQtZmFtaWx5PSJzYW5zLXNlcmlmIiBmb250LXNpemU9IjEyIiBsZW5ndGhBZGp1c3Q9InNwYWNpbmciIHRleHQtZGVjb3JhdGlvbj0idW5kZXJsaW5lIiB0ZXh0TGVuZ3RoPSIyMiIgeD0iNzUiIHk9IjcxLjcxMDkiPmVkaXQ8L3RleHQ+PHRleHQgZmlsbD0iIzAwMDAwMCIgZm9udC1mYW1pbHk9InNhbnMtc2VyaWYiIGZvbnQtc2l6ZT0iMTIiIGxlbmd0aEFkanVzdD0ic3BhY2luZyIgdGV4dC1kZWNvcmF0aW9uPSJ1bmRlcmxpbmUiIHRleHRMZW5ndGg9IjQzIiB4PSI5OSIgeT0iNzEuNzEwOSI+ZGVzdHJveTwvdGV4dD48dGV4dCBmaWxsPSIjMDAwMDAwIiBmb250LWZhbWlseT0ic2Fucy1zZXJpZiIgZm9udC1zaXplPSIxMiIgbGVuZ3RoQWRqdXN0PSJzcGFjaW5nIiB0ZXh0LWRlY29yYXRpb249InVuZGVybGluZSIgdGV4dExlbmd0aD0iNTAiIHg9IjYiIHk9Ijg4Ljg0MzgiPk5ldyBXYXQ8L3RleHQ+PCEtLU1ENT1bNTZkMDFiZTRmYWFmYjA0NDI2YjNiZGExMTc2MjJmMDhdCkBzdGFydHVtbA0Kc2FsdA0Kew0KPHNpemU6MTY+V2F0czwvc2l6ZT4NCnsNCjxiPk5hbWUgfCAuIHwgLiB8IC4NCndhdCB8IDx1PnNob3c8L3U+IHwgPHU+ZWRpdDwvdT4gfCA8dT5kZXN0cm95PC91Pg0Kd2FhdCB8IDx1PnNob3c8L3U+IHwgPHU+ZWRpdDwvdT4gfCA8dT5kZXN0cm95PC91Pg0KfQ0KPHU+TmV3IFdhdDwvdT4NCn0NCkBlbmR1bWwNCgpQbGFudFVNTCB2ZXJzaW9uIDEuMjAyMi43KE1vbiBBdWcgMjIgMTM6MDE6MzAgRURUIDIwMjIpCihHUEwgc291cmNlIGRpc3RyaWJ1dGlvbikKSmF2YSBSdW50aW1lOiBPcGVuSkRLIFJ1bnRpbWUgRW52aXJvbm1lbnQKSlZNOiBPcGVuSkRLIDY0LUJpdCBTZXJ2ZXIgVk0KRGVmYXVsdCBFbmNvZGluZzogVVRGLTgKTGFuZ3VhZ2U6IGVuCkNvdW50cnk6IFVTCi0tPjwvZz48L3N2Zz4=" y="77"/><rect fill="#F1F1F1" height="63" rx="12.5" ry="12.5" style="stroke:#181818;stroke-width:0.5;" width="94" x="203.5" y="402"/><image height="44" width="75" x="213.5" xlink:href="data:image/svg+xml;base64,PHN2ZyBoZWlnaHQ9IjQ0IiB3aWR0aD0iNzUiIHhtbG5zOnhsaW5rPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5L3hsaW5rIiB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciID48ZGVmcy8+PGc+PHJlY3QgZmlsbD0iI0ZGRkZGRiIgc3R5bGU9IndpZHRoOjc1cHg7aGVpZ2h0OjQ0cHg7YmFja2dyb3VuZDojRkZGRkZGOyIgLz4gPHRleHQgZmlsbD0iIzAwMDAwMCIgZm9udC1mYW1pbHk9InNhbnMtc2VyaWYiIGZvbnQtc2l6ZT0iMTIiIGZvbnQtd2VpZ2h0PSJib2xkIiBsZW5ndGhBZGp1c3Q9InNwYWNpbmciIHRleHRMZW5ndGg9IjM1IiB4PSI3IiB5PSIxOC42MDE2Ij5OYW1lPC90ZXh0Pjx0ZXh0IGZpbGw9IiMwMDAwMDAiIGZvbnQtZmFtaWx5PSJzYW5zLXNlcmlmIiBmb250LXNpemU9IjEyIiBsZW5ndGhBZGp1c3Q9InNwYWNpbmciIHRleHRMZW5ndGg9IjQiIHg9IjQyIiB5PSIxOC42MDE2Ij46PC90ZXh0Pjx0ZXh0IGZpbGw9IiMwMDAwMDAiIGZvbnQtZmFtaWx5PSJzYW5zLXNlcmlmIiBmb250LXNpemU9IjEyIiBsZW5ndGhBZGp1c3Q9InNwYWNpbmciIHRleHRMZW5ndGg9IjIwIiB4PSI0OCIgeT0iMTguNjAxNiI+d2F0PC90ZXh0Pjx0ZXh0IGZpbGw9IiMwMDAwMDAiIGZvbnQtZmFtaWx5PSJzYW5zLXNlcmlmIiBmb250LXNpemU9IjEyIiBsZW5ndGhBZGp1c3Q9InNwYWNpbmciIHRleHQtZGVjb3JhdGlvbj0idW5kZXJsaW5lIiB0ZXh0TGVuZ3RoPSIyMiIgeD0iNiIgeT0iMzUuNzM0NCI+RWRpdDwvdGV4dD48dGV4dCBmaWxsPSIjMDAwMDAwIiBmb250LWZhbWlseT0ic2Fucy1zZXJpZiIgZm9udC1zaXplPSIxMiIgbGVuZ3RoQWRqdXN0PSJzcGFjaW5nIiB0ZXh0TGVuZ3RoPSI0IiB4PSIyOCIgeT0iMzUuNzM0NCI+wqA8L3RleHQ+PHRleHQgZmlsbD0iIzAwMDAwMCIgZm9udC1mYW1pbHk9InNhbnMtc2VyaWYiIGZvbnQtc2l6ZT0iMTIiIGxlbmd0aEFkanVzdD0ic3BhY2luZyIgdGV4dC1kZWNvcmF0aW9uPSJ1bmRlcmxpbmUiIHRleHRMZW5ndGg9IjI3IiB4PSIzMiIgeT0iMzUuNzM0NCI+QmFjazwvdGV4dD48IS0tTUQ1PVsxZmUxYzVjMWEzMThhOGY4ZTkyOWVhOTdiMzBhMGRhNF0KQHN0YXJ0dW1sDQpzYWx0DQp7DQp7DQo8Yj5OYW1lPC9iPjogfCB3YXQNCn0NCjx1PkVkaXQ8L3U+IDx1PkJhY2s8L3U+DQp9DQpAZW5kdW1sDQoKUGxhbnRVTUwgdmVyc2lvbiAxLjIwMjIuNyhNb24gQXVnIDIyIDEzOjAxOjMwIEVEVCAyMDIyKQooR1BMIHNvdXJjZSBkaXN0cmlidXRpb24pCkphdmEgUnVudGltZTogT3BlbkpESyBSdW50aW1lIEVudmlyb25tZW50CkpWTTogT3BlbkpESyA2NC1CaXQgU2VydmVyIFZNCkRlZmF1bHQgRW5jb2Rpbmc6IFVURi04Ckxhbmd1YWdlOiBlbgpDb3VudHJ5OiBVUwotLT48L2c+PC9zdmc+" y="412"/><rect fill="#F1F1F1" height="111" rx="12.5" ry="12.5" style="stroke:#181818;stroke-width:0.5;" width="123" x="19" y="237"/><image height="92" width="104" x="29" xlink:href="data:image/svg+xml;base64,PHN2ZyBoZWlnaHQ9IjkyIiB3aWR0aD0iMTA0IiB4bWxuczp4bGluaz0iaHR0cDovL3d3dy53My5vcmcvMTk5OS94bGluayIgeG1sbnM9Imh0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnIiA+PGRlZnMvPjxnPjxyZWN0IGZpbGw9IiNGRkZGRkYiIHN0eWxlPSJ3aWR0aDoxMDRweDtoZWlnaHQ6OTJweDtiYWNrZ3JvdW5kOiNGRkZGRkY7IiAvPiA8dGV4dCBmaWxsPSIjMDAwMDAwIiBmb250LWZhbWlseT0ic2Fucy1zZXJpZiIgZm9udC1zaXplPSIxNiIgbGVuZ3RoQWRqdXN0PSJzcGFjaW5nIiB0ZXh0TGVuZ3RoPSI2NyIgeD0iNiIgeT0iMjEuNDY4OCI+TmV3IFdhdDwvdGV4dD48dGV4dCBmaWxsPSIjMDAwMDAwIiBmb250LWZhbWlseT0ic2Fucy1zZXJpZiIgZm9udC1zaXplPSIxMiIgbGVuZ3RoQWRqdXN0PSJzcGFjaW5nIiB0ZXh0TGVuZ3RoPSIzNiIgeD0iNyIgeT0iMzkuNDQ1MyI+bmFtZTo8L3RleHQ+PHRleHQgZmlsbD0iIzAwMDAwMCIgZm9udC1mYW1pbHk9InNhbnMtc2VyaWYiIGZvbnQtc2l6ZT0iMTIiIGxlbmd0aEFkanVzdD0ic3BhY2luZyIgdGV4dExlbmd0aD0iNCIgeD0iNDgiIHk9IjM5LjQ0NTMiPsKgPC90ZXh0PjxsaW5lIHN0eWxlPSJzdHJva2U6IzAwMDAwMDtzdHJva2Utd2lkdGg6MS4wOyIgeDE9IjQ2IiB4Mj0iOTciIHkxPSI0MS45NzY2IiB5Mj0iNDEuOTc2NiIvPjxsaW5lIHN0eWxlPSJzdHJva2U6IzAwMDAwMDtzdHJva2Utd2lkdGg6MS4wOyIgeDE9IjQ2IiB4Mj0iNDYiIHkxPSIzOC45NzY2IiB5Mj0iNDAuOTc2NiIvPjxsaW5lIHN0eWxlPSJzdHJva2U6IzAwMDAwMDtzdHJva2Utd2lkdGg6MS4wOyIgeDE9Ijk3IiB4Mj0iOTciIHkxPSIzOC45NzY2IiB5Mj0iNDAuOTc2NiIvPjxyZWN0IGZpbGw9IiNFRUVFRUUiIGhlaWdodD0iMTguMTMyOCIgcng9IjUiIHJ5PSI1IiBzdHlsZT0ic3Ryb2tlOiMwMDAwMDA7c3Ryb2tlLXdpZHRoOjIuNTsiIHdpZHRoPSI4NCIgeD0iOC41IiB5PSI0OS40NzY2Ii8+PHRleHQgZmlsbD0iIzAwMDAwMCIgZm9udC1mYW1pbHk9InNhbnMtc2VyaWYiIGZvbnQtc2l6ZT0iMTIiIGxlbmd0aEFkanVzdD0ic3BhY2luZyIgdGV4dExlbmd0aD0iNjMiIHg9IjE5IiB5PSI2My4wNzgxIj5DcmVhdGUgV2F0PC90ZXh0Pjx0ZXh0IGZpbGw9IiMwMDAwMDAiIGZvbnQtZmFtaWx5PSJzYW5zLXNlcmlmIiBmb250LXNpemU9IjEyIiBsZW5ndGhBZGp1c3Q9InNwYWNpbmciIHRleHQtZGVjb3JhdGlvbj0idW5kZXJsaW5lIiB0ZXh0TGVuZ3RoPSIyNyIgeD0iNiIgeT0iODMuNzEwOSI+QmFjazwvdGV4dD48IS0tTUQ1PVs1ZTY2ODg2OWUxMWQ5MGUwNWZhNmM3MzQzZTAwZDQwNl0KQHN0YXJ0dW1sDQpzYWx0DQp7DQo8c2l6ZToxNj5OZXcgV2F0PC9zaXplPg0Kew0KbmFtZTogfCAiICAgICAgIg0KfQ0KW0NyZWF0ZSBXYXRdDQo8dT5CYWNrPC91Pg0KfQ0KQGVuZHVtbA0KClBsYW50VU1MIHZlcnNpb24gMS4yMDIyLjcoTW9uIEF1ZyAyMiAxMzowMTozMCBFRFQgMjAyMikKKEdQTCBzb3VyY2UgZGlzdHJpYnV0aW9uKQpKYXZhIFJ1bnRpbWU6IE9wZW5KREsgUnVudGltZSBFbnZpcm9ubWVudApKVk06IE9wZW5KREsgNjQtQml0IFNlcnZlciBWTQpEZWZhdWx0IEVuY29kaW5nOiBVVEYtOApMYW5ndWFnZTogZW4KQ291bnRyeTogVVMKLS0+PC9nPjwvc3ZnPg==" y="247"/><rect fill="#F1F1F1" height="111" rx="12.5" ry="12.5" style="stroke:#181818;stroke-width:0.5;" width="125" x="300" y="237"/><image height="92" width="106" x="310" xlink:href="data:image/svg+xml;base64,PHN2ZyBoZWlnaHQ9IjkyIiB3aWR0aD0iMTA2IiB4bWxuczp4bGluaz0iaHR0cDovL3d3dy53My5vcmcvMTk5OS94bGluayIgeG1sbnM9Imh0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnIiA+PGRlZnMvPjxnPjxyZWN0IGZpbGw9IiNGRkZGRkYiIHN0eWxlPSJ3aWR0aDoxMDZweDtoZWlnaHQ6OTJweDtiYWNrZ3JvdW5kOiNGRkZGRkY7IiAvPiA8dGV4dCBmaWxsPSIjMDAwMDAwIiBmb250LWZhbWlseT0ic2Fucy1zZXJpZiIgZm9udC1zaXplPSIxNiIgbGVuZ3RoQWRqdXN0PSJzcGFjaW5nIiB0ZXh0TGVuZ3RoPSI4OSIgeD0iNiIgeT0iMjEuNDY4OCI+RWRpdGluZyBXYXQ8L3RleHQ+PHRleHQgZmlsbD0iIzAwMDAwMCIgZm9udC1mYW1pbHk9InNhbnMtc2VyaWYiIGZvbnQtc2l6ZT0iMTIiIGxlbmd0aEFkanVzdD0ic3BhY2luZyIgdGV4dExlbmd0aD0iMzgiIHg9IjciIHk9IjM5LjQ0NTMiPk5hbWU6PC90ZXh0Pjx0ZXh0IGZpbGw9IiMwMDAwMDAiIGZvbnQtZmFtaWx5PSJzYW5zLXNlcmlmIiBmb250LXNpemU9IjEyIiBsZW5ndGhBZGp1c3Q9InNwYWNpbmciIHRleHRMZW5ndGg9IjQiIHg9IjUwIiB5PSIzOS40NDUzIj7CoDwvdGV4dD48bGluZSBzdHlsZT0ic3Ryb2tlOiMwMDAwMDA7c3Ryb2tlLXdpZHRoOjEuMDsiIHgxPSI0OCIgeDI9Ijk5IiB5MT0iNDEuOTc2NiIgeTI9IjQxLjk3NjYiLz48bGluZSBzdHlsZT0ic3Ryb2tlOiMwMDAwMDA7c3Ryb2tlLXdpZHRoOjEuMDsiIHgxPSI0OCIgeDI9IjQ4IiB5MT0iMzguOTc2NiIgeTI9IjQwLjk3NjYiLz48bGluZSBzdHlsZT0ic3Ryb2tlOiMwMDAwMDA7c3Ryb2tlLXdpZHRoOjEuMDsiIHgxPSI5OSIgeDI9Ijk5IiB5MT0iMzguOTc2NiIgeTI9IjQwLjk3NjYiLz48cmVjdCBmaWxsPSIjRUVFRUVFIiBoZWlnaHQ9IjE4LjEzMjgiIHJ4PSI1IiByeT0iNSIgc3R5bGU9InN0cm9rZTojMDAwMDAwO3N0cm9rZS13aWR0aDoyLjU7IiB3aWR0aD0iODQiIHg9IjguNSIgeT0iNDkuNDc2NiIvPjx0ZXh0IGZpbGw9IiMwMDAwMDAiIGZvbnQtZmFtaWx5PSJzYW5zLXNlcmlmIiBmb250LXNpemU9IjEyIiBsZW5ndGhBZGp1c3Q9InNwYWNpbmciIHRleHRMZW5ndGg9IjY3IiB4PSIxNyIgeT0iNjMuMDc4MSI+VXBkYXRlIFdhdDwvdGV4dD48dGV4dCBmaWxsPSIjMDAwMDAwIiBmb250LWZhbWlseT0ic2Fucy1zZXJpZiIgZm9udC1zaXplPSIxMiIgbGVuZ3RoQWRqdXN0PSJzcGFjaW5nIiB0ZXh0LWRlY29yYXRpb249InVuZGVybGluZSIgdGV4dExlbmd0aD0iMjkiIHg9IjYiIHk9IjgzLjcxMDkiPlNob3c8L3RleHQ+PHRleHQgZmlsbD0iIzAwMDAwMCIgZm9udC1mYW1pbHk9InNhbnMtc2VyaWYiIGZvbnQtc2l6ZT0iMTIiIGxlbmd0aEFkanVzdD0ic3BhY2luZyIgdGV4dExlbmd0aD0iNCIgeD0iMzUiIHk9IjgzLjcxMDkiPsKgPC90ZXh0Pjx0ZXh0IGZpbGw9IiMwMDAwMDAiIGZvbnQtZmFtaWx5PSJzYW5zLXNlcmlmIiBmb250LXNpemU9IjEyIiBsZW5ndGhBZGp1c3Q9InNwYWNpbmciIHRleHQtZGVjb3JhdGlvbj0idW5kZXJsaW5lIiB0ZXh0TGVuZ3RoPSIyNyIgeD0iMzkiIHk9IjgzLjcxMDkiPkJhY2s8L3RleHQ+PCEtLU1ENT1bMjY4ZGMyNTZhNzVhZjcyMWVmMjcyZTA2ZjJhY2FhN2VdCkBzdGFydHVtbA0Kc2FsdA0Kew0KPHNpemU6MTY+RWRpdGluZyBXYXQ8L3NpemU+DQp7DQpOYW1lOiB8ICIgICAgICAiDQp9DQpbVXBkYXRlIFdhdF0NCjx1PlNob3c8L3U+IDx1PkJhY2s8L3U+DQp9DQpAZW5kdW1sDQoKUGxhbnRVTUwgdmVyc2lvbiAxLjIwMjIuNyhNb24gQXVnIDIyIDEzOjAxOjMwIEVEVCAyMDIyKQooR1BMIHNvdXJjZSBkaXN0cmlidXRpb24pCkphdmEgUnVudGltZTogT3BlbkpESyBSdW50aW1lIEVudmlyb25tZW50CkpWTTogT3BlbkpESyA2NC1CaXQgU2VydmVyIFZNCkRlZmF1bHQgRW5jb2Rpbmc6IFVURi04Ckxhbmd1YWdlOiBlbgpDb3VudHJ5OiBVUwotLT48L2c+PC9zdmc+" y="247"/><rect fill="#F1F1F1" height="48.2656" rx="12.5" ry="12.5" style="stroke:#181818;stroke-width:0.5;" width="154" x="364.5" y="101"/><text fill="#000000" font-family="sans-serif" font-size="12" lengthAdjust="spacing" textLength="134" x="374.5" y="122.6016">WatsController#destroy</text><text fill="#000000" font-family="sans-serif" font-size="12" lengthAdjust="spacing" textLength="98" x="374.5" y="136.7344">redirect_to index</text><rect fill="#F1F1F1" height="48.2656" rx="12.5" ry="12.5" style="stroke:#181818;stroke-width:0.5;" width="147" x="7" y="409.5"/><text fill="#000000" font-family="sans-serif" font-size="12" lengthAdjust="spacing" textLength="127" x="17" y="431.1016">WatsController#create</text><text fill="#000000" font-family="sans-serif" font-size="12" lengthAdjust="spacing" textLength="95" x="17" y="445.2344">redirect_to show</text><rect fill="#F1F1F1" height="48.2656" rx="12.5" ry="12.5" style="stroke:#181818;stroke-width:0.5;" width="152" x="347.5" y="409.5"/><text fill="#000000" font-family="sans-serif" font-size="12" lengthAdjust="spacing" textLength="132" x="357.5" y="431.1016">WatsController#update</text><text fill="#000000" font-family="sans-serif" font-size="12" lengthAdjust="spacing" textLength="95" x="357.5" y="445.2344">redirect_to show</text><!--MD5=[552613f025e83fd74f2ded4ec6498215]
link start to index--><g id="link_start_index"><path d="M217.5,26.12 C217.5,34.49 217.5,47.7 217.5,61.72 " fill="none" id="start-to-index" style="stroke:#181818;stroke-width:1.0;"/><polygon fill="#181818" points="217.5,66.92,221.5,57.92,217.5,61.92,213.5,57.92,217.5,66.92" style="stroke:#181818;stroke-width:1.0;"/></g><!--MD5=[d60135a85fe9e8d8fe27515af6f095ad]
link index to show--><g id="link_index_show"><path d="M201.05,183.2 C190.64,228.69 181.83,293.52 197.5,348 C202.53,365.5 212.37,382.87 222.22,397.27 " fill="none" id="index-to-show" style="stroke:#181818;stroke-width:1.0;"/><polygon fill="#181818" points="225.27,401.65,223.3835,391.9835,222.4017,397.5545,216.8307,396.5727,225.27,401.65" style="stroke:#181818;stroke-width:1.0;"/><text fill="#000000" font-family="sans-serif" font-size="11" lengthAdjust="spacing" textLength="28" x="198.5" y="297.1348">show</text></g><!--MD5=[4626843acef478dc7e3b7780ac9b5ba1]
reverse link index to show--><g id="link_index_show"><path d="M224.21,188.29 C231.18,253.1 241.83,351.94 247.18,401.66 " fill="none" id="index-backto-show" style="stroke:#181818;stroke-width:1.0;"/><polygon fill="#181818" points="223.67,183.29,220.6706,192.671,224.2128,188.2605,228.6233,191.8026,223.67,183.29" style="stroke:#181818;stroke-width:1.0;"/><text fill="#000000" font-family="sans-serif" font-size="11" lengthAdjust="spacing" textLength="25" x="241.5" y="297.1348">back</text></g><!--MD5=[6f049cd4aa77a477c6904fe8c2e62b7d]
link index to new--><g id="link_index_new"><path d="M133.17,160.72 C115.75,171.73 99.4,185.67 88.5,203 C83.07,211.64 79.91,221.73 78.21,231.94 " fill="none" id="index-to-new" style="stroke:#181818;stroke-width:1.0;"/><polygon fill="#181818" points="77.49,236.95,82.7361,228.6146,78.2051,232.0014,74.8183,227.4704,77.49,236.95" style="stroke:#181818;stroke-width:1.0;"/><text fill="#000000" font-family="sans-serif" font-size="11" lengthAdjust="spacing" textLength="21" x="89.5" y="214.6348">new</text></g><!--MD5=[1c1a96ba1d0e4573be4287ece55c0694]
reverse link index to new--><g id="link_index_new"><path d="M153.58,186.91 C148.68,192.23 143.93,197.63 139.5,203 C130.8,213.54 122.2,225.38 114.37,236.89 " fill="none" id="index-backto-new" style="stroke:#181818;stroke-width:1.0;"/><polygon fill="#181818" points="156.99,183.24,147.94,187.1255,153.5927,186.9085,153.8096,192.5612,156.99,183.24" style="stroke:#181818;stroke-width:1.0;"/><text fill="#000000" font-family="sans-serif" font-size="11" lengthAdjust="spacing" textLength="25" x="140.5" y="214.6348">back</text></g><!--MD5=[d9b189f20c49232791e04e042e54c1d9]
link index to edit--><g id="link_index_edit"><path d="M267.81,183.42 C281.83,199.43 297.08,216.84 311.17,232.91 " fill="none" id="index-to-edit" style="stroke:#181818;stroke-width:1.0;"/><polygon fill="#181818" points="314.72,236.97,311.7861,227.5683,311.4204,233.2133,305.7754,232.8476,314.72,236.97" style="stroke:#181818;stroke-width:1.0;"/><text fill="#000000" font-family="sans-serif" font-size="11" lengthAdjust="spacing" textLength="20" x="294.5" y="214.6348">edit</text></g><!--MD5=[481b7ef10163829e27359c2d0eab7dc1]
reverse link index to edit--><g id="link_index_edit"><path d="M305.43,186.75 C310.81,191.96 315.9,197.39 320.5,203 C328.69,213 335.72,224.86 341.55,236.6 " fill="none" id="index-backto-edit" style="stroke:#181818;stroke-width:1.0;"/><polygon fill="#181818" points="301.66,183.19,305.4535,192.279,305.2939,186.6244,310.9485,186.4648,301.66,183.19" style="stroke:#181818;stroke-width:1.0;"/><text fill="#000000" font-family="sans-serif" font-size="11" lengthAdjust="spacing" textLength="25" x="330.5" y="214.6348">back</text></g><!--MD5=[5c24189927eff948a0d43fee09e5c954]
link index to destroy--><g id="link_index_destroy"><path d="M301.55,125 C320.38,125 340.35,125 359.12,125 " fill="none" id="index-to-destroy" style="stroke:#181818;stroke-width:1.0;"/><polygon fill="#181818" points="364.38,125,355.38,121,359.38,125,355.38,129,364.38,125" style="stroke:#181818;stroke-width:1.0;"/><text fill="#000000" font-family="sans-serif" font-size="11" lengthAdjust="spacing" textLength="41" x="312.5" y="119.6348">destroy</text></g><!--MD5=[297e511babf8be0e6e1e8046a259f254]
reverse link index to destroy--><g id="link_index_destroy"><path d="M306.76,141.86 C320.87,142.99 335.32,143.25 349,142 C354.04,141.54 359.22,140.94 364.42,140.25 " fill="none" id="index-backto-destroy" style="stroke:#181818;stroke-width:1.0;"/><polygon fill="#181818" points="301.58,141.4,310.2032,146.1582,306.5615,141.8294,310.8903,138.1878,301.58,141.4" style="stroke:#181818;stroke-width:1.0;"/><text fill="#000000" font-family="sans-serif" font-size="11" lengthAdjust="spacing" textLength="30" x="318" y="139.6348">index</text></g><!--MD5=[6cddfb4084259a1bd964848322e1f2a6]
link new to create--><g id="link_new_create"><path d="M80.5,348.04 C80.5,367.17 80.5,387.89 80.5,404.02 " fill="none" id="new-to-create" style="stroke:#181818;stroke-width:1.0;"/><polygon fill="#181818" points="80.5,409.31,84.5,400.31,80.5,404.31,76.5,400.31,80.5,409.31" style="stroke:#181818;stroke-width:1.0;"/><text fill="#000000" font-family="sans-serif" font-size="11" lengthAdjust="spacing" textLength="33" x="81.5" y="379.6348">create</text></g><!--MD5=[0b2425b8578287f4e2f0561628404ad9]
link create to show--><g id="link_create_show"><path d="M154.08,433.5 C168.94,433.5 184.28,433.5 198.23,433.5 " fill="none" id="create-to-show" style="stroke:#181818;stroke-width:1.0;"/><polygon fill="#181818" points="203.31,433.5,194.31,429.5,198.31,433.5,194.31,437.5,203.31,433.5" style="stroke:#181818;stroke-width:1.0;"/><text fill="#000000" font-family="sans-serif" font-size="11" lengthAdjust="spacing" textLength="28" x="164.75" y="428.1348">show</text></g><!--MD5=[2e619884978e139b57dc4508f5bda552]
link edit to update--><g id="link_edit_update"><path d="M386.43,348.04 C394.9,367.34 404.08,388.25 411.19,404.44 " fill="none" id="edit-to-update" style="stroke:#181818;stroke-width:1.0;"/><polygon fill="#181818" points="413.32,409.31,413.3885,399.4614,411.3212,404.7269,406.0556,402.6595,413.32,409.31" style="stroke:#181818;stroke-width:1.0;"/><text fill="#000000" font-family="sans-serif" font-size="11" lengthAdjust="spacing" textLength="37" x="400.5" y="379.6348">update</text></g><!--MD5=[6780bdf8607217fa8168538b105faceb]
reverse link show to update--><g id="link_show_update"><path d="M302.87,433.5 C316.88,433.5 332.31,433.5 347.32,433.5 " fill="none" id="show-backto-update" style="stroke:#181818;stroke-width:1.0;"/><polygon fill="#181818" points="297.77,433.5,306.77,437.5,302.77,433.5,306.77,429.5,297.77,433.5" style="stroke:#181818;stroke-width:1.0;"/><text fill="#000000" font-family="sans-serif" font-size="11" lengthAdjust="spacing" textLength="28" x="308.5" y="428.1348">show</text></g><!--MD5=[ca84e10651d6f05f1a32fba289aa9d0c]
reverse link edit to show--><g id="link_edit_show"><path d="M315.09,352.34 C301.39,369.35 286.98,387.22 275.26,401.77 " fill="none" id="edit-backto-show" style="stroke:#181818;stroke-width:1.0;"/><polygon fill="#181818" points="318.56,348.04,309.7901,352.5221,315.4157,351.9276,316.0102,357.5531,318.56,348.04" style="stroke:#181818;stroke-width:1.0;"/><text fill="#000000" font-family="sans-serif" font-size="11" lengthAdjust="spacing" textLength="20" x="300.5" y="379.6348">edit</text></g><!--MD5=[6ae721267255bd81045c917adc265713]
@startuml
!unquoted procedure SALT($x)
"{ {
salt
%invoke_procedure("_"+$x)
} }" as $x
!endprocedure

!procedure _index()
{
<size:16>Wats</size>
{
<b>Name | . | . | .
wat | <u>show</u> | <u>edit</u> | <u>destroy</u>
waat | <u>show</u> | <u>edit</u> | <u>destroy</u>
}
<u>New Wat</u>
}
!endprocedure

!procedure _edit()
{
<size:16>Editing Wat</size>
{
Name: | "      "
}
[Update Wat]
<u>Show</u> <u>Back</u>
}
!endprocedure

!procedure _new()
{
<size:16>New Wat</size>
{
name: | "      "
}
[Create Wat]
<u>Back</u>
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

(*) - -> SALT(index)

index - -> [show] SALT(show)
index - -> [new] SALT(new)
index - -> [edit] SALT(edit)

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
build new features, fix old bugs on many things built this way. I'm
hoping the table and diagrams will give the mind some things to
pattern match on and ask questions about.

Here's some thoughts and questions I've had to puzzle over:

1.  Starting from the index the flow has two short paths into the
    "update" cycle. What alternative paths could be made? What ways are
    there out of the update cycle?
2.  Over time, the `wats` table fills up with entries. How can we setup
    searching and sorting features? What about aggregates and archives?
3.  If we need more specialized interfaces and actions, to support
    complex processes, how can we find "natural" ways of extending the
    structure we find here?

I'm thinking about starting with some simple examples, starting with
adding a feature to copy `wats` entries. I don't know if I want to try
to come up with an example application I can share. I'd like to derive
design principles that aren't Rails-specific, even though, Rails is
the framework I'll probably use for the examples.

