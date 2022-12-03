---
layout: post
title: "Web Application Design: Ruby on Rails scaffolded interfaces v6 vs v7"
date: "2022-11-03T18:45-0400"
tags: 
  - web-application-design
  - ruby-on-rails
---

I learned a funny thing while building a test application for my next
posts, which is that Rails changed their scaffolding templates in 2.7.

I don't expect it to change my overall plans very much, but here's a
little tangent on the changes.

<!-- more -->

Here's the activity diagram from the last post representing the
scaffolding in Rails 6.

<figure class="centered">
  <figcaption>Rails 6 Scaffolding</figcaption>
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

And here's what it has been changed to:

<figure class="centered">
  <figcaption>Rails 7 Scaffolding</figcaption>
  <svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" contentStyleType="text/css" height="563px" preserveAspectRatio="none" style="width:655px;height:563px;background:#FFFFFF;" version="1.1" viewBox="0 0 655 563" width="655px" zoomAndPan="magnify"><defs/><g><ellipse cx="308.5" cy="16" fill="#222222" rx="10" ry="10" style="stroke:none;stroke-width:1.0;"/><rect fill="#F1F1F1" height="170" rx="12.5" ry="12.5" style="stroke:#181818;stroke-width:0.5;" width="110" x="253.5" y="67"/><image height="151" width="91" x="263.5" xlink:href="data:image/svg+xml;base64,PHN2ZyBoZWlnaHQ9IjE1MSIgd2lkdGg9IjkxIiB4bWxuczp4bGluaz0iaHR0cDovL3d3dy53My5vcmcvMTk5OS94bGluayIgeG1sbnM9Imh0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnIiA+PGRlZnMvPjxnPjxyZWN0IGZpbGw9IiNGRkZGRkYiIHN0eWxlPSJ3aWR0aDo5MXB4O2hlaWdodDoxNTFweDtiYWNrZ3JvdW5kOiNGRkZGRkY7IiAvPiA8dGV4dCBmaWxsPSIjMDAwMDAwIiBmb250LWZhbWlseT0ic2Fucy1zZXJpZiIgZm9udC1zaXplPSIxNiIgbGVuZ3RoQWRqdXN0PSJzcGFjaW5nIiB0ZXh0TGVuZ3RoPSIzNyIgeD0iNiIgeT0iMjEuNDY4OCI+V2F0czwvdGV4dD48dGV4dCBmaWxsPSIjMDAwMDAwIiBmb250LWZhbWlseT0ic2Fucy1zZXJpZiIgZm9udC1zaXplPSIxMiIgZm9udC13ZWlnaHQ9ImJvbGQiIGxlbmd0aEFkanVzdD0ic3BhY2luZyIgdGV4dExlbmd0aD0iMzgiIHg9IjgiIHk9IjQwLjQ0NTMiPk5hbWU6PC90ZXh0Pjx0ZXh0IGZpbGw9IiMwMDAwMDAiIGZvbnQtZmFtaWx5PSJzYW5zLXNlcmlmIiBmb250LXNpemU9IjEyIiBsZW5ndGhBZGp1c3Q9InNwYWNpbmciIHRleHRMZW5ndGg9IjIwIiB4PSI0OCIgeT0iNDAuNDQ1MyI+d2F0PC90ZXh0Pjx0ZXh0IGZpbGw9IiMwMDAwMDAiIGZvbnQtZmFtaWx5PSJzYW5zLXNlcmlmIiBmb250LXNpemU9IjEyIiBsZW5ndGhBZGp1c3Q9InNwYWNpbmciIHRleHQtZGVjb3JhdGlvbj0idW5kZXJsaW5lIiB0ZXh0TGVuZ3RoPSI3NyIgeD0iNyIgeT0iNTcuNTc4MSI+U2hvdyB0aGlzIHdhdDwvdGV4dD48dGV4dCBmaWxsPSIjMDAwMDAwIiBmb250LWZhbWlseT0ic2Fucy1zZXJpZiIgZm9udC1zaXplPSIxMiIgZm9udC13ZWlnaHQ9ImJvbGQiIGxlbmd0aEFkanVzdD0ic3BhY2luZyIgdGV4dExlbmd0aD0iMzgiIHg9IjgiIHk9Ijc0LjcxMDkiPk5hbWU6PC90ZXh0Pjx0ZXh0IGZpbGw9IiMwMDAwMDAiIGZvbnQtZmFtaWx5PSJzYW5zLXNlcmlmIiBmb250LXNpemU9IjEyIiBsZW5ndGhBZGp1c3Q9InNwYWNpbmciIHRleHRMZW5ndGg9IjI3IiB4PSI0OCIgeT0iNzQuNzEwOSI+d2FhdDwvdGV4dD48dGV4dCBmaWxsPSIjMDAwMDAwIiBmb250LWZhbWlseT0ic2Fucy1zZXJpZiIgZm9udC1zaXplPSIxMiIgbGVuZ3RoQWRqdXN0PSJzcGFjaW5nIiB0ZXh0LWRlY29yYXRpb249InVuZGVybGluZSIgdGV4dExlbmd0aD0iNzciIHg9IjciIHk9IjkxLjg0MzgiPlNob3cgdGhpcyB3YXQ8L3RleHQ+PHRleHQgZmlsbD0iIzAwMDAwMCIgZm9udC1mYW1pbHk9InNhbnMtc2VyaWYiIGZvbnQtc2l6ZT0iMTIiIGZvbnQtd2VpZ2h0PSJib2xkIiBsZW5ndGhBZGp1c3Q9InNwYWNpbmciIHRleHRMZW5ndGg9IjM4IiB4PSI4IiB5PSIxMDguOTc2NiI+TmFtZTo8L3RleHQ+PHRleHQgZmlsbD0iIzAwMDAwMCIgZm9udC1mYW1pbHk9InNhbnMtc2VyaWYiIGZvbnQtc2l6ZT0iMTIiIGxlbmd0aEFkanVzdD0ic3BhY2luZyIgdGV4dExlbmd0aD0iMzQiIHg9IjQ4IiB5PSIxMDguOTc2NiI+d2FhYXQ8L3RleHQ+PHRleHQgZmlsbD0iIzAwMDAwMCIgZm9udC1mYW1pbHk9InNhbnMtc2VyaWYiIGZvbnQtc2l6ZT0iMTIiIGxlbmd0aEFkanVzdD0ic3BhY2luZyIgdGV4dC1kZWNvcmF0aW9uPSJ1bmRlcmxpbmUiIHRleHRMZW5ndGg9Ijc3IiB4PSI3IiB5PSIxMjYuMTA5NCI+U2hvdyB0aGlzIHdhdDwvdGV4dD48dGV4dCBmaWxsPSIjMDAwMDAwIiBmb250LWZhbWlseT0ic2Fucy1zZXJpZiIgZm9udC1zaXplPSIxMiIgbGVuZ3RoQWRqdXN0PSJzcGFjaW5nIiB0ZXh0LWRlY29yYXRpb249InVuZGVybGluZSIgdGV4dExlbmd0aD0iNTAiIHg9IjYiIHk9IjE0My4yNDIyIj5OZXcgV2F0PC90ZXh0PjwhLS1NRDU9W2EwZDk2ZTc3Y2NiNjYwYjZjOWI1YmI2MjhkNzY1OWYwXQpAc3RhcnR1bWwNCnNhbHQNCnsNCjxzaXplOjE2PldhdHM8L3NpemU+DQp7DQp7DQo8Yj5OYW1lOjwvYj4gfCB3YXQNCn0NCjx1PlNob3cgdGhpcyB3YXQ8L3U+DQp7DQo8Yj5OYW1lOjwvYj4gfCB3YWF0DQp9DQo8dT5TaG93IHRoaXMgd2F0PC91Pg0Kew0KPGI+TmFtZTo8L2I+IHwgd2FhYXQNCn0NCjx1PlNob3cgdGhpcyB3YXQ8L3U+DQp9DQo8dT5OZXcgV2F0PC91Pg0KfQ0KQGVuZHVtbA0KClBsYW50VU1MIHZlcnNpb24gMS4yMDIyLjEyKFN1biBPY3QgMjMgMTQ6MTI6MjYgRURUIDIwMjIpCihHUEwgc291cmNlIGRpc3RyaWJ1dGlvbikKSmF2YSBSdW50aW1lOiBPcGVuSkRLIFJ1bnRpbWUgRW52aXJvbm1lbnQKSlZNOiBPcGVuSkRLIDY0LUJpdCBTZXJ2ZXIgVk0KRGVmYXVsdCBFbmNvZGluZzogVVRGLTgKTGFuZ3VhZ2U6IGVuCkNvdW50cnk6IFVTCi0tPjwvZz48L3N2Zz4=" y="77"/><rect fill="#F1F1F1" height="85" rx="12.5" ry="12.5" style="stroke:#181818;stroke-width:0.5;" width="186" x="245.5" y="472"/><image height="66" width="167" x="255.5" xlink:href="data:image/svg+xml;base64,PHN2ZyBoZWlnaHQ9IjY2IiB3aWR0aD0iMTY3IiB4bWxuczp4bGluaz0iaHR0cDovL3d3dy53My5vcmcvMTk5OS94bGluayIgeG1sbnM9Imh0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnIiA+PGRlZnMvPjxnPjxyZWN0IGZpbGw9IiNGRkZGRkYiIHN0eWxlPSJ3aWR0aDoxNjdweDtoZWlnaHQ6NjZweDtiYWNrZ3JvdW5kOiNGRkZGRkY7IiAvPiA8dGV4dCBmaWxsPSIjMDAwMDAwIiBmb250LWZhbWlseT0ic2Fucy1zZXJpZiIgZm9udC1zaXplPSIxMiIgZm9udC13ZWlnaHQ9ImJvbGQiIGxlbmd0aEFkanVzdD0ic3BhY2luZyIgdGV4dExlbmd0aD0iMzgiIHg9IjciIHk9IjE4LjYwMTYiPk5hbWU6PC90ZXh0Pjx0ZXh0IGZpbGw9IiMwMDAwMDAiIGZvbnQtZmFtaWx5PSJzYW5zLXNlcmlmIiBmb250LXNpemU9IjEyIiBsZW5ndGhBZGp1c3Q9InNwYWNpbmciIHRleHRMZW5ndGg9IjIwIiB4PSI0NyIgeT0iMTguNjAxNiI+d2F0PC90ZXh0Pjx0ZXh0IGZpbGw9IiMwMDAwMDAiIGZvbnQtZmFtaWx5PSJzYW5zLXNlcmlmIiBmb250LXNpemU9IjEyIiBsZW5ndGhBZGp1c3Q9InNwYWNpbmciIHRleHQtZGVjb3JhdGlvbj0idW5kZXJsaW5lIiB0ZXh0TGVuZ3RoPSI3MCIgeD0iNiIgeT0iMzUuNzM0NCI+RWRpdCB0aGlzIHdhdDwvdGV4dD48dGV4dCBmaWxsPSIjMDAwMDAwIiBmb250LWZhbWlseT0ic2Fucy1zZXJpZiIgZm9udC1zaXplPSIxMiIgbGVuZ3RoQWRqdXN0PSJzcGFjaW5nIiB0ZXh0TGVuZ3RoPSI0IiB4PSI4MCIgeT0iMzUuNzM0NCI+fDwvdGV4dD48dGV4dCBmaWxsPSIjMDAwMDAwIiBmb250LWZhbWlseT0ic2Fucy1zZXJpZiIgZm9udC1zaXplPSIxMiIgbGVuZ3RoQWRqdXN0PSJzcGFjaW5nIiB0ZXh0LWRlY29yYXRpb249InVuZGVybGluZSIgdGV4dExlbmd0aD0iNzIiIHg9Ijg4IiB5PSIzNS43MzQ0Ij5CYWNrIHRvIHdhdHM8L3RleHQ+PHJlY3QgZmlsbD0iI0VFRUVFRSIgaGVpZ2h0PSIxOC4xMzI4IiByeD0iNSIgcnk9IjUiIHN0eWxlPSJzdHJva2U6IzAwMDAwMDtzdHJva2Utd2lkdGg6Mi41OyIgd2lkdGg9IjEzMiIgeD0iOC41IiB5PSI0Mi43NjU2Ii8+PHRleHQgZmlsbD0iIzAwMDAwMCIgZm9udC1mYW1pbHk9InNhbnMtc2VyaWYiIGZvbnQtc2l6ZT0iMTIiIGxlbmd0aEFkanVzdD0ic3BhY2luZyIgdGV4dExlbmd0aD0iOTIiIHg9IjI4LjUiIHk9IjU2LjM2NzIiPkRlc3Ryb3kgdGhpcyB3YXQ8L3RleHQ+PCEtLU1ENT1bNTU3YzNmMDY2MmVjMzcxNTE0ZTE0YTlhMmY2YjczOGNdCkBzdGFydHVtbA0Kc2FsdA0Kew0Kew0KPGI+TmFtZTo8L2I+IHwgd2F0DQp9DQo8dT5FZGl0IHRoaXMgd2F0PC91PiA8VSswMDdjPiA8dT5CYWNrIHRvIHdhdHM8L3U+DQpbRGVzdHJveSB0aGlzIHdhdF0NCn0NCkBlbmR1bWwNCgpQbGFudFVNTCB2ZXJzaW9uIDEuMjAyMi4xMihTdW4gT2N0IDIzIDE0OjEyOjI2IEVEVCAyMDIyKQooR1BMIHNvdXJjZSBkaXN0cmlidXRpb24pCkphdmEgUnVudGltZTogT3BlbkpESyBSdW50aW1lIEVudmlyb25tZW50CkpWTTogT3BlbkpESyA2NC1CaXQgU2VydmVyIFZNCkRlZmF1bHQgRW5jb2Rpbmc6IFVURi04Ckxhbmd1YWdlOiBlbgpDb3VudHJ5OiBVUwotLT48L2c+PC9zdmc+" y="482"/><rect fill="#F1F1F1" height="127" rx="12.5" ry="12.5" style="stroke:#181818;stroke-width:0.5;" width="117" x="7" y="291"/><image height="108" width="98" x="17" xlink:href="data:image/svg+xml;base64,PHN2ZyBoZWlnaHQ9IjEwOCIgd2lkdGg9Ijk4IiB4bWxuczp4bGluaz0iaHR0cDovL3d3dy53My5vcmcvMTk5OS94bGluayIgeG1sbnM9Imh0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnIiA+PGRlZnMvPjxnPjxyZWN0IGZpbGw9IiNGRkZGRkYiIHN0eWxlPSJ3aWR0aDo5OHB4O2hlaWdodDoxMDhweDtiYWNrZ3JvdW5kOiNGRkZGRkY7IiAvPiA8dGV4dCBmaWxsPSIjMDAwMDAwIiBmb250LWZhbWlseT0ic2Fucy1zZXJpZiIgZm9udC1zaXplPSIxNiIgbGVuZ3RoQWRqdXN0PSJzcGFjaW5nIiB0ZXh0TGVuZ3RoPSI2NyIgeD0iNiIgeT0iMjEuNDY4OCI+TmV3IFdhdDwvdGV4dD48dGV4dCBmaWxsPSIjMDAwMDAwIiBmb250LWZhbWlseT0ic2Fucy1zZXJpZiIgZm9udC1zaXplPSIxMiIgbGVuZ3RoQWRqdXN0PSJzcGFjaW5nIiB0ZXh0TGVuZ3RoPSIzNCIgeD0iNyIgeT0iMzkuNDQ1MyI+TmFtZTwvdGV4dD48dGV4dCBmaWxsPSIjMDAwMDAwIiBmb250LWZhbWlseT0ic2Fucy1zZXJpZiIgZm9udC1zaXplPSIxMiIgbGVuZ3RoQWRqdXN0PSJzcGFjaW5nIiB0ZXh0TGVuZ3RoPSI0IiB4PSIxMCIgeT0iNTUuNTc4MSI+wqA8L3RleHQ+PGxpbmUgc3R5bGU9InN0cm9rZTojMDAwMDAwO3N0cm9rZS13aWR0aDoxLjA7IiB4MT0iOCIgeDI9IjkxIiB5MT0iNTguMTA5NCIgeTI9IjU4LjEwOTQiLz48bGluZSBzdHlsZT0ic3Ryb2tlOiMwMDAwMDA7c3Ryb2tlLXdpZHRoOjEuMDsiIHgxPSI4IiB4Mj0iOCIgeTE9IjU1LjEwOTQiIHkyPSI1Ny4xMDk0Ii8+PGxpbmUgc3R5bGU9InN0cm9rZTojMDAwMDAwO3N0cm9rZS13aWR0aDoxLjA7IiB4MT0iOTEiIHgyPSI5MSIgeTE9IjU1LjEwOTQiIHkyPSI1Ny4xMDk0Ii8+PHJlY3QgZmlsbD0iI0VFRUVFRSIgaGVpZ2h0PSIxOC4xMzI4IiByeD0iNSIgcnk9IjUiIHN0eWxlPSJzdHJva2U6IzAwMDAwMDtzdHJva2Utd2lkdGg6Mi41OyIgd2lkdGg9Ijg0IiB4PSI4LjUiIHk9IjY1LjYwOTQiLz48dGV4dCBmaWxsPSIjMDAwMDAwIiBmb250LWZhbWlseT0ic2Fucy1zZXJpZiIgZm9udC1zaXplPSIxMiIgbGVuZ3RoQWRqdXN0PSJzcGFjaW5nIiB0ZXh0TGVuZ3RoPSI2MyIgeD0iMTkiIHk9Ijc5LjIxMDkiPkNyZWF0ZSBXYXQ8L3RleHQ+PHRleHQgZmlsbD0iIzAwMDAwMCIgZm9udC1mYW1pbHk9InNhbnMtc2VyaWYiIGZvbnQtc2l6ZT0iMTIiIGxlbmd0aEFkanVzdD0ic3BhY2luZyIgdGV4dC1kZWNvcmF0aW9uPSJ1bmRlcmxpbmUiIHRleHRMZW5ndGg9IjcyIiB4PSI2IiB5PSI5OS44NDM4Ij5CYWNrIHRvIHdhdHM8L3RleHQ+PCEtLU1ENT1bMDRmMWU1MTkxMjU3NjEzOTZlY2UzZjQzOTAxYmQxM2VdCkBzdGFydHVtbA0Kc2FsdA0Kew0KPHNpemU6MTY+TmV3IFdhdDwvc2l6ZT4NCnsNCk5hbWUNCiIgICAgICAgICAgIg0KfQ0KW0NyZWF0ZSBXYXRdDQo8dT5CYWNrIHRvIHdhdHM8L3U+DQp9DQpAZW5kdW1sDQoKUGxhbnRVTUwgdmVyc2lvbiAxLjIwMjIuMTIoU3VuIE9jdCAyMyAxNDoxMjoyNiBFRFQgMjAyMikKKEdQTCBzb3VyY2UgZGlzdHJpYnV0aW9uKQpKYXZhIFJ1bnRpbWU6IE9wZW5KREsgUnVudGltZSBFbnZpcm9ubWVudApKVk06IE9wZW5KREsgNjQtQml0IFNlcnZlciBWTQpEZWZhdWx0IEVuY29kaW5nOiBVVEYtOApMYW5ndWFnZTogZW4KQ291bnRyeTogVVMKLS0+PC9nPjwvc3ZnPg==" y="301"/><rect fill="#F1F1F1" height="127" rx="12.5" ry="12.5" style="stroke:#181818;stroke-width:0.5;" width="193" x="456" y="291"/><image height="108" width="174" x="466" xlink:href="data:image/svg+xml;base64,PHN2ZyBoZWlnaHQ9IjEwOCIgd2lkdGg9IjE3NCIgeG1sbnM6eGxpbms9Imh0dHA6Ly93d3cudzMub3JnLzE5OTkveGxpbmsiIHhtbG5zPSJodHRwOi8vd3d3LnczLm9yZy8yMDAwL3N2ZyIgPjxkZWZzLz48Zz48cmVjdCBmaWxsPSIjRkZGRkZGIiBzdHlsZT0id2lkdGg6MTc0cHg7aGVpZ2h0OjEwOHB4O2JhY2tncm91bmQ6I0ZGRkZGRjsiIC8+IDx0ZXh0IGZpbGw9IiMwMDAwMDAiIGZvbnQtZmFtaWx5PSJzYW5zLXNlcmlmIiBmb250LXNpemU9IjE2IiBsZW5ndGhBZGp1c3Q9InNwYWNpbmciIHRleHRMZW5ndGg9Ijg5IiB4PSI2IiB5PSIyMS40Njg4Ij5FZGl0aW5nIFdhdDwvdGV4dD48dGV4dCBmaWxsPSIjMDAwMDAwIiBmb250LWZhbWlseT0ic2Fucy1zZXJpZiIgZm9udC1zaXplPSIxMiIgbGVuZ3RoQWRqdXN0PSJzcGFjaW5nIiB0ZXh0TGVuZ3RoPSIzNCIgeD0iNyIgeT0iMzkuNDQ1MyI+TmFtZTwvdGV4dD48dGV4dCBmaWxsPSIjMDAwMDAwIiBmb250LWZhbWlseT0ic2Fucy1zZXJpZiIgZm9udC1zaXplPSIxMiIgbGVuZ3RoQWRqdXN0PSJzcGFjaW5nIiB0ZXh0TGVuZ3RoPSIyMCIgeD0iMTAiIHk9IjU1LjU3ODEiPndhdDwvdGV4dD48bGluZSBzdHlsZT0ic3Ryb2tlOiMwMDAwMDA7c3Ryb2tlLXdpZHRoOjEuMDsiIHgxPSI4IiB4Mj0iOTEiIHkxPSI1OC4xMDk0IiB5Mj0iNTguMTA5NCIvPjxsaW5lIHN0eWxlPSJzdHJva2U6IzAwMDAwMDtzdHJva2Utd2lkdGg6MS4wOyIgeDE9IjgiIHgyPSI4IiB5MT0iNTUuMTA5NCIgeTI9IjU3LjEwOTQiLz48bGluZSBzdHlsZT0ic3Ryb2tlOiMwMDAwMDA7c3Ryb2tlLXdpZHRoOjEuMDsiIHgxPSI5MSIgeDI9IjkxIiB5MT0iNTUuMTA5NCIgeTI9IjU3LjEwOTQiLz48cmVjdCBmaWxsPSIjRUVFRUVFIiBoZWlnaHQ9IjE4LjEzMjgiIHJ4PSI1IiByeT0iNSIgc3R5bGU9InN0cm9rZTojMDAwMDAwO3N0cm9rZS13aWR0aDoyLjU7IiB3aWR0aD0iODQiIHg9IjguNSIgeT0iNjUuNjA5NCIvPjx0ZXh0IGZpbGw9IiMwMDAwMDAiIGZvbnQtZmFtaWx5PSJzYW5zLXNlcmlmIiBmb250LXNpemU9IjEyIiBsZW5ndGhBZGp1c3Q9InNwYWNpbmciIHRleHRMZW5ndGg9IjY3IiB4PSIxNyIgeT0iNzkuMjEwOSI+VXBkYXRlIFdhdDwvdGV4dD48dGV4dCBmaWxsPSIjMDAwMDAwIiBmb250LWZhbWlseT0ic2Fucy1zZXJpZiIgZm9udC1zaXplPSIxMiIgbGVuZ3RoQWRqdXN0PSJzcGFjaW5nIiB0ZXh0LWRlY29yYXRpb249InVuZGVybGluZSIgdGV4dExlbmd0aD0iNzciIHg9IjYiIHk9Ijk5Ljg0MzgiPlNob3cgdGhpcyB3YXQ8L3RleHQ+PHRleHQgZmlsbD0iIzAwMDAwMCIgZm9udC1mYW1pbHk9InNhbnMtc2VyaWYiIGZvbnQtc2l6ZT0iMTIiIGxlbmd0aEFkanVzdD0ic3BhY2luZyIgdGV4dExlbmd0aD0iNCIgeD0iODciIHk9Ijk5Ljg0MzgiPnw8L3RleHQ+PHRleHQgZmlsbD0iIzAwMDAwMCIgZm9udC1mYW1pbHk9InNhbnMtc2VyaWYiIGZvbnQtc2l6ZT0iMTIiIGxlbmd0aEFkanVzdD0ic3BhY2luZyIgdGV4dC1kZWNvcmF0aW9uPSJ1bmRlcmxpbmUiIHRleHRMZW5ndGg9IjcyIiB4PSI5NSIgeT0iOTkuODQzOCI+QmFjayB0byB3YXRzPC90ZXh0PjwhLS1NRDU9W2MxMGU4YTQ3OWYzNDNhNTVjZDcwMDgyNGUwZTljMWE0XQpAc3RhcnR1bWwNCnNhbHQNCnsNCjxzaXplOjE2PkVkaXRpbmcgV2F0PC9zaXplPg0Kew0KTmFtZQ0KIndhdCAgICAgICAiDQp9DQpbVXBkYXRlIFdhdF0NCjx1PlNob3cgdGhpcyB3YXQ8L3U+IDxVKzAwN2M+IDx1PkJhY2sgdG8gd2F0czwvdT4NCn0NCkBlbmR1bWwNCgpQbGFudFVNTCB2ZXJzaW9uIDEuMjAyMi4xMihTdW4gT2N0IDIzIDE0OjEyOjI2IEVEVCAyMDIyKQooR1BMIHNvdXJjZSBkaXN0cmlidXRpb24pCkphdmEgUnVudGltZTogT3BlbkpESyBSdW50aW1lIEVudmlyb25tZW50CkpWTTogT3BlbkpESyA2NC1CaXQgU2VydmVyIFZNCkRlZmF1bHQgRW5jb2Rpbmc6IFVURi04Ckxhbmd1YWdlOiBlbgpDb3VudHJ5OiBVUwotLT48L2c+PC9zdmc+" y="301"/><rect fill="#F1F1F1" height="48.2656" rx="12.5" ry="12.5" style="stroke:#181818;stroke-width:0.5;" width="154" x="144.5" y="330.5"/><text fill="#000000" font-family="sans-serif" font-size="12" lengthAdjust="spacing" textLength="134" x="154.5" y="352.1016">WatsController#destroy</text><text fill="#000000" font-family="sans-serif" font-size="12" lengthAdjust="spacing" textLength="98" x="154.5" y="366.2344">redirect_to index</text><rect fill="#F1F1F1" height="48.2656" rx="12.5" ry="12.5" style="stroke:#181818;stroke-width:0.5;" width="147" x="35" y="490.5"/><text fill="#000000" font-family="sans-serif" font-size="12" lengthAdjust="spacing" textLength="127" x="45" y="512.1016">WatsController#create</text><text fill="#000000" font-family="sans-serif" font-size="12" lengthAdjust="spacing" textLength="95" x="45" y="526.2344">redirect_to show</text><rect fill="#F1F1F1" height="48.2656" rx="12.5" ry="12.5" style="stroke:#181818;stroke-width:0.5;" width="152" x="481.5" y="490.5"/><text fill="#000000" font-family="sans-serif" font-size="12" lengthAdjust="spacing" textLength="132" x="491.5" y="512.1016">WatsController#update</text><text fill="#000000" font-family="sans-serif" font-size="12" lengthAdjust="spacing" textLength="95" x="491.5" y="526.2344">redirect_to show</text><!--MD5=[552613f025e83fd74f2ded4ec6498215]
link start to index--><g id="link_start_index"><path d="M308.5,26.21 C308.5,34.11 308.5,46.37 308.5,60.19 " fill="none" id="start-to-index" style="stroke:#181818;stroke-width:1.0;"/><polygon fill="#181818" points="308.5,65.13,312.5,56.13,308.5,60.13,304.5,56.13,308.5,65.13" style="stroke:#181818;stroke-width:1.0;"/></g><!--MD5=[d60135a85fe9e8d8fe27515af6f095ad]
link index to show--><g id="link_index_show"><path d="M315.5,237.07 C321.41,308.13 329.67,407.4 334.52,465.62 " fill="none" id="index-to-show" style="stroke:#181818;stroke-width:1.0;"/><polygon fill="#181818" points="334.91,470.31,338.1488,461.0089,334.4948,465.3273,330.1764,461.6733,334.91,470.31" style="stroke:#181818;stroke-width:1.0;"/><text fill="#000000" font-family="sans-serif" font-size="11" lengthAdjust="spacing" textLength="28" x="331.5" y="359.1348">show</text></g><!--MD5=[4626843acef478dc7e3b7780ac9b5ba1]
reverse link index to show--><g id="link_index_show"><path d="M351.48,242.89 C357.29,258.6 362.33,275.04 365.5,291 C376.48,346.37 373.25,362.09 365.5,418 C363.02,435.92 358.03,455.21 353.01,471.72 " fill="none" id="index-backto-show" style="stroke:#181818;stroke-width:1.0;"/><polygon fill="#181818" points="349.85,238.55,349.2648,248.3815,351.6056,243.2316,356.7554,245.5725,349.85,238.55" style="stroke:#181818;stroke-width:1.0;"/><text fill="#000000" font-family="sans-serif" font-size="11" lengthAdjust="spacing" textLength="25" x="374.5" y="359.1348">back</text></g><!--MD5=[6f049cd4aa77a477c6904fe8c2e62b7d]
link index to new--><g id="link_index_new"><path d="M253.26,179.24 C217.55,197.84 171.41,225.11 136.5,257 C127.2,265.5 118.32,275.38 110.19,285.51 " fill="none" id="index-to-new" style="stroke:#181818;stroke-width:1.0;"/><polygon fill="#181818" points="107.19,289.31,115.9052,284.7226,110.2873,285.3849,109.625,279.7669,107.19,289.31" style="stroke:#181818;stroke-width:1.0;"/><text fill="#000000" font-family="sans-serif" font-size="11" lengthAdjust="spacing" textLength="21" x="137.5" y="268.6348">new</text></g><!--MD5=[3f3c1aab8cad22d4c1fc4ac26cf0d295]
link new to index--><g id="link_new_index"><path d="M124.32,299.65 C127.74,296.7 131.15,293.8 134.5,291 C145.69,281.65 149.02,279.99 160.5,271 C189.48,248.3 221.44,222.87 248.22,201.45 " fill="none" id="new-to-index" style="stroke:#181818;stroke-width:1.0;"/><polygon fill="#181818" points="251.98,198.44,242.4568,200.9515,248.0798,201.5687,247.4627,207.1918,251.98,198.44" style="stroke:#181818;stroke-width:1.0;"/><text fill="#000000" font-family="sans-serif" font-size="11" lengthAdjust="spacing" textLength="25" x="178.5" y="268.6348">back</text></g><!--MD5=[ca84e10651d6f05f1a32fba289aa9d0c]
reverse link edit to show--><g id="link_edit_show"><path d="M510.63,423.33 C502.2,433.8 492.74,443.83 482.5,452 C467.41,464.04 449.62,474.13 431.78,482.43 " fill="none" id="edit-backto-show" style="stroke:#181818;stroke-width:1.0;"/><polygon fill="#181818" points="513.5,419.68,504.8032,424.3023,510.4185,423.6175,511.1032,429.2328,513.5,419.68" style="stroke:#181818;stroke-width:1.0;"/><text fill="#000000" font-family="sans-serif" font-size="11" lengthAdjust="spacing" textLength="20" x="498.5" y="449.6348">edit</text></g><!--MD5=[e1824fd4ea3a73870af0494bca17db3e]
link edit to show--><g id="link_edit_show"><path d="M469.76,418.36 C454.75,429.63 439.19,441.21 424.5,452 C417.41,457.21 409.96,462.62 402.53,467.98 " fill="none" id="edit-to-show" style="stroke:#181818;stroke-width:1.0;"/><polygon fill="#181818" points="398.62,470.8,408.2633,468.7981,402.6809,467.883,403.5961,462.3007,398.62,470.8" style="stroke:#181818;stroke-width:1.0;"/><text fill="#000000" font-family="sans-serif" font-size="11" lengthAdjust="spacing" textLength="28" x="443.5" y="449.6348">show</text></g><!--MD5=[2b6315f86e256fb7902e9ab239fdb209]
reverse link destroy to show--><g id="link_destroy_show"><path d="M236.58,384.3 C247.46,404.07 263.07,430.52 279.5,452 C284.53,458.58 290.19,465.23 295.95,471.59 " fill="none" id="destroy-backto-show" style="stroke:#181818;stroke-width:1.0;"/><polygon fill="#181818" points="234.23,379.98,235.0035,389.7984,236.6133,384.3755,242.0362,385.9852,234.23,379.98" style="stroke:#181818;stroke-width:1.0;"/><text fill="#000000" font-family="sans-serif" font-size="11" lengthAdjust="spacing" textLength="41" x="280.5" y="449.6348">destroy</text></g><!--MD5=[297e511babf8be0e6e1e8046a259f254]
reverse link index to destroy--><g id="link_index_destroy"><path d="M269.27,243.42 C255.36,275.47 240.86,308.88 231.54,330.37 " fill="none" id="index-backto-destroy" style="stroke:#181818;stroke-width:1.0;"/><polygon fill="#181818" points="271.25,238.84,264.0031,245.5095,269.2632,243.4283,271.3443,248.6884,271.25,238.84" style="stroke:#181818;stroke-width:1.0;"/><text fill="#000000" font-family="sans-serif" font-size="11" lengthAdjust="spacing" textLength="30" x="263.5" y="268.6348">index</text></g><!--MD5=[6cddfb4084259a1bd964848322e1f2a6]
link new to create--><g id="link_new_create"><path d="M82.62,418.4 C88.8,441.13 95.5,465.72 100.53,484.2 " fill="none" id="new-to-create" style="stroke:#181818;stroke-width:1.0;"/><polygon fill="#181818" points="101.77,488.76,103.2855,479.0284,100.4665,483.9329,95.5621,481.114,101.77,488.76" style="stroke:#181818;stroke-width:1.0;"/><text fill="#000000" font-family="sans-serif" font-size="11" lengthAdjust="spacing" textLength="33" x="92.5" y="449.6348">create</text></g><!--MD5=[0b2425b8578287f4e2f0561628404ad9]
link create to show--><g id="link_create_show"><path d="M182.22,514.5 C200.28,514.5 219.91,514.5 238.96,514.5 " fill="none" id="create-to-show" style="stroke:#181818;stroke-width:1.0;"/><polygon fill="#181818" points="243.89,514.5,234.89,510.5,238.89,514.5,234.89,518.5,243.89,514.5" style="stroke:#181818;stroke-width:1.0;"/><text fill="#000000" font-family="sans-serif" font-size="11" lengthAdjust="spacing" textLength="28" x="199.75" y="509.1348">show</text></g><!--MD5=[2e619884978e139b57dc4508f5bda552]
link edit to update--><g id="link_edit_update"><path d="M554.49,418.4 C555.2,440.93 555.97,465.29 556.56,483.72 " fill="none" id="edit-to-update" style="stroke:#181818;stroke-width:1.0;"/><polygon fill="#181818" points="556.72,488.71,560.4353,479.5888,556.5629,483.7125,552.4393,479.8401,556.72,488.71" style="stroke:#181818;stroke-width:1.0;"/><text fill="#000000" font-family="sans-serif" font-size="11" lengthAdjust="spacing" textLength="37" x="555.5" y="449.6348">update</text></g><!--MD5=[6780bdf8607217fa8168538b105faceb]
reverse link show to update--><g id="link_show_update"><path d="M437.91,514.5 C452.47,514.5 467.26,514.5 481.26,514.5 " fill="none" id="show-backto-update" style="stroke:#181818;stroke-width:1.0;"/><polygon fill="#181818" points="433.21,514.5,442.21,518.5,438.21,514.5,442.21,510.5,433.21,514.5" style="stroke:#181818;stroke-width:1.0;"/><text fill="#000000" font-family="sans-serif" font-size="11" lengthAdjust="spacing" textLength="28" x="442.5" y="509.1348">show</text></g><!--MD5=[481b7ef10163829e27359c2d0eab7dc1]
reverse link index to edit--><g id="link_index_edit"><path d="M368.59,202.38 C401.04,229.04 441.42,262.23 476.08,290.71 " fill="none" id="index-backto-edit" style="stroke:#181818;stroke-width:1.0;"/><polygon fill="#181818" points="365.04,199.46,369.4514,208.2657,368.9021,202.6355,374.5323,202.0862,365.04,199.46" style="stroke:#181818;stroke-width:1.0;"/><text fill="#000000" font-family="sans-serif" font-size="11" lengthAdjust="spacing" textLength="25" x="446.5" y="268.6348">back</text></g><!--MD5=[6081006f9b5b9e65d83fbe51a0e7e747]
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

!unquoted procedure MODEL($field)
{
<b>Name:</b> | $field
}
!endprocedure

!procedure _index()
{
<size:16>Wats</size>
{
MODEL(wat)
<u>Show this wat</u>
MODEL(waat)
<u>Show this wat</u>
MODEL(waaat)
<u>Show this wat</u>
}
<u>New Wat</u>
}
!endprocedure

!procedure _new()
{
<size:16>New Wat</size>
FORM("Create", '"          "')
<u>Back to wats</u>
}
!endprocedure

!procedure _edit()
{
<size:16>Editing Wat</size>
FORM("Update", '"wat       "')
<u>Show this wat</u> <U+007c> <u>Back to wats</u>
}
!endprocedure

!procedure _show()
{
MODEL(wat)
<u>Edit this wat</u> <U+007c> <u>Back to wats</u>
[Destroy this wat]
}
!endprocedure

(*) - -> VIEW(index)

index - -> [show] VIEW(show)
index - -> [new] VIEW(new)
show -up-> [edit] VIEW(edit)

show -up-> [destroy] "WatsController#destroy
redirect_to index" as destroy
destroy -up-> [index] index
show -up-> [back] index

new - -> [create] "WatsController#create
redirect_to show" as create
create -right-> [show] show
new - -> [back] index

edit - -> [update] "WatsController#update
redirect_to show" as update
update -left-> [show] show
edit - -> [show] show
edit -up-> [back] index
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

We can see that most of the links involving the resource have been
moved to the `show` view, and `index` just has links to `show` (for
each resource listed in the index) and `new`. This makes sense. It's
still a good base to start from. I plan on exploring the
implementation of "rich" views in which the features for updating and
creating are incorporated into `index` and `show`, but not having any
intimations of that in the base provided from the scaffolding isn't
going to complicate the exploration of that any.

In Rails 6 (and before) `index` represented all it's resources in a
table, which was a different representation from `show`. In Rails 7
`index` and `show` render the representation in the same model-partial
which resembles the Rails 6 `show` representation: attribute fields in
paragraphs.

The case for this is that, with the additional example, [it better
illustrates partials](https://github.com/rails/rails/pull/41210), and is more coherent with the partials used for
the JSON views, and sure, it definitely does and is. However,
personally, I've never liked the `show` representation, because what
even is that. The `form` partial used by `new` and `edit` is for
representing resources as a form. If we had different views with
tables we were going to represent the model on maybe we'd have
partials for representing these models as rows. A big app might have
lots of representations for a single model and many places where they
are rendered to users. There might even be different JSON
representations in an API. If nothing else, I don't think the partial
should be named after the model as if it were some canonical
representation, but maybe name it "basic" or "static" or
something. Anyway, I'm planning on getting to that aspect, but maybe
sooner than I was first thinking.

Another thing I was thinking of covering, which might also come up
sooner than I expected, are techniques of making your own generators
and scaffolding. If you have a big Rails app, and you have some
standardized or even merely conventional ways was representing new
resources, writing your own scaffolding for them is a big time saver.

Okay, that's a survey of the scaffolding changes in Rails 7 which I
hadn't even looked at before my last post.

