---
title: うたにっき
date: 2018/04/14 02:45:58 JST
author: 石井大海
description: ブログ未満の日常の破片
disqus: false
top-star: false
head: |
  <script src="//s.hatena.ne.jp/js/HatenaStar.js"></script>
  <script>
  Hatena.Star.Token = '7bf3845df18764ea7bfa120294f8c5ed1cd371e9';
  Hatena.Star.SiteConfig = {
    entryNodes: {
      'article.log': {
        uri: 'header h3 a',
        title: 'header h3',
        container: 'li.hatena-star'
      }
    }
  };
  </script>
---
{{{pagination}}}

{{#items}}
<article class="log" id="{{ident}}">
<header>
<h2><a href="/logs/{{ident}}.html">{{title}}</a></h2>
<div id="social-{{ident}}">
<i class="fa fa-bookmark" aria-hidden="true"></i>
<ul id="socials-{{ident}}">
<li id="facebook-{{ident}}">
<div class="fb-like" data-href="https://konn-san.com/logs/{{ident}}.html" data-send="false" data-layout="button_count" data-width="450" data-show-faces="true"></div>
<li id="hatena-bookmarks-{{ident}}">
<a href="//b.hatena.ne.jp/entry/https://konn-san.com/logs/{{ident}}.html" class="hatena-bookmark-button" data-hatena-bookmark-title="{{title}} - konn-san.com" data-hatena-bookmark-layout="simple-balloon" title="このエントリーをはてなブックマークに追加"><img src="//b.st-hatena.com/images/entry-button/button-only.gif" alt="このエントリーをはてなブックマークに追加" width="20" height="20" style="border: none;" /></a><script src="//b.st-hatena.com/js/bookmark_button.js" async="async"></script>
<li>
<script src="//apis.google.com/js/plusone.js"></script>
<div class="g-plusone" data-size="standard" data-count="true"></div>
<li class="hatena-star">
</ul>
</div>
<div><i class="fa fa-calendar" aria-hidden="true"></i> posted on {{date}}</div>
</header>
<div class="log-body">
{{{ log }}}
</div>
</article>
{{/items}}

{{{pagination}}}
