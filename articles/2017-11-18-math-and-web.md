---
title: konn-san.comの数式描画を支える技術：KaTeX、MathJax、tex2img、Pandoc
date: 2017/11/18 15:24:41 JST
author: 石井大海
description: 最近 konn-san.com の数式描画エンジンをMathJax単体から KaTeX + MathJax に切り替えました。そもそもどのようにしてLaTeXからHTMLとPDFを同時に生成しているのかなども含め、その辺りの体験記を書きます。
tag: 数式,MathJax,KaTeX,Pandoc,LaTeX,TeX
---

弊サイトkonn-san.comでは数式を含んだ記事をHTMLとPDF両方で[大量に公開](/math)していますが、どのようにしてこれを実現しているか、という話をしようと思います。

なお、この執筆当時のkonn-san.comの生成器の概要は [GitHub](https://github.com/konn/konn-san.com-example) に上げてあります。

PandocとHakyll——文書変換とページ生成
---------------------------------------
初期の頃の記事を除けば、これらのほとんど全ては$\LaTeX$のソースから生成されています。
一方、むかしのごく少数の記事、たとえば[Gröbner基底の記事](/math/groebner-and-entrance-math.html)や[選択公理オフの記事](/math/SkeletonAndAC.html)はMarkdown記法からHTMLに変換しています。
これを可能にしているのが [Pandoc](http://pandoc.org)というドキュメント変換ライブラリ／アプリです。
Pandocは色々なフォーマットに対応していて、たとえば Markdown から MS Word の docx ファイルに変換したりも出来ます。

個々のページの変換は完全にPandocの機構に乗っかっていますが、サイト全体としてのサイトマップ、フィード、テンプレートなどの管理・生成は[Hakyll](https://jaspervdj.be/hakyll/)という静的サイト生成ツールを使っています。

これは[Jekyll](https://jekyllrb-ja.github.io)というRuby製のアプリのHaskell版クローンで、宣言的にサイトの構造を定義出来るので、こちらを使っています。
上のGitHubだと、[site.hsのmain](https://github.com/konn/konn-san.com-example/blob/master/site.hs#L90)あたりがその中心部分です。

そうそう、PandocもHakyllも共にHaskell製のアプリケーション／ライブラリです。
実際にはPandocのLaTeXからの変換機能は限定的（たとえば個人的に定義している定理環境がそのままだとHTMLに変換されないなど）なので、そうした部分の変換機構は自分で作ってやる必要があります。


$\KaTeX$とMathJax——ウェブ数式の未来はどっちだ
-----------------------------------------------
<figure class="figure mx-auto">
  <img class="img-fluid figure-img rounded" alt="MathJax" data-gifffer="./imgs/math-and-web/mathjax.gif">
  <figcaption class="figure-caption">MathJax版</figcaption>
</figure>
<figure class="figure mx-auto">
  <img class="img-fluid figure-img rounded" alt="MathJax" data-gifffer="./imgs/math-and-web/katex-auto.gif">
  <figcaption class="figure-caption">$\KaTeX$版、ブラウザ自動レンダー</figcaption>
</figure>

<figure class="figure mx-auto">
  <img class="img-fluid figure-img rounded" alt="MathJax" data-gifffer="./imgs/math-and-web/katex-pre.gif">
  <figcaption class="figure-caption">$\KaTeX$版、前処理版</figcaption>
</figure>
