---
title: プログラミング関連
author: Hiromi ISHII
description: GitHub などへの外部リンクと昔につくったプロダクトの紹介。
tag: Haskell,Ruby,オートマトン,IRC
top-star: false
---

ソフトウェアやプログラミング関連の記事を溜めておく場所です。

<div class="row-fluid">
<div class="span9">
記事一覧
=======
<dl id="#children">
\$children\$
</dl>

プロジェクト関連
==============
[computational-algebra](http://konn.github.io/computational-algebra/)
:    Haskellの先進的な機能をフルに使って実装された計算代数ライブラリ。

[MathChrome](https://github.com/konn/MathChrome)
:    [mimetex](http://www.forkosh.com/mimetex.html) や [Google Chart](https://developers.google.com/chart/) で書かれた数式を [MathJax](http://www.mathjax.org/) を用いて描画するようにする Google Chrome 拡張機能。`.crx` をダウンロードしたら、拡張機能ウインドウを開いてドラッグ・アンド・ドロップすれば使えます。


外部リンク
========
大抵のものは外部にある感じです。

[GitHub](https://github.com/konn/)
:    プロジェクトは主にここでホストしている。gist にも割と面白いコードが転がっている筈。ここで管理している主要なソフトウェアは以下の通り。

    *   [Yablog](https://github.com/konn/Yablog) - Yesod で書かれた blog エンジン。[これは圏です](http://blog.konn-san.com) はこれで動いている。
	*   [gitolist](https://github.com/konn/gitolist) - Yesod で書かれた Git Web Front-end。余りまだ機能がない。
	*   [algebra](https://github.com/konn/computational-algebra) - Gröbner 基底などの代数計算用のライブラリ。
	*   [yesod-auth-oauth](https://github.com/yesodweb/yesod/tree/master/yesod-auth-oauth) - ウェブフレームワーク Yesod の OAuth 認証プラグイン
	*   [authenticate-oauth](https://github.com/yesodweb/authenticate/tree/master/authenticate-oauth) - http-conduit を使って OAuth API を叩くためのライブラリ。
	*   [lk-proof-assistant](https://github.com/konn/lk-proof-assistant) - LK での証明をアシストして、結果の証明図を LaTeX で出力してくれる子。色々実験的な機能を使っているけどやや buggy。[解説記事](http://blog.konn-san.com/article/20120530/lk-proof-assistant-in-haskell)。
 
[Qiita](http://qiita.com/users/mr_konn)
:    Qiita に登録しているけど、最近はあまり使ってない。

[これは圏です](http://blog.konn-san.com)
:    ブログ。技術記事はここに集めてある。

デモ系
=====
[有限オートマトン・シミュレータ](./automaton/)
:    Haskell で書かれたオートマトン・シミュレータ。Haskell から JavaScript へとコンパイルしてくれる [Fay](http://fay-lang.org/) というものがあり、そのデモ。[詳しい解説記事](http://blog.konn-san.com/article/20121225/fay-introduction)。

過去の遺産
=========
中高時代に開発していた過去の遺物。あの頃は Ruby を使っていたので Ruby 1.8 製。今も動くかは知らん。

* [WereWolf](./ruby/werewolf/) - Ruby 製の汎用人狼系ゲームエンジン、らしい。
* [rupircd](./ruby/rupircd) - リレーしない IRC サーバ。上のと組み合わせて、人狼が組み込まれている IRC サーバを作ろうとしていたがその目的は忘れ去られた。今は [GitHub](https://github.com/konn/rupircd) を見た方がまだいい。
* [偽音](./ruby/gionbot/) - IRC 人狼bot。今あるのかはしらないがむかし人狼 BBS と云うものが流行って、IRC 上でも人狼をしたいと云う人達がいた。IRC人狼には真音（マロン）という bot があって、それは CHOCOA とかじゃないと動かなかったのでクローンを作った。上の WereWolf を使って書かれている。UO人狼のルールとかも実装してたんじゃなかったかなぁ。

いずれも古い Ruby 環境で作られたものなので今も動くかは知らない。多分動かない。
</div>
<div class="span3">
<a class="qiita-timeline" href="https://qiita.com/users/mr_konn" data-qiita-username="mr_konn">mr_konnのtips</a><script src="//qiita-widget.suin.org/widget.js"></script>
</div>
</div>
