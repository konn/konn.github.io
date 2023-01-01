---
title: 空から Mandelbrot 集合を見てみよう
date: 2015/01/24 02:26:00 JST
author: 石井大海
description: Mandelbrot 集合はよく知られたフラクタル図形ですが、これを「上から」（あるいは「下から」）見てみたらどう見えるのか、というお話。
tag: 数学,娯楽数学,フラクタル,マンデルブロ集合,Mandelbrot集合,Mandelbrot set,ステレオグラム,visualisation,visualization,ヴィジュアライゼーション,可視化,コラム
---

前説：Mandelbrot 集合とは？
===========================

**Mandelbrot集合**（マンデルブロー集合）と呼ばれるフラクタル図形がある。こんなやつ[^1]：


<img src="http://upload.wikimedia.org/wikipedia/commons/thumb/2/21/Mandel_zoom_00_mandelbrot_set.jpg/644px-Mandel_zoom_00_mandelbrot_set.jpg" class="thumbnail image-responsive" width="500">

検索すればゴマンとヒットするのであんまり細かいことは説明しないけれど、「ゼロから始めて自乗しては一定の定数$\xi \in \C$を足す」という操作を繰り返していったときに、何らかの値に収束するような$\xi \in \C$の全体をMandelbrot集合という。この図からもわかる通り、一見相似な図形が色々なスケールで無数に現れていて、大雑把にいえばこういう無限の入れ子構造になった図形の事をフラクタル図形という。因みに、この「相似な図形の入れ子構造」は、このMandelbrot集合の場合、相似に見えるだけで実際には微妙に異なる形が入れ替わり立ち代わり現れている。

規則がありそうでなさそうな感じ。その感じに多くの人が惹かれて、Mandelbrot集合の好きな部分を拡大して描画するアプリとかを書いて配布していたりする。

で。集合っていうのは本来色がない筈で、実は世間にある画像の大半は、Mandelbrot集合の「外側」に色を付けたものだ。何を基準に色をつけているのかというと、だいたい発散の速さを基準にしている事が多い。速いほど明るいとか、「発散した」と判定したループ回数の剰余で決めたりとか。まあ発散の速さを元にしてどういう配色をするのかは、結構適当に決めちゃってもサマになることがおおい。
僕も、以前 [`diagrams` パッケージ](http://hackage.haskell.org/package/diagrams)を使って図を描いてみたりしたことがある：

<img width="300" src="./3d-mandelbrot/mandelbrot.png" class="thumbnail image-responsive">

まあ作った時に diagrams の使い方に慣れてなかったので、ちょっとドット絵っぽくてなめらかじゃないけど、それでもそれなりに綺麗っぽく見えているといえるだろう。「発散の速さ」は、「最初に指定した回数反復して、何回で浮動小数の $\infty$ に発散または大きさが閾値を越えたか？」という方法で測っている。この配色は、ゼロ回反復から時計回りに色相環を辿っていって、指定反復回数上限で一周するように色を選ぶ形になっている。

本題：Mandelbrot集合を空から見てみる（立体錯視編）
==========================================

と、こんなドット絵みたいなやつを見せたいのが今回の本題だったのではない。そこら辺のひとがもっと綺麗な画像を公開してるしね。

今回の本題は、外ではあまり試みられていない、**Mandelbrot 集合を空から見てみる**という提案。

どういうこと？っていうと、そんなに難しいことじゃない。Mandelbrot集合の配色は、発散の速さで決められることが多かった。それを色にマップするのではなくて、**高さを割り当ててみたらどうか？**という趣旨。イマイチ何をいってるのかわからないかもしれないので、とりあえず次の画像を見てみてほしい：

[<img width="600" src="./3d-mandelbrot/mandelbrot-inv.jpg" class="thumbnail image-responsive">](./3d-mandelbrot/mandelbrot-inv.jpg)

エッこのノイズだらけの絵がなんだって？と思うかもしれないが、これは所謂[ステレオグラム](http://ja.wikipedia.org/wiki/%E3%82%B9%E3%83%86%E3%83%AC%E3%82%AA%E3%82%B0%E3%83%A9%E3%83%A0)というやつだ。マジカルアイとかいって、寄り目になると立体画像が見える！目が良くなる！という触れ込みの本が何年も前に流行ったけど、アレ。寄り目でがんばって眺めていると、上でみたMandelbrot集合の形がウスボンヤリと浮かびあがってくるんじゃないだろうか？

これは decafish さんの [STISPlot](http://www011.upp.so-net.ne.jp/decafish/STISPlot/STISPlotj.html ) というアプリケーションを使って生成したものだ。STISPlotには標高のデータからこういう立体錯視画像を作ってくれるソフトで、これで色々遊んでいるうちに「Mandelbrot集合の"標高"をこれでプロットしてみたら面白いんじゃないか？」と浮かんで、上のような画像を作ってみたのだった。

上の画像は、「発散が遅いほど上」に来るようにデータを作ってある[^2]。また、標高は線型ではなく逆数スケールで勾配を付けてある。これを、そのまま「発散したステップ数」について線型な画像にすると、次のような、より境目のクッキリした急勾配な画像が出来上がる：

[<img width="600" src="./3d-mandelbrot/mandelbrot-inv-steep.jpg" class="thumbnail image-responsive">](./3d-mandelbrot/mandelbrot-inv.jpg)

Mandelbrot集合本体の輪郭はわかりやすい。しかし、色々な「標高」の点が複雑な絡み合い方で共存しているのだ、というのは、先程の逆数スケールの方がわかりやすい。

これを「裏」から見てみたのが次の二枚だ（それぞれ順に逆数スケールと線型スケール）：

[<img width="600" src="./3d-mandelbrot/mandelbrot-forth-recip.jpg" class="thumbnail image-responsive">](./3d-mandelbrot/mandelbrot-inv.jpg)

[<img width="600" src="./3d-mandelbrot/mandelbrot-forth-steep.jpg" class="thumbnail image-responsive">](./3d-mandelbrot/mandelbrot-inv.jpg)

色分けすると「発散」の構造がわかりづらいが、こうして立体視を使うとけっこうわかりやすくていいんじゃないだろうか。

ふう……寄り目ばかりで疲れた……。ステレオグラムだと目を動かすとなんか一緒に立体部も動いてくれて、それもまた楽しい。

こうしてやってみると、選んだ範囲をクローズアップしてその部分のステレオグラムを生成するツールとかがほしくなる。STISPlotはプロットするデータを予め自分で用意しないといけないので、そうすると立体錯視画像を生成する部分から自分で作ることになるだろうか。また、錯視のノイズの色も、あるていど「発散の速さ」に応じて変化するとヴィジュアルもよくなるかなと思う。錯視の原理を知らないので何ともいえないけれど。

あと、ノイズをグッと睨んでいてMandelbrot集合が浮かびあがってきた瞬間は結構気持ち良いのだけど、ずっと寄り目は疲れるし、立体錯視は誰でも出来る訳ではないから、普通に3D画像としてレンダーするのもありかもしれない。これもそのうちやってみたい。立体錯視画像を生成するよりは、そういうツールの方が沢山あるからすぐできるだろう。あとは、その立体画像の上にも色をつけて……とか無限に夢が広がっている。

とりあえず、現時点はこんな感じで。また何か進展があれば更新する予定。

更新履歴
--------
* 2015/01/24 03:31:51 JST 初版公開


[^1]: 「[マンデルブロ集合 - Wikipedia](http://ja.wikipedia.org/wiki/%E3%83%9E%E3%83%B3%E3%83%87%E3%83%AB%E3%83%96%E3%83%AD%E9%9B%86%E5%90%88)」より。

[^2]: データ生成に使ったプログラムは[Gist](https://gist.github.com/konn/67b180ada53878cddb58)にある。