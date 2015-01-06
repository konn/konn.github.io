---
title: latexmk で楽々 TeX タイプセットの薦め（＆ biblatex+biberで先進的な参考文献処理）
date: 2015/01/06 23:48:54 JST
author: 石井大海
description: TeX ファイルの変更を監視して、必要な回数だけ自動タイプセットしてくれる latexmk の紹介です。OMake はもう古い。あとbibtexの代替である biber と biblatex についても紹介。
tag: TeX, LaTeX, upLaTeX, biblatex, biber, latexmk
---

OMake はもう古い！latexmk で快適 TeX コンパイルのススメ
================================================
TeX のタイプセットって面倒ですよね。数式番号をちゃんと出力するには二、三回タイプセットする必要がありますし、参考文献を自動処理しようとすると、`latex` と `bibtex` を交互に呼んだりしないといけなくなります。こういう手順を自動化できたら便利ですよね。あと、ファイルを保存する度にいちいち手動で一からコンパイル仕直すのも手間です。ファイルを変更したら自動的に PDF を生成してくれるような仕組みがあったら楽なんですけどねえ。

……というような用途だと、昔は [OMake](http://omake-japanese.sourceforge.jp/) が有名で、OMake を使ったら [身長が5cm伸びて彼女が出来た](http://d.hatena.ne.jp/hayamiz/20081208/1228727272) というような記事までありました。すごいですね！

しかし、OMake は最近あまりメンテナンスされていなくて、bib ファイルや目次が変更された際にちゃんと正しい回数コンパイルしてくれず、正しい PDF が得られないという問題があります。また、TeXファイルごとに OMakefile を書かないといけないので面倒です。

どうしたものか……と悩んでいたのですが、 [latexmk](http://users.phys.psu.edu/~collins/software/latexmk-jcc/ ) というツールを使えば、こうした悩みは全部一挙に解決してしまうことがわかりました！しかも、OMake と違い、latexmk は標準的な TeX ディストリビューション（TeXLive とか MacTeX）にはデフォルトで付いてくるので、何も準備せずに使うことが出来るのです！

以下では簡単に latexmk の使い方を紹介したいと思います。あと、折角なので biblatex と biber についても紹介します。

latexmk の設定
-------------
latexmk を使うには、まず設定ファイルを書いてやる必要があります。書かなくてもコマンドライン引数で全部指定出来るんですが、書いた方が楽です。まず、以下の内容を `~/.latexmkrc` に保存してください：

~~~{.perl}
#!/usr/bin/env perl
$$latex            = 'platex -synctex=1 -halt-on-error';
$$latex_silent     = 'platex -synctex=1 -halt-on-error -interaction=batchmode';
$$bibtex           = 'pbibtex';
$$dvipdf           = 'dvipdfmx %O -o %D %S';
$$makeindex        = 'mendex %O -o %D %S';
$$max_repeat       = 5;
$$pdf_mode	  = 3; # generates pdf via dvipdfmx

# Prevent latexmk from removing PDF after typeset.
# This enables Skim to chase the update in PDF automatically.
$$pvc_view_file_via_temporary = 0;

# Use Skim as a previewer
$$pdf_previewer    = "open -ga /Applications/Skim.app";
~~~

これは何をしているのか順に説明しましょう。

まず `$$latex`{.perl} の値はその名の通り .tex ファイルのコンパイルに使う `latex` コマンドを指定しています。ちなみに`-synctex=1`というオプションを付けておくと、SyncTeX が有効になり、SyncTeX対応のPDFビューワ（Skimとか）を使っていると、PDFの文章から tex ファイルの対応する部分に即座に飛ぶことが出来るようになります。便利です。

また、`-halt-on-error` オプションを付けておくと、文法エラーやらコマンド未定義やらでコンパイルに失敗した時、あの鬱陶しいプロンプトが出ずにそのまま TeX エンジンが終了してくれます。これは、後述の `-pvc` コマンドと組合せたときに、コンパイルに失敗してもいちいち C-d を送ったりする必要がなく、単にファイルを修正して保存しなおせば再度コンパイルが走ってくれるようになるので便利です。

`$$bibtex` とか `$$dvipdf`、`$$mendex` とかもその他のツールを指定しているだけです。

`$$pdf_mode	  = 3;`ってえのは、「latex で .dvi ファイルを生成してから dvipdf を使って PDF を生成するんだぜ」と云う意味です。日本語のTeX環境で直接PDFを吐かせる実用的な方法はまだない（XeTeXとかLuaTeXとかがあるにはありますが）ので、この方法を指定している訳です。

`$$pdf_previewer  = "open -ga /Applications/Skim.app";` ではPDFプレビューの方法を指定してます。僕はOSX使いなので、[Skim](http://skim-app.sourceforge.net/)を使っています。Skim はPDFファイルの変更を自動的に検知して表示を自動的に更新してくれる他、先述のように SyncTeX 機能を使って PDF から TeX ソースの当該個所に直にジャンプ出来たりします。スライドショー向けの機能もついていたりして、かなり便利なビューワです。

これと関連して必要になるのが `$$pvc_view_file_via_temporary = 0;` の設定です。latexmk はデフォルトではタイプセットする度に古い PDF を削除してから新しい PDF を生成します。しかし、この方式だと Skim がファイルの変更を検知できず、折角の自動更新が無効になってしまいます。そこで、「古いPDFファイルを削除せず上書きする」という風に設定を変更することで、Skim の自動更新機能を使えるようにしているのです。
例えば Skim ではなく Preview.app を使う場合だったらこの設定は要らず、その代わりに`$$pdf_update_command = "open -ga Preview %S';`とかしておけば勝手に更新されるようになる筈。

latexmk を使う
-------------
ここまで整えればもう一瞬です[^1]。コマンドラインで

```sh
$$ latexmk my-great-work.tex
```

とかやればあとは全部一瞬でよしなにやってくれます。また、

```sh
$$ latexmk -pvc my-great-work.tex
```

とすると、一度タイプセットしただけでは終了せず、`my-great-work.tex` や、それが依存するファイルに変更があるたびに latexmk が必要な回数だけ走って、自動的に PDF が更新されます。依存するファイルというのは、そのファイル自身以外にも .bib ファイルや .sty ファイル、include されている他の tex ファイルなどです。texmf に配置してあるグローバルなスタイルファイルやbibファイルの変更までも追い掛けて、更新があったらタイプセットしなおしてくれます。OMake ではここまでやってくれませんでした。これはべんり！

また、文書によっては違う設定をしたいときがあると思います。そういう時は `latexmk` に直接オプションを渡してやったり、そのディレクトリに新たに `.latexmkrc` を作成しておけばそっちの設定を優先してくれます。たとえば XeLaTeX でタイプセットしたいんだったら、

```sh
$$ latexmk -xelatex -pvc my-great-work.tex
```

とかすれば良い。他にも `--help` を見れば色々と有用な情報が書いてあります。設定するパラメータについては、latexmk 本体を読むと色々わかります。

そうそう、本来の用途からは外れますが、例えばタイプセットに成功する度に `git commit -am` したいというような場合は、

```perl
$$pdf_update_command = "git commit -am'update'";
```

とか書いておけば、PDFのアップデートコマンドが走るタイミングでコミットされて `git` で自動的にバックアップが取れます。


uplatex を使う
=============
オマケとして upLaTeX と biber+biblatex の紹介を書いておきます。

upLaTex っていうのは、`platex`の内部をユニコードに対応させたヴァージョンで、詳細は[ZRさんの記事](http://zrbabbler.sp.land.to/uplatex.html)を参照。pLaTeX から upLaTeX に乗り換えるのは簡単で、`jarticle` とか `jbook` とかのドキュメントクラスを使っている場合は `ujarticle` や `ujbook` を代わりに使えば良いだけです。`jsarticle`などの`js*`系の場合は、
```tex
\documentclass[a4j,twocolumn,uplatex,dvipdfmx]{jsarticle}
```
みたいに、`uplatex`を引数に渡せばそのままコンパイルが通るようになります。latexmk でこいつを使うようにするには、`-latex=uplatex`などとしてコマンドライン引数の形で指定するか、`.latexmkrc` をいじって

```perl
$$latex            = 'uplatex -synctex=1';
$$latex_silent     = 'uplatex -synctex=1 -interaction=batchmode';
$$bibtex           = 'upbibtex';
```

などとしておけばよいです。`upbibtex` というのは `pbibtex` の内部ユニコード版です。

biber と biblatex
=================
つぎに biblatex と biber の話をしましょう。biblatex というのは bibtex の後継で、より柔軟な運用が可能な文献管理・処理パッケージです。目で見て判りやすいところでは、例えばデフォルトで英字の書名は斜体、ユニコード文字の書名は太字で出力されるようになったりします[^2]。MacTeX や TeXLive であれば、これらは最初から何もしなくとも勝手に入っています。
もちろん見た目だけではなくって、例えば文献一覧のソート順を柔軟に指定出来たり、様々な実用的なフィールドが追加されていたりします。詳しい仕様については[biblatex.pdf](ftp://ftp.kddilabs.jp/CTAN/info/translations/biblatex/de/biblatex-de.pdf) を参考にしてください。

実際に BibLaTeX の使い方を見てみましょう。例えば今まで pBibTeX なんかでは日本人の名前をちゃんとソートするのに `Yomi` フィールドを指定していましたが、BibLaTeX では次のように `sortname` フィールドを指定すれば、後はよしなにやってくれます：

```bibtex
@book{Takeuti:2001,
	Author = {竹内 外史},
	Keywords = {集合論, 数学, 数学基礎論, 数理論理学},
	Publisher = {講談社ブルーバックス},
	Title = {新装版 集合とはなにか},
	Year = {2001},
	Sortname = {Gaisi Takeuti}}
```

また、例えば次のようにして `translator` や `Origlanguage` によって訳者、元言語などを指定することも出来ます：
```bibtex
@book{Kunen:2008,
	Author = {Kenneth Kunen},
	Keywords = {集合論, 数学, 数理論理学, 数学基礎論, 強制法},
	Language = {Japanese},
	Note = {\cite{Kunen:1980} の邦訳},
	Origlanguage = {english},
	Publisher = {日本評論社},
	Title = {集合論―独立性証明への案内},
	Translator = {藤田博司},
	Year = {2008}}
```

biblatex を使うには、preamble に次のように書きます：

```tex
\usepackage[backend=bibtex, style=numeric]{biblatex}
\addbibresource{myreference.bib}
\addbibresource{biblio.bib}
```

ここでは、裏でファイル処理などを行ってくれるバックエンドとして今まで通り `bibtex` を使っています。今までは文献一覧を印字したいところに`\bibliography{myreference.bib,biblio.bib}`{.tex}などと指定していましたが、BibLaTeX では preamble で `\addbibresouce` を使って一つずつ追加していきます。

本文で参考文献を表示したい時には次のようにします：

```tex
\printbibliography[title=参考文献]
```

今までは `\bibliography{..}` を使っていましたが、BibLaTeX では `\printbibliography` です。BibLaTeX はまだ日本語にローカライズされていないので、単に`\printbibliography` だけだと「参考文献」の代わりに「Bibliography」が節題になってしまいます。それを避けるために、`[title=参考文献]`として強制的に節題を指定している訳です。

biblatex にバックエンドとして bibtex と組み合わせて使う際には、一つ注意があります。どうした訳か `upbibtex` は `biblatex` の吐いた aux ファイルを処理しようとするとフリーズしてしまいます。ですので、latexmk での設定では
```perl
$$bibtex = 'pbibtex';
```
と pbibtex を利用するように指定してやりましょう。そうすればちゃんと動きます。

さて。biblatex は、biber と組み合わせれば文献のソート順も変えることが出来ます。[biber](http://texdoc.net/texmf-dist/doc/bibtex/biber/biber.pdf) というのは（コマンドラインツールとしての）bibtex の代替で、biblatexのバックエンドとして使うことを想定して開発されているものです。

例えば preamble に次のように書いたとします：

```tex
\usepackage[backend=biber, style=numeric]{biblatex}
\DeclareSortingScheme{mysorting}{
  \sort{\citeorder}
  \sort{\field{sortname} \field{author} \field{translator} \field{editor}}
  \sort{year}
  \sort{title}
}
\ExecuteBibliographyOptions{sorting=mysorting}
```

すると、参考文献は次のようにソートされます：

1. まず本文での出現順にソート
2. 次に `sortname`、`author`、`translator`、`editor` フィールドを順に見て云って、存在したらその値を使ってソート。
3. 続いて発表年でソートして
4. 最後にタイトルでソート

こんな具合に、biber と biblatex を組み合わせると色々と柔軟な設定が出来るようになります。
ただ、単に backend を biber に指定しただけでは uplatex でちゃんと参考文献一覧を印字出来ません。latexmk の設定を次のように弄るひつようがあります：

```perl
$$biber = 'biber --bblencoding=utf8 -u -U --output_safechars';
```

これは、「.bblファイルのエンコードは UTF-8、入出力も共に UTF-8 を使って、アクセント記号とかちょっとヤバめの記号は TeX にエンコードする」という意味の設定です。最後の「ヤバめ」云々というのがちょっと判りづらいですね。例えば biber はデフォルトのままだと bib ファイルで `G{\"o}del` のように書かれているものを、自動的に `Gödel` に変更して bbl に出力します。欧米では既にUTF-8をサポートしたTeXのエンジンが広く用いられているのでこれで問題がないのですが、upLaTeX はユニコード文字は全て和文だと判断して処理します。すると、上の文字は`G ö del`のように字間や書体が変な形で印字されてしまうのです。それを避けるために、アクセント記号をはじめとしたユニコード文字は LaTeX の命令を使ってエンコードした形で出力させる必要があります。それに必要なのが `--output_safechars` オプションという訳です。

こんな具合に、biber や biblatex を使えば従来の BibTeX では手のまわらなかった細かいところまで柔軟に設定することが出来ます。まだ日本語環境にローカライズされていなかったりしますが、それでも実用には耐え得るものになっていると云えると思います。

変更履歴
--------

* 2015/01/06 23:48:54 JST `-halt-on-error` についての説明を追加。
* 2014/01/31 21:21:58 JST （多分）初版公開。

[^1]: 本当は設定書かなくても平気なんですけど、platexとかの設定はちゃんと書いといた方が一々コマンドラインで指定しなくてよくて楽です。

[^2]: これは BibLaTeX がデフォルトでは `\emph{}` コマンドを使って書名を強調する仕様になっているからです。biblatex.pdf によればこの辺りのフォーマット方法とかも柔軟に設定出来るようですが、まだそこまで確かめていないです。また、和文と洋文とで異なる書体が混在しているのは気持ち悪いという事がありますが、その場合は `\emph{}` の定義を上書きしてしまえば、両方とも太字に統一出来たりします。
