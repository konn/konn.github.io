---
title: Emacs から Visual Studio Code に乗り換える作業のメモ
date: 2018/10/15 18:15:00 JST
author: 石井大海
description: |
  Emacs にそろそろ辟易してきたので、VSCode に乗り換えようとしています。その作業記録。
tag: emacs,vscode,env,ide,intero,Haskero,
---

発端
====
発端という程のことは何もないんですが、Emacsのプラグインが肥大化するに伴なって、食い合せが悪くて変な挙動をしたり、バージョンを上げたらパッケージが動かなくなったり……といったことにいい加減辟易してきました。
最近はよくわらないタイミングでビジーになったり死んでしまうことも多くなってきたので、いっそモダンなエディタに乗り換えよう、という機運でした。

そんな中で VSCode (Visual Studio Code) を選んだのは、何か最近話題になっていたからというのと、Atom があんまり手に合わない印象があったから、あと TL で VSCode 使ってる人が多かったからという感じで、まあ特に理由はないです。

要件
----
とはいえ、Emacs ではかなりカスタマイズを進めていたので、VSCode に移行するに当っては以下の要件が満たされて欲しい、という思いがあります。

* マトモな Haskell 統合環境
  - Emacs ではここ数年はずっと [intero] を使っていた。
    intero の補完・警告システムはかなり良かったので、完全互換ではないにせよ、十分伍するものが欲しい。

    そもそも今回乗り換えようと思ったのは、intero が最近あまりメンテされていない感じで、REPL で IO の結果を表示しようとすると Hang するとか、型の重いプログラムを書いていると、REPLで補完を働かせようとしてビジーになるとか、そういうことが続いたからでもあった。
* コードスニペットデータの移行
  - 僕はコードスニペットを多用してコーディングをしているので、[これまでのスニペット][current snippets]が移行出来ないと死ぬ。
  - スニペットエンジンは [Yasnippet] を使っていて、たとえば `module`{.hs} 宣言の補完とかは
    内部で ELisp を使って `piyo/Hoge/Fuga/Bar.hs` なら `module Hoge.Fuga.Bar where...`{.hs}、
    小文字のファイル名 `moke.hs` とかなら `module Main where`{.hs} となるようにしていたので、こういうことが出来ると嬉しい。
* Rust と Scala の統合環境
  * 最近使ってみているので、快適なのが欲しい。
* [YaTeX] の代替になるやつ
  - 永らく LaTeX の執筆環境では YaTeX-mode を利用してきた。  
    特に、YaTeX にはイメージ補完というのがあって、アスキーで表現された数学記号のイメージに相当する記号を手軽に入力出来る。
    例えば <kbd>;||-</kbd> で $\Vdash$ (`\Vdash`)、<kbd>;ox</kbd> で $\otimes$ (`\otimes`)、あるいは <kbd>:a</kbd> で $\alpha$ (`\alpha`)、<kbd>:ph-</kbd> で $\varphi$ (`\varphi`) といった具合に。

    僕はもうこれがなければ数式が打てないレベルまで来ているので、この機能の代替が欲しい。
    前 Atom に乗り換えようとした際にも、[Atom 向けのイメージ補完機構][atom shape compl]を作ってみていたのだが、途中で飽きて放り投げてある。キーイベントをフックするのとかが面倒だったし、よくわからないタイミングでキーが外に逃げたりするので……。

現状
----

上記の要件に対して、どの程度まで移行出来ているか、以下で順次書いていくことにする。

### Haskell 環境
この構築に一番手間取った。検索してヒットするのはだいたい以下の三つだろう：

* Haskell Language Server
  * [`haskell-ide-engine`][haskell-ide-engine] ベース。
    裏で `intero`, `ghc-mod`, `brittany`, `hoogle`, `HaRe` などを呼び出すことになっている。
* Haskero
* Haskelly
  * どちらも[`intero`][intero] ベース。

最終的には haskell-ide-engine に収斂していくべきだと思っているので、Haskell Language Server をまず入れてみたのがだ、結論から言うと全然動かなかった。
一応 `hie-wrapper` をめぼしい GHC バージョンごとにビルドしてインストールはしてあったのだが、モジュールの補完は利かないし、マウスオーバーで表示されるはずのドキュメントがいつまで経っても出てこないし、warning や error の報告も全然表示されないしで、検索するとヒットするような対応はだいたい試してみたつもりだが、ロクに動かなかった。

次いで Haskelly。これは調べた感じWarning や error の表示がないみたいなので割愛。

最後に Haskero。これもなかなかマウスオーバーでドキュメント表示が上手くいかなかった。
これはローカルのDB だけあればいいだろうと思っていたら、どうも global な DB も必要だったらしい。
なので、以下を実行すればマウスオーバーは出来るようになる：

```zsh
$ hoogle generate
$ cd path/to/project
$ stack haddock --keep-going
$ stack hoogle generate
```

多分最後の三行はローカルの DB 要らねと思ったら実行する必要はない。
実にはこの工程を試したあとに Haskell Language Server のマウスオーバーも試していて、その上でタイムアウトしている。

これで Haskero のマウスオーバーは出来るようになったが、警告やエラーが一向に表示されない。
調べてみると、次の issue が見付かった。

[](https://gitlab.com/vannnns/haskero/issues/82#note_73389918)

ここで書かれている対処法を参考に、インストールの Haskero を書き換える形で一時的に解決している。
なんでも `:l` した後に `:r` をするのが不味いらしい。
具体的には、``$HOME/.vscode/extensions/vans.haskero-1.3.1/server/intero/commands/reload.js`` の `ReloadRequest`{.js} クラスの `send`{.js} メソッドを以下のように書き換える：

```js
    send(interoProxy) {
        const filePath = uriUtils_1.UriUtils.toFilePath(this.uri);
        const escapedFilePath = interoUtils_1.InteroUtils.escapeFilePath(filePath);
        const load = `:l ${escapedFilePath}`;
        // const reloadRequest = ':r';
        return interoProxy.sendRawRequest(load)
        /*
               .then(response => {
            return interoProxy.sendRawRequest(reloadRequest);
        })
        */
               .then((response) => {
                return Promise.resolve(new ReloadResponse(response.rawout, response.rawerr));
            });
    }
```

すると、開いた直後は掻き消えることがあるが、一応問題なく動ようになった。

Haskero には REPL はなさそうだったので、[GHCi Helper] を入れた。
REPL としての intero は、`-fdefer-type-errors` が掛かっていたり、無駄に補完しようとしてビジーになったり、正直腐り切っていたので、`stack ghci` を使う GHCi Helper は性に合っていそうである。

あと、コードフォーマッタは Haskero は `brittany` を推奨しているようだが、僕は `stylish-haskell` を使っているので、[stylish-haskell用機能拡張][shvscode]を入れた。
HLint も使いたいので、[`hlint`機能拡張][hlint]も入れた。`haskell-linter`とかいうのもあるが、`apply-refact` を使った自動反映に対応しているっぽかったので、`hlint` の方を選んだ。

他にも Haskero の warning や error を同様に適用してくれる奴として、 [`Haskutil`][Haskutil]も入れた。
以上で、完全ではないがあるていどこれまでの Haskell 統合環境に近いものが構築出来た。

さらに、オマケとして、Haskell 編集中だけ Haskell用リガチャフォントの [Hasklig] を使うようにするため、以下の設定を `settings.json` に追加した：

```json
{   ...,
    "[haskell]": {
        "editor.fontFamily": "Hasklig,Menlo, Monaco, 'Courier New', monospace"
    }
}
```
### コードスニペットの移行
検索すると幾つか変換器がヒットするが、どう対応しているのかわからなかったので、Rust を使って変換器を書いた。

[](https://github.com/konn/yas-to-vscode)

Rust のパーザコンビネータである `combine` crate の良い練習になった。
Yasnippet では選択肢から選ぶという場合はそれ専用の ELisp 関数を呼び出しやる必要があったのだが、VSCode では専用の構文が用意されており、 `${1|foo,bar,buz|}`{.vscode} とすればよく大変楽だった。

取り敢えず、可能な限り選択肢からの変換やプレースホルダの変換の面倒を見つつ、内部で任意コードを呼びだしているっぽいスニペットが来たら、標準出力で警告をするようにしている。
使い方としては、

```zsh
$ mkdir -p path/to/output
$ crates ~/.emacs/share/snippets path/to/output
```

とすれば、言語ごとに `haskell.json` のような形でまとめられたスニペット定義が出来上がるので、上の警告を参考に書き換えつつ、macOS なら `$HOME/Library/Application Support/Code/User/snippets` にでも置けばよい。
ところで、如何に天下の Microsoft だからといって、サポートファイルの識別名を `Code` にするのはいかがなものか。
というか、なんで `$HOME/.vscode` にしないのか全く理解出来ない。

一方で、任意のコードが実行出来るわけではないので、上の要件に上げたモジュール名補完は今のところ有効打が見付かっていない。
一応、正規表現で一回だけ置換したり大文字小文字を弄ったり、空かどうかで分岐したり、といった機構が VSCode にもあるにはある。
だが、調べた感じ複数回置換は出来なさそうだし、そもそも正規表のキャプチャグループとスニペットのタブストップへの参照がどちらも `$1, $2, ...` という形でごっちゃになっている上に、変数の正規表現置換と空文字列の条件分岐を組み合わせようとすると一瞬で変な挙動をするので、今のところ実現出来ていない、という感じである。

何か良い手を思い付いた方は御一報下さい。いっそのこと機能拡張を作って、動的にスニペットを生成して補完候補として呼び出させるのも一つの手かもしれないが……。

### Rust と Scala
Rust
:    Rusty Code と Rust (rls) を入れたら万事 OK。

Scala
:    Scala Language Server、Scala (sbt) あたりを入れたら動いて見える。

### YaTeX の代替
残念ながらまだ見付かってないです。良い奴教えてください。
最低要件は上で書いたイメージ補完が使えることです。

[YaTeX]: https://www.yatex.org
[intero]: http://commercialhaskell.github.io/intero/
[current snippets]: https://github.com/konn/dotfiles/tree/master/.emacs.d/share/snippets
[atom shape compl]: https://github.com/konn/atom-latex-shape-completion
[Yasnippet]: https://github.com/joaotavora/yasnippet
[GHCi Helper]: https://marketplace.visualstudio.com/items?itemName=rcook.ghci-helper
[shvscode]: https://marketplace.visualstudio.com/items?itemName=vigoo.stylish-haskell
[hlint]: https://marketplace.visualstudio.com/items?itemName=lunaryorn.hlint
[Haskutil]: https://marketplace.visualstudio.com/items?itemName=Edka.haskutil
[Hasklig]: https://github.com/i-tu/Hasklig
[haskell-ide-engine]: https://github.com/haskell/haskell-ide-engine