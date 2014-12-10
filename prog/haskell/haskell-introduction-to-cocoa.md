---
title: Haskell ではじめる Cocoa アプリ開発
date: 2014/12/13 00:00:00 JST
author: 石井大海
published: false
tag: haskell, cocoa, programming
description: Haskell による Cocoa アプリ開発の実際についての記事。Haskell Advent Calendar 2014 参加記事です。
---

この記事は[Haskell Advent Calendar 2014](http://qiita.com/advent-calendar/2014/haskell) の 13日目の記事です。

タイトルは釣りです。厳密にいえば、入門調のタイトルになっていますが、どちらかというと「Haskell で Cocoa アプリを作ってみた」的なエントリです。それでも、これから Haskell で Cocoa アプリを開発しようという人に役立つ知見をまとめたつもりではいます。対象読者は、これから Cocoa アプリ開発で Haskell を使いたいと思っている Haskell プログラマです。Objective-C （以下、Obj-C）がなんとなく読めればよりよいでしょうが、私じしんそこまで Obj-C は得意ではないので、まあ読めなくても何とかなるでしょう。また、本稿では OS X 向けの Cocoa 開発を対象とし、iOS アプリの開発は扱いません[^1]。

以下で採り上げる例は、全て[GitHubで閲覧可能](https://github.com/konn/objc-tutor)です。

Haskell で Cocoaアプリ開発？
==========================
その昔、HaskellでCocoaアプリ開発をするためのライブラリとして、[HOC](http://hoc.sourceforge.net/)というものがありました。これはCocoa APIに対する包括的なラッパーライブラリを提供することを企図したものでしたが、現在ではもうメンテナンスされておらず、Haskell / Obj-C 双方の変化に追随出来ていません。

今回以下で紹介するのは[`language-c-inline`](http://hackage.haskell.org/package/language-c-inline)を用いる方法です。名前に *inline* と入っている事からも推察出来るように、今回採る方法は巨大なラッパライブラリを利用するのではなく、Haskellのプログラムの中に Obj-C のコードを交ぜ書きするスタイルです。

ですので、Obj-Cを自分で書かなければならないという点ではラッパライブラリを用いるのに較べて少し手間かもしれません。しかし、ラッパを使うにしてもちゃんとしたプログラムを書くには、CocoaのAPIリファレンスを読まなくてはいけない訳ですし、さして必要な労力は変わらないでしょう。

また、`language-c-inline`じたいは、HOCが提供していたような、Obj-Cのオブジェクト・システムを再現するための機構は提供していません。しかし、GHC の最近の型機能を使えば、その必要な部分だけをエミュレートするような型システムを簡単に設計することが出来ます。そもそも、Obj-CのAPIを呼んだり、コントローラを書いたりする部分以外は関数型のパラダイムを使ってプログラミングする訳ですから、Obj-Cが提供するような高度なオブジェクト・システムすべてが使える必要はなく、単純な継承とアップキャスト、（unsafe な）ダウンキャスト、`id`{.Objectivec}型くらいがあれば十分な訳です。以下ではその技法も含めて解説出来ればな、という感じです。

また、オブジェクト・システムを模倣する上で、現在 Hackage に上がっている `language-c-inline`（`0.7`系統）には無い機能を使っています。なので、以下の作業をする上では[GitHubから直接最新版を取ってくる](https://github.com/mchakravarty/language-c-inline/)のが一番やりやすいと思います。そのうち最新版がリリースされる筈ですが、作者の方が忙しいので、Hackage に上がるのはもう暫く待ったほうが良いようです。

開発チュートリアル  {#tutorial}
===============

Hello, World!
-------------
色々と能書きを述べてきましたが、まあ取り敢えず Hello, World をやってみましょう。

```haskell
{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
module Main where
import Language.C.Inline.ObjC
import Language.C.Quote.ObjC

objc_import ["<Foundation/Foundation.h>"]

nsLog :: String -> IO ()
nsLog msg = $$(objc ['msg :> ''String] $$
              void [cexp| NSLog(@"%@", msg) |])

objc_emit

main = do
  objc_initialise
  nsLog "Hello, from Haskell-Cocoa!"
```

一つずつ解説していきましょう。まず四行目の `objc_import`{.haskell} は Template Haskell マクロで、裏で生成されるヘッダファイル等が `import`{.objectivec} するファイルを指定しています。今回は単にCocoaの機能を使って文字列を出力したいだけなので、`Foundation`を読み込ませています。

続く `nsLog`{.haskell} の部分では、`objc`{.haskell} マクロが呼ばれています。これは、引数の名前とその型のヒントのリスト、返値のアノテーションが付いた定義部を取って、しかるべきCラッパ関数と Haskell の FFI 宣言を生成するマクロです。ここでは、次の形のラッパ関数が定義されています：

* `String`{.haskell} 型の引数 `msg` を取り、
* 定義が `NSLog("%@", msg)`{.c} であり、
* 返値は `void`{.c} であるような C 関数。

しかし、ここで生成されているのはあくまで「定義」であって、FFI 宣言が実際にスプライスされたり、関数がヘッダファイルや`.m`ファイルとして書き出されるのは次の行の `objc_emit`{.haskell} が呼ばれた段階です。逆に云えば、いくら `objc` マクロなどを使って Obj-C コードを埋め込んでも、`objc_emit` が呼び出されていなければそれらが機能することはないので気を付けましょう。

その後の `main` 関数の所では、まず `objc_initialise` を呼んで上で定義した FFI や `nsLog` がちゃんと機能するような前処理を行っています。その後、`nsLog` を呼んでログを出力してめでたしめでたしという訳です。

では、これで動くかどうか実際にコンパイル&実行してみましょう。`hello.hs`などという名前で保存されているとすると、これをコンパイルするには、次のようにします：

```zsh
$$ ghc -c hello.hs
$$ cc -fobjc-arc -I/Library/Frameworks/GHC.framework/Versions/7.8.3-x86_64/usr/lib/ghc-7.8.3/include -I/Library/Frameworks/GHC.framework/Versions/7.8.3-x86_64/usr/lib/ghc-7.8.3/../../includes -c -o hello_objc.o hello_objc.m
$$ ghc -o hello hello.o hello_objc.o -package language-c-quote -package language-c-inline -framework Foundation 
$$ ./hello
2014-12-13 00:00:00.000 hello[88135:507] Hello, from Haskell-Cocoa!
```

気をつけるべき所は、まずは普段通りに `ghc (--make)` を呼ぶの**ではなく**、`ghc -c`を呼んでオブジェクトファイルを生成するに留めておくことです。これは、最初に`ghc`を呼んだ段階では未だ Obj-C ヘッダファイルや、それに付随するオブジェクトファイルが生成されていないので、リンクしようと思っても出来ないからです。なので、一旦 `ghc -c` を走らせて Template Haskell の処理を行わせて、Obj-C ヘッダ・ソースファイルを生成させているのです。実際、この後にディレクトリの内容を確認してみると、以下の四つのファイルが増えていることがわかります：

    hello_objc.h  hello_objc.m  hello.hi  hello.o

名前からもわかるとおり、この内で `*_objc.[hm]` という名前の物が今回生成された Obj-C ファイルです。このように、生成されるファイルは `元のファイル名_objc.[hm]` という名前で生成されるので、これと被るようなファイル名は使わないようにしましょう（また、どういうタイミングで生成されるのかもよくわからないのですが、`元のファイル名_stub.[hm]`という名前のヘッダファイルが出力されることもあります）。バージョン名は場合によっては異なることもあると思うので、適宜修正してください。たぶん `locate HsFFI.h` とかやれば見付かる筈です。

続いて、今回生成されたObj-Cファイルをコンパイルしてやる必要があります。そこで`cc`コマンド（実際には`clang`？）を呼び出してリンクしている訳です。ここで、`-I/Library/Frameworks/GHC.framework/Versions/7.8.3-x86_64` とか `-I/Library/Frameworks/GHC.framework/Versions/7.8.3-x86_64/usr/lib/ghc-7.8.3/../../includes` といったオプションが指定されているのは、Haskell の FFI で値をやり取りするのに必要なヘッダファイル（`HsFFI.h`）を見付ける為です。

最後に、今までに生成したオブジェクトファイルをリンクして、実行ファイルを作成します。その際には Haskell 側で使っているパッケージを `-package` オプションで、Obj-C 側で使っているフレームワーク（今回の場合は `Foundation`）を `-framework` オプションでそれぞれ指定してやる必要があります。

### ![](/img/dbend.png ) 再コンパイルに御用心 ###
生成される Obj-C ファイルで使われるラッパ関数名は、Template Haskell が走る度に異なる名前になります。ファイルの幾つかを変更して再コンパイルすると、「関数が見付からないよ！」と怒られる場合があります。その場合は、生成された `.hi`, `.o`, `_objc.[hm]` ファイルをすべて削除して、もういちど最初からやり直してみてください。

Currency Converter -- オブジェクト指向界の Hello, World! と FRP
-----------------------------------------------------------

### はじめの一歩：原始的な GUI ###

Hello, World! はうまくいきましたね。それではもう少しマトモなアプリケーションを書いてみましょう。OOPにおける Hello, World! とでもいうべき Currency Converter （通貨換算器）を作ってみましょう。

まず、Xcodeを開いて新しい "Cocoa Application" プロジェクトを作成します（そんなの面倒くさい！とか Xcode の使い方なんて知らん！という場合、[こちらに既に用意](/t/CurrencyConverter-01.zip)してあります！）。`MainMenu.xib` を開いて、次のような感じでウインドウにコントロールを配置し、`AppDelegate`{.objectivec} との間にアウトレット、アクションを設定してください：

![ウインドウの初期配置](../imgs/currency-first.jpg)

ロジックなどはまだ実装していませんが、この段階でいちど `Release` をターゲットにしてアプリケーションをビルドします。ビルドが完了したら、出来上がった実行ファイルを適当な場所にコピーしておいてください。

続いて、ついに Haskell でアプリケーションのロジックを記述します。

``` {.haskell}
```

TODO
----

* 簡易オブジェクト・システムの概要説明
    * `Symbol`{.haskell}型の `Typeable`{.haskell} インスタンス出来る迄の苦労云々
* スクラッチで作る
* Cocoa Bindings との兼ね合い？


[^1]: 理論上は ghc-ios と FFI を使えば iOS アプリを開発することも可能ですが、ghc-ios は執筆時点で Template Haskell に対応していないため、以下で説明する方法はまだ使えません。詳細後述。
