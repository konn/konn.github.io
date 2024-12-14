---
title: できる！Template Haskell（完）
date: 2011/12/18 00:05:08 JST
author: 石井大海
description: 大昔に書いた Template Haskell の入門記事。はてなグループの消滅に巻き込まれてアクセス不能になっていました。大分古くなってしまった部分もありますが、考え方としてはまだ使える部分もあるのでWeb Archive からサルベージしました。
tag: Template Haskell,メタプログラミング,Haskell
---

# おことわり

以下の記事は、2011年末にはてなのHaskellグループで公開されていた記事を、2024年になって無理矢理復元したものです。

はてなグループの消滅に伴い本記事もアクセスできなくなっていましたので、Web Archive からサルベージし体裁を簡単に整えたものになります。
Typed Template Haskell に対する言及がなかったり、当時から AST の詳細がかわっていたりと、古くなってしまった部分も多くありますし、もっと実用的な例なども挙げられると思いますが、記録のため中身は体裁・リンク以外はいじらずにほぼ原文ママで掲載します。

---

この記事は Haskell Advent Calendar 2011 18日目の参加記事です [^1]。

- 前の記事：
  [id:thimura](http://haskell.g.hatena.ne.jp/thimura/)
  さんの [作ってみよう Enumerator - http-enumerator で Twitter API ぺろぺろ - ごったく](http://d.hatena.ne.jp/thimura/20111218/1324216591)
- 次の記事：ujm さんの [no title](http://vim-users.jp/2011/12/hack241/)

本稿は、Template Haskell の初心者にむけて、その原理と方法論を、THを書くときの思考法にそって丁寧に解説するこころみです。なので幾分冗長なところがありますので、知ってると思ったら飛ばしてしまって大丈夫です。

## Template Haskell とは？── TH で出来ること

Template Haskell とは、Haskellで [^2] コンパイル時メタプログラミングを行うための仕組みです。

……何だかよくわかりませんね。Template Haskell（以下 TH と略）とはマクロの一種です。マクロっていうのは、三崎漁港で取れるお魚のことではなく、簡単に云うとプログラムを生成するためのプログラムです。

そんな事をして何が嬉しいのか？例えば応用例としてはこんなことが出来ます：

- 型クラスインスタンス宣言の自動生成
- 他言語とのブリッヂの自動生成
- 準クォートによるリーダマクロ[^3]
- などなど……

これらを総合して、「何らかのパターンのある記述を自動的に生成することができる」と云うことになるでしょうか。こういうのをboilerplate と云うそうです。

次のような事は出来ません：

- 型システムを拡張する
- 彼女

さて。生成すると云いましたが、実際にはHaskellプログラムの構文木をコンパイル時に組替え・合成して、それをその場に埋め込むと云う形になっています。

と云うことで、THを使いこなすことと云うのは、構文木を適切に合成すること、と云い換えることが出来るでしょう。幾つか構文木の例を見ていきましょう。

**例1**：

```haskell
putStrLn "hello, TH World！"
```

```haskell
AppE (VarE 'putStrLn) (LitE (StringL "hello, TH World！"))
```

**例2**：

```haskell
fact 0 = 1
fact n = n * fact (n - 1)
```

```haskell
[FunD 
  (mkName "fact")
  [ Clause [LitP (IntegerL 0)] (NormalB (LitE (IntegerL 1))) []
  , Clause [VarP $ mkName "n"] 
      (NormalB 
        (InfixE 
          (Just (VarE $ "n"))
          (VarE '(*))
          (Just 
            (AppE (VarE $ mkName"fact") 
              (InfixE 
                (Just (VarE $ mkName "n")) 
                (VarE '(-)) 
                (Just (LitE (IntegerL 1)))
              )
            )
          )
        )
      ) 
      []
  ]
]
```

**例3**：

```haskell
data MyGreatData = MGD String Int
```

```haskell
DataD [] (mkName "MyGreatData") [] 
  [NormalC (mkName "MGD") [(NotStrict, ConT ''String),(NotStrict, ConT ''Int)]]
  []
```

どう？簡単でしょう？

## Template Haskell ことはじめ

……はい。全然簡単じゃないですね。僕もそう思います。「こんなの一々書かなきゃいけないなら
TH いいや……」と云う声も聴こえてきそうです。

また更に、この構文木はええと GHC 7.0.4 標準添付の `template-haskell-2.5.0.0` 準拠の物です。これはどういうことかと云うと、将来のバージョンで構文木が変わるかもしれない、と云うことです。

うひゃあ、そんなの追随出来る訳ないじゃんどうするの……と云う感じです。生構文木とか書ける気がしないし、構文木も変わっちゃうんじゃあ……。

しかーし。問題はありません。TH を使う場合、簡単な例であれば生の構文木を書かないで済ませることが出来ます。また、生の構文木を書く場合も簡単にその構文木を書き下すことが出来る方法があります。

それを、これから解説していきたいと思います。

### そのいち：Template Haskell のバージョンを確認する

TH を使って開発をする前に、自分がどのバージョンを使っているのか確認しましょう。先程もいいましたが、構文木の形式が違ったり、後で述べる
splice や準クォートの方式が違ったりします。

```bash
ghc-pkg list template-haskell
```

などとすれば確認出来ます。ここでは最新の `template-haskell-2.5.0.0` に準拠しています。

### そのに：Keep your reference handy！

TH を本格的に使っていこうと思ったら、リファレンスを読まずにやっていくのは辛いものがあります。

Haskell Platform を使っていればリファレンスがローカルにインストールされていると思います。
無ければ Hackage のリファレンスを参照すると良いでしょう。

### そのさん： ghci を起動しておく

はい。実は構文木を確認したりする作業は　GHCi　で行ないます。なので、GHCiを起動しておきましょう。その際に、

```bash
ghci -XTemplateHaskell
```

と必ず `-XTemplateHaskell` プラグマを付ける事を忘れずに。

## TH の仕組みと実際の手順

### 構文木の簡単な説明

さて、それでは実際に TH を使ったプログラミングをしていきましょう。

その前に、今後のために少し TH における構文木の構造を簡単に説明したいと思います。
GHC のバージョンによって微妙に変更があったりはしますが、概ねこの構成は変わりません。

構文木は大きく分けて以下の四つの種類に分かれます[^4]。

1. 式
2. パタン
3. 宣言
4. 型

それぞれについて見ていきましょう。式と云うのは、通常の式、例えば `1 + 2`{.hs} とか `\x y -> x * y`{.hs}などです。これらは **`Exp`** 型の構文木です。

対してパタンの構文木は、関数宣言やパタンマッチに登場するパタンのことで、**`Pat`**型の構文木です。

宣言構文木は **`Dec`** 型で、関数宣言や型の宣言、型クラス・インスタンス宣言などがこれに当ります。

型構文木は文字通り型を表わす構文木で、**`Type`** 型を持ちます。

実際には、これより更に細かい構文木 [^5] の多重再帰によってTHの構文木は定義されています。

### 構文木を見ながら仕組みを学ぶ

さて。色々と御託を並べましたが、習うより慣れろです。実際に TH を使ってみましょう。

まず GHCi を立ち上げ、`Language.Haskell.TH` を読み込みます。

```haskell
$ ghci -XTemplateHaskell -XQuasiQuotes 
GHCi, version 7.0.4: http://www.haskell.org/ghc/  :? for help
Loading package ghc-prim ... linking... done.
Loading package integer-gmp ... linking ... done.
Loading package base. .. linking ... done.
Loading package ffi-1.0 ... linking ... done.
Loading package filepath-1.2.0.0 ... linking ... done.
Loading package old-locale-1.0.0.2 ... linking ... done.
Loading package old-time-1.0.0.6 ... linking ... done.
Loading package unix-2.4.2.0 ... linking ... done.
Loading package directory-1.1.0.0 ... linking ... done.
Loading package process-1.0.1.5 ... linking ... done.
Prelude> :m Language.Haskell.TH
```

では、試しに、最初にも例示した式

```haskell
putStrLn  "hello, TH World！"
```

の構文木を見てみましょう。

```haskell

Prelude Language.Haskell.TH> runQ [e|putStrLn "hello, TH World！"|]
AppE (VarE System.IO.putStrLn) (LitE (StringL "hello, TH World！"))
```

幾つか説明が必要ですね。上で出て来た `[e| ... |]` は **式クォート**
と呼ばれます。その名の通り、`...`に書いた部分の構文木をパースして構文木に変換してくれるものです。こうしたクォートは全部で四種類あります。それを以下に纏めます。

式クォート
:   `[e| ... |]` の形式。よく使うので `[| ... |]` と `e` を略して記せる。式をクォートする。返値は **`Q Exp`** 型

パタンクォート
:   `[p| ... |]` の形式。パタンを記述する。返値は **`Q Pat`**型

宣言クォート
:   `[d| ... |]` の形式。函数宣言やクラス宣言、データ宣言などを表わす。返値は **`Q Dec`** 型

型クォート
:   `[t| ... |]` の形式。型レベルの式を記述する。返値は **`Q Type`** 型。

見てわかるとおり、先程挙げた四つの構文木と対応するものがあります。

さて。返値が **`Exp`** ではなく **`Q Exp`** と頭に `Q`
が付いた状態になっているのに気付いたと思います。これは TH のマクロ展開が
`Q`
モナド
と呼ばれる
モナド
の中で行われるためです。

なぜそのような特別な
モナド
が用意されているのかと云うと、構文木を合成する際、型や函数の定義を参照したり外部と入出力を行ったり、と云ったIO処理を行えるようにするためです。`Q`
モナド
の中では次のようなことが出来ます：

- 型や函数、
    インスタンス
    の情報を取得する
- 任意の `IO` 処理を実行する
  + `runIO :: IO a -> Q a` と云う函数がある
- フレッシュな名前を供給
- ソースの現在位置を取得する

任意のIO処理が実行出来るので、外部からファイルを読んでそれを基に構文木を組替えたり、コンパイル時に音楽を鳴らすことだってできます[^6]。そう、`Q`モナドならね。

合成した構文木をプログラムの任意の場所に埋め込む（これを接合（splice）する、と云います）際にも、これらの操作をするためにQモナドに包まれた構文木を渡します。なので、式クォートの類はすべて予め `Q`
モナド
で包まれているのです。

`Q`
モナド
に包まれた構文木は処理系が勝手に外してその場に接合する訳ですが、
GHCi
上などでデバッグの用途でそれを外したいときがあります。たとえば、構文木が
`Q` に包まれたままでは print できません。そのためには、`Q` の値を `IO` の値[^7] に変換する `runQ`
函数を使います。まさに上の例では式クォートで生成した木を `runQ` で `IO`
に引き戻して
GHCi
に `print` させていますね。

と云うところで、例に戻ります。出て来た構文木を
GHCi
にコピペしてみましょう。

```haskell
Prelude Language.Haskell.TH> let expr = AppE (VarE System.IO.putStrLn) (LitE (StringL "hello, TH World！"))

<interactive>:1:23:
Couldn't match expected type `Name' with actual type `String -> IO ()'
  In the first argument of `VarE', namely `putStrLn'
  In the first argument of `AppE', namely `(VarE putStrLn)'
  In the expression:
    AppE (VarE putStrLn) (LitE (StringL "hello, TH World！"))
```

おや、叱られてしまいましたね……？何がいけないんでしょう。

実は、吐き出された生のままの構文木ではなく、こう打つのが正しかったのです：

```haskell
Prelude Language.Haskell.TH> let expr = AppE (VarE 'putStrLn) (LitE (StringL "hello, TH World！"))
```

上で `System.IO.putStrLn` となっていたところが `'putStrLn` に変わっていますね。これはなんでしょう？

これは実は名前クォートと呼ばれるものです。名前は **`Name`**型の値で、その名の通り函数や型の名前などを指します。構文木は `show` するとその形のまま吐かれますが、`Name` 型を `show` するとそれが指す名前そのものになってしまい、コピペしただけでは名前として取り出せないので上のエラーが出た訳です。

名前クォートには以下の二種類があります：

函数名クォート
:   `'putStrLn` など一重のシングルクォート + 識別子の形。式文脈中の函数の名前を指す。`'Just` などデータ構築子に対しても使える。

型名クォート
:   `''String` など **二重のシングルクォート[^8]** + 識別子の形。型文脈中の型構築子の名前を指す。

上の場合は、`System.IO.putStrLn` と名前が直がきされて式と見分けがつかなくなっていたところを、 `'putStrLn`と名前クォートをつかって直してやった訳です。

名前クォートは既に定義されている函数を指す他に、新たな引数や変数をつくりたいときにも使われます。

さて、今までは式から構文木を調べる方法を見てきましたが、逆はどうすればいいのでしょう？構文木が実際にどんなプログラムを表しているのか知りたいですよね？

実はそれはとても簡単です。TH には HughesPJ ライブラリを利用したプリティプリント機能がついていて、任意の構文木をきちんと動くプログラムの形に整形することが出来ます。

このあたりの詳しい函数の使い方については、[リファレンスマニュアルのこの部分](http://hackage.haskell.org/packages/archive/template-haskell/2.5.0.0/doc/html/Language-Haskell-TH.html#g:23)を参照するとよいと思います。使うのは主に `ppr` 函数と `pprint`函数でしょう。

例えば、今の例の `expr` で試してみると、

```haskell
> let expr = AppE (VarE 'putStrLn) (LitE (StringL "hello, TH World！"))
> ppr expr
System.IO.putStrLn "hello, TH World！"
```

こんな具合に元の式に復元出来ました。関数名に関しては修飾された形でより正確なものになっているのがわかると思います。

さて、駆け足でしたが、こんな感じで前提知識の紹介を終えたいと思います。次の節からは、幾つかの例を通して、TH
を使った実際の開発について見ていきたいと思います。

## 例1：任意長タプルを扱う函数

それではさっそく例を見ていきましょう。THの入門記事や紹介では必ずといっていいほど紹介されている、任意長タプルを扱う函数をここでも扱ってみたいと思います。

ここでいう任意長タプルを扱う函数、と云うのは、 二要素タプルに対する `fst` や `snd`のようなものを、任意長のタプルについて定義してあげよう！と云うことです。

次のような感じにしたいですね。

```haskell
sel
  :: Int  -- ^ タプルの要素数
  -> Int  -- ^ 取り出したいタプルの要素の番目
  -> ExpQ -- ^ 函数を表わす構文木
sel len nth = ...
```

返値の `ExpQ` は `Q Exp` の型シノニムです。

使い方としては、`sel` から返された構文木を接合して、実際の函数として使ってやる形になります。

……あ、そうそう、構文木の接合をどうやるかという方法を説明していませんでしたね。合成した構文木を埋め込むには、`$( )` で囲めばよいのです。そう、次のように（GHCi のプロンプトで試している想定です）：

```haskell
> $(sel 3 2) (1, 2, 3)
2
```

また、接合したい式が単なる名前一つだけで済む場合、つまり、

```haskell
hoge = $(fuga)
```

の様な場合は

```haskell
hoge = $fuga
```

と書き直すことが出来ます。ここがややこしく、僕らの大好きな中置
演算子 `($)` と混同しやすいです。
Haskell のパーザは、`$` の後にスペースが開いていれば演算子、そうでなければ接合と解釈するので気を付けましょう。THのコードを書く際には、なるべく`($)` を使わない方がよいかもしれません。

このように式や型を接合する際には`$()`が必要ですが、トップレベル宣言の接合の場合は
`$( )` を省略することが出来ます。その例は後程インスタンス生成の例で見ていきたいと思います。

構文木の接合は、普通の
Haskell
のソース中にも書けますし、更に式クォートや型クォートの中にも書くことが出来ます。普通のソース中への接合を特にトップレベル接合と云います。

構文木を接合する際に一つ、Stage Restriction
と云う制限があります。これはトップレベル接合では同一モジュール内の函数や引数を参照出来ないという制限です(式クォート中であれば問題ありません)。これについては後の落とし穴のコーナーで言及します。

閑話休題。先程から `sel`
の返値を「函数」と呼んできましたが、実際には「式」がそこに接合されることになるので、実際にはラムダ式がそこにくることになります。

さて、では sel の実装に入りましょう。知るべきことは何でしょう？

1.  ラムダ式を表わす構文木は？
2.  タプルを表わす構文木は？
3.  変数を表わす構文木は？

こんな所でしょうか。これらを一つずつ調べてもいいですが、今生成したいのはラムダ式だったので、目的のラムダ式と似たようなラムダ式、例えば `\(x,y,z) -> y`{.hs} の構文木を表示させて仕舞えば簡単ですね。

では、
GHCi
を起動してください。 `-XTemplateHaskell` をつけて起動して、`Language.Haskell.TH` を読み込むのを忘れずに。

```haskell
$ ghci XTemplateHaskell 
GHCi, version 7.0.4: http://www.haskell.org/ghc/  :? for help
...
Prelude> :m Language.Haskell.TH
Prelude Language.Haskell.TH> 
```

では、構文木を表示させてしまいましょう。

```haskell
> runQ [|\(x,y,z) -> y |]
LamE [TupP [VarP x_0,VarP y_1,VarP z_2]] (VarE y_1)
```

大体形が予想出来たと思います。ラムダ式を表わす構文木(の構築子名)が `LamE`、タプルのパタンを表わす構文木が `TupP`、変数のパタンおよび式を表わすのが `VarP` および `VarE` です。

ここから大体想像がつくと思いますが、構文木のデータ構築子には、それが何を表わす構文木なのかを示す接尾辞が付いています。式ならば大文字の`E`、パタンなら `P`、型は`T`、宣言は `D` 、といった具合です。

なお、上式中の `x_0`,  `y_1`, `z_2` はそれぞれ `Name` です。

さて、ではこれを参考に `sel` を実装していきましょうー。

はい、既に実装したものが実は下にあります (`TupTH.hs`) ！

```haskell
module TupTH where
import Language.Haskell.TH
import Control.Monad

sel :: Int -> Int -> ExpQ
sel count nth = do
  vars <- replicateM count $ newName "x"
  lamE [tupP $ map varP vars] (varE $ vars !! (nth-1))
```

`sel` の定義一行目では、タプルの各要素に使われる名前を生成しています。ここで登場する `newName` と云うのは、その型

```haskell
> :t newName
newName :: String -> Q Name
```

からもわかるとおり、文字列を取ってそれを元に名前を作って返す函数です。あれ、でも最初の方で
`mkName` と云う函数が出て来ていましたね。あれはどうちがうんでしょう……？型は、、、

```haskell
> :t mkName
mkName :: String -> Name
```

newName とそっくりですね。でもよく見ると返値が `Q` で包まれていません。

この違いはなにか？次を試してみるとわかります。

```haskell
> $(varE $ mkName "pi")
3.141592653589793

> $(varE =<< newName "pi")
<interactive>:1:3:
    Not in scope: `pi[a19D]'
    In the result of the splice:
      $(varE =<< newName "pi")
    To see what the splice expanded to, use -ddump-splices
    In the expression: $(varE =<< newName "pi")
    In an equation for `it': it = $(varE =<< newName "pi")
```

このように、`mkName` で作られたものは
文脈
中に被る名前があればそれを参照しますが、`newName`
は完全にフレッシュな、衝突しない名前を返します。これが出来るのは、 `Q`
モナド
に包まれているからです。上のエラーをよくみると、``Not in scope: `pi[a19D]'`` となっていますね。newName
で生成された識別子は厳密には `pi` ではなく `pi[a19D]`
と云う名前であることになっていて、後ろの `[ ]`
に囲まれた部分がその一意性を保証している訳です。

sel の例では、newName を count 回繰り返してフレッシュな名前を count
個得ています。逆に上の vars を定義している部分を、

```haskell
let vars = replicate count $ mkName "x"
```

に差し替えると、`sel`
の定義を読み込んだ時点ではエラーは出ませんが、それを接合しようとすると、、、

```haskell
> $(sel 3 2) (1,2,3)

<interactive>:1:3:
    Conflicting definitions for `x'
    Bound at: <interactive>:1:3-9
              <interactive>:1:3-9
              <interactive>:1:3-9
    In a lambda abstraction
    In the result of the splice:
      $(sel 3 2)
    To see what the splice expanded to, use -ddump-splices
    In the expression: $(sel 3 2)
```

こんな具合に mkName
で生成されている名前が被ってしまっているのでエラーが出ます。つまり、

```haskell
\(x,x,x) -> x
```

のような何が何やらよくわからない式になってしまっているので、弾かれて仕舞うわけです。

ここで一つポイントなのが、このエラーが出るのは *接合されるとき*
だということです。函数としては型があってしまえば構文木としてはまったく問題ないので、それが実際に式へと変換されて始めてエラーがおきるのです。

では二行目に移りましょう。ここではさっき確認した構文木を参考に、ラムダ式を表わす構文木を生成しているところです。

```haskell
lamE [tupP $ map varP vars] (varE $ vars !! (nth-1))
```

一行目で生成した `vars`
はあくまで名前のリストだったので、引数パタンがくる部分では `map varP`
で名前を参照するパターンに、第二引数の実際の式の部分では $n$-番目の引数に
`varE` を付けて式に変換しています。

さて。注意深くみてみると、おや？と思われたひとも多いと思います。さっきみた構文木では
`LamE` や `VarP`
のように先頭が大文字になっていましたよね。データ構築子なので当然です。ところが上の例では、
`lamE`
や `varP`
のように先頭が小文字になり、関数呼び出しになっています。試しに型を見てみましょう。

```haskell
> :t lamE
lamE :: [PatQ] -> ExpQ -> ExpQ
```

TH では `Q`
モナド
の中で合成を行う都合上、データ構築子の引数や返値の構文木を `Q`
モナド
で包んだ形で扱ったほうがべんりなので、こうしたユーティリティ函数が定義されているのです。大抵の構文木の構築子に対してこうした先頭を小文字に直したものが用意されています。また、頻出するイディオムを自動化するための函数も多く定義されています。こうしたものを知っているかいないかでは手間が大きく違うので、リファレンスを一通り見ておくとよいでしょう。

## 例2：任意長タプルの別解 ── ワイルドカードとデータ構築子

さて、前回は簡単に実装出来てしまいましたが、タプルは単なる
代数的データ型
でした。

```haskell
(1,2,3) == (,,) 1 2 3
(3,4)   == (,) 3 4
```

また、変数をタプルの要素数の数だけ生成しましたが、これはいかにも無駄なので、ワイルドカードパタン `_`
で代用出来そうな気もします。なので、ここでは以下を使った別解を考えてみたいと思います：

1.  タプルのデータ構築子に対するパタンマッチ
2.  ワイルドカードパタン

ワイルドカードパタンはそのものズパリ、`WildP`
と云うのがそれを表わす構文木です。勿論 `Q`
モナド
版の `wildP` もあります。これを使えば全然問題ないですね。

ではデータ構築子のパタンマッチはどうすればいいのでしょう？と云う訳でまた
GHCi
に訊いてみましょー。

```haskell
> runQ [p| (,,) 1 2 3 |]
ConP GHC.Tuple.(,,) [LitP (IntegerL 1),LitP (IntegerL 2),LitP (IntegerL 3)]
> :t ConP
ConP :: Name -> [Pat] -> Pat
```

`ConP` と云うのがその物ズバリのようですね。引数から明らかなように、構築子の名前が第一引数、構築子に対する引数のリストが第二引数になります。

ところで、一つ問題があります。上では3要素決め打ちだったのでデータ構築子の名前は
`(,,)` で大丈夫でしたが、任意長に対してはどうすればいいのでしょう？

```haskell
mkName ("(" ++ (replicate count ',') ++ ")")
```

で出来ないこともないですが、いかにもスマートではないですね……。

と、云わけでリファレンスを紐解きましょう。すると、

```haskell
tupleDataName :: Int -> Name
```

と云うお誂え向きの函数が見付かります！こんな具合に、
GHCi
に頼り切らずリファレンスで何かよいものがないかを探すのは、再三の繰り返しになりますが、とても大事です。基本的な函数やデータ型は
`Language.Haskell.TH`、ユーティリティ函数やシノニムは `Language.Haskell.TH.Lib` を参照すると良いでしょう。

では、上のものを使って `sel` を書き直してみたものが次です：

```haskell
sel' :: Int -> Int -> ExpQ
sel' count nth | count >= nth = do
  var <- newName "x"
  let pats = replicate (nth - 1) wildP ++ [varP var] ++ replicate (count - nth) wildP
  lamE [conP (tupleDataName count) pats] (varE var)
```

説明はもう殆んど要らないと思います。沢山変数を生成するかわりに目当ての引数と残りをワイルドカードにしたのと、あとは明示的にタプルに対するパタンマッチで書き直したのだけです。

さて、以上の知識を得た上で、次の課題をやってみたいひとはやってみてください。

1. 任意長タプル要素の入れ替え函数を定義する (難易度：易)
2. 一般化 `flip` の定義。(難易度：易)
3. 一般化 `curry`/`uncurry` の定義。(難易度：易)
       - つまり、 `$(ncurry 5) :: ((a,b,c,d,e) -> f) -> a -> b -> c -> d -> e -> f` となるような `ncurry` の定義。`uncurry`も同様。

## インスタンス の自動生成──型情報と定義の扱い

さて。大体コツはつかめてきたでしょうか。タプルの例はちょっと簡単すぎた感があるので、いっきに進んだ例を扱ってみましょう。

ソースコード全てを載せると大変なので、GitHubに載せた[ソースコード](https://github.com/konn/template-haskell-tutor)を参照してください。

次のようなバイナリ・エンコードのための型クラス、`Bin` を考えます(完全な実装は
GitHub の `Bin.hs` をご覧ください)：

```haskell
data Bit = O | I deriving (Show, Read, Eq, Ord)

class Bin a where
  encode :: a -> [Bit]
  decode :: [Bit] -> (a, [Bit])

instance Bin Int where
  encode = ...
  decode = ...
```

この `Bin` のインスタンスを一々生成していたのではとても面倒なので、何とかして自動生成したいなあ、と思いますよね？思ってください[^14]。

さて、自動生成したいと思って貰えたと思います。その方法は総称プログラミングの手法をつかったり色々ありますが、ここでは
Template
Haskell
を使おうと思います。TH
でそんなことが出来るの？と思うかもしれませんが、TH には

> - 型や函数、インスタンスの情報を取得する

と云う最強の技が残っていたのでした。この章の目的は、この機能を説明することです。

型、函数、
クラス
の情報を取得するには
**[`reify`](http://hackage.haskell.org/packages/archive/template-haskell/2.5.0.0/doc/html/Language-Haskell-TH-Syntax.html#v:reify)[^9] 函数**

```haskell
reify :: Name -> Q Info
```

を使用します。名前に紐付けられている実体に関する情報
[`Info`](http://hackage.haskell.org/packages/archive/template-haskell/2.5.0.0/doc/html/Language-Haskell-TH-Syntax.html#t:Info)
を返してくれます。リファレンスを読むとわかるように、2.5.0.0 時点では
型クラス
、型構築子、データ構築子、変数、
型変数
の情報を得ることが出来ます。

なんだかわくわくしますね！早速試してみましょう。

```haskell
$ ghci -XTemplateHaskell 
GHCi, version 7.0.4: http://www.haskell.org/ghc/  :? for help
> :m Language.Haskell.TH
> runQ $ reify ''String
Template Haskell error: Can't do `reify' in the IO monad
*** Exception: user error (Template Haskell failure)
```

おや、怒られてしまいましたね……。そう、実は **`reify`函数はコンパイル時にしか実行出来ない** んです。

これは、Template
Haskell
は裏で
GHCi
を使って色々な処理をしているかららしいです。らしい、と云うのは良くしらないと云うことです。

なので、「この型の情報が欲しいなあ」「この函数の情報が欲しい」「クラス舐めたいぺろぺろ」みたいなことを考えたら、一旦プログラムを書いて、それをGHCiで読み込むなり実行するなりしてやる必要があります。そこで、  `reifier.hs` とか適当な名前のファイルを作って、そこで色々実験することにしましょう。

```haskell
{-# LANGUAGE TemplateHaskell #-}
import Language.Haskell.TH
import Bin

do info <- reify ''String
   runIO $ print info
   return []
```

これを
GHCi
で読み込むと……

```haskell
> :l reifier.hs
[1 of 1] Compiling Main             ( reifier.hs, interpreted )
TyConI (TySynD GHC.Base.String [] (AppT ListT (ConT GHC.Types.Char)))
Ok, modules loaded: Main.
```

シレッと `String` の定義を盗み見ることに成功しました！真ん中の `TyConI`
のところです。

ところで、上の `reifier.hs` ですが妙ですね……。何もないところにいきなり
`do`{.hs}-式が書いてあります。malformed なんじゃないの？

そんなことはありません。実はこれは、

```haskell
$(do info <- reify ''String
     runIO $ print info
     return [])
```

の省略形なのです。そういえばだいぶ昔に「トップレベル宣言では `$( )`を省略出来る」と云うようなことを云っていたと思います。宣言の接合なので
do式全体の値は `Q Dec` を返す必要があります。なので、最後の行で `return []`
としてツジツマを合わせているのです。

では、さっき定義した `Bin` 型クラス
の情報を見てみたいですね。しかし、`Bin` の名前はどうやって指定すればいいのでしょう……？

実は、TH では型名と型クラス名は同じ名前空間・構文木で表現されるという約束があります。なので今のプログラムの

```haskell
do info <- reify ''String
```

を

```haskell
do info <- reify ''Bin
```

に書き換えれば……

```haskell
> :l reifier.hs
[1 of 2] Compiling Bin              ( Bin.hs, interpreted )
[2 of 2] Compiling Main             ( reifier.hs, interpreted )
ClassI (ClassD [] Bin.Bin [PlainTV a_1627406217] [] [SigD Bin.encode (ForallT [PlainTV a_1627406217] [ClassP Bin.Bin [VarT a_1627406217]] (AppT (AppT ArrowT (VarT a_1627406217)) (AppT ListT (ConT Bin.Bit)))),SigD Bin.decode (ForallT [PlainTV a_1627406217] [ClassP Bin.Bin [VarT a_1627406217]] (AppT (AppT ArrowT (AppT ListT (ConT Bin.Bit))) (AppT (AppT (TupleT 2) (VarT a_1627406217)) (AppT ListT (ConT Bin.Bit)))))]) [ClassInstance {ci_dfun = Bin.$fBinInt, ci_tvs = [], ci_cxt = [], ci_cls = Bin.Bin, ci_tys = [ConT GHC.Types.Int]},ClassInstance {ci_dfun = Bin.$fBinBool, ci_tvs = [], ci_cxt = [], ci_cls = Bin.Bin, ci_tys = [ConT GHC.Bool.Bool]},ClassInstance {ci_dfun = Bin.$fBin[], ci_tvs = [PlainTV a_1627406230], ci_cxt = [ClassP Bin.Bin [VarT a_1627406230]], ci_cls = Bin.Bin, ci_tys = [AppT ListT (VarT a_1627406230)]},ClassInstance {ci_dfun = Bin.$fBinBit, ci_tvs = [], ci_cxt = [], ci_cls = Bin.Bin, ci_tys = [ConT Bin.Bit]}]
Ok, modules loaded: Bin, Main.
```

うひゃあ、長々とデータご出て来ましたね！メンバ函数の型や制約、
クラス
の
インスタンス
などの情報が取れているのがわかると思います。

ところでリファレンスを見ると、`Info` 型には

```haskell
VarI Name Type (Maybe Dec) Fixity
```

という構築子がありますね。これはきっと函数・変数の定義をとってくる子にちがいありません。
`Maybe Dec` ということはひょっとすると定義も取ってこれるのかな……！
と思いつつ先程の部分を、

```haskell
do info <- reify 'curry
```

にしてみると……

```haskell
> :reload
[1 of 2] Compiling Bin              ( Bin.hs, interpreted )
[2 of 2] Compiling Main             ( reifier.hs, interpreted )
VarI Data.Tuple.curry (ForallT [PlainTV a_1627409906,PlainTV b_1627409907,PlainTV c_1627409908] [] (AppT (AppT ArrowT (AppT (AppT ArrowT (AppT (AppT (TupleT 2) (VarT a_1627409906)) (VarT b_1627409907))) (VarT c_1627409908))) (AppT (AppT ArrowT (VarT a_1627409906)) (AppT (AppT ArrowT (VarT b_1627409907)) (VarT c_1627409908))))) Nothing (Fixity 9 InfixL)
Ok, modules loaded: Bin, Main.
```

おお、型の情報とか色々とれた！と思いますが肝心の函数定義のところが
`Nothing` ですね……。

ここで残念なお知らせです。`Info`
には定義を入れる部分があるにもかかわらず、現在
**函数定義をとってくる機能は実装されていません**。まあ余り使わないと云えばそれまでですが、悲しいものがありますね……。


閑話休題。いよいよインスタンスの自動生成にはいりましょー。まずは常套手段、適当な宣言を書いて構文木の探りを入れてみましょう。

```haskell
> :{
 runQ [d|
   instance (Bin a) => Bin (Maybe a) where
     encode Nothing = [O]
     encode (Just a) = I : encode a
     decode (I:xs) = let (a, xs') = decode xs in (Just a, xs')
     decode (O:xs) = (Nothing, xs)
   |]
:}
[ InstanceD
    [ClassP Bin.Bin [VarT a_0]]
    (AppT (ConT Bin.Bin) (AppT (ConT Data.Maybe.Maybe) (VarT a_0)))
    [ FunD encode [ Clause [ConP Data.Maybe.Nothing []] (NormalB (ListE [ConE Bin.O])) []
                  , Clause [ConP Data.Maybe.Just [VarP a_1]]
                           (NormalB
                             (InfixE (Just (ConE Bin.I)) (ConE GHC.Types.:) (Just (AppE (VarE encode) (VarE a_1))))) []]
    , FunD decode [ Clause [InfixP (ConP Bin.I []) GHC.Types.: (VarP xs_2)]
                           (NormalB (LetE [ValD (TupP [VarP a_3,VarP xs'_4]) (NormalB (AppE (VarE decode) (VarE xs_2))) []] (TupE [AppE (ConE Data.Maybe.Just) (VarE a_3),VarE xs'_4]))) []
                  , Clause [InfixP (ConP Bin.O []) GHC.Types.: (VarP xs_5)]
                                   (NormalB (TupE [ConE Data.Maybe.Nothing,VarE xs_5])) []]
   ]
]
```

インスタンス
宣言には `InstanceD`
を使えばよいようですね。構文木の引数の順番は大体実際のプログラムで書く順番に対応しているので、それぞれ順に
インスタンス
制約、
インスタンス
宣言本体、メンバ函数などの定義であろうと類推をつけることが出来ます。実際型を見てみると、

```haskell
> :t InstanceD
InstanceD :: Cxt -> Type -> [Dec] -> Dec
```

どうやら読み通りだったようです。
[`Cxt`](http://hackage.haskell.org/packages/archive/template-haskell/latest/doc/html/Language-Haskell-TH-Syntax.html#t:Cxt)
の説明を見ると `Pred` と云うのが出て来て、
クラス
制約を表わす `ClassP` と型同値を表わす `EqualP`
の二つがあることがわかります。上で吐かれた構文木だと `Bin a`
と云う制約が掛かっている様によめます。

次に函数定義の方を読んでいきましょう。`encode` の定義とおぼしき部分だけ切り出すと、

```haskell
FunD encode [ Clause [ConP Data.Maybe.Nothing []]
                     (NormalB (ListE [ConE Bin.O]))
                     []
            , Clause [ConP Data.Maybe.Just [VarP a_1]]
                     (NormalB
                       (InfixE (Just (ConE Bin.I)) 
                               (ConE GHC.Types.:) 
                               (Just (AppE (VarE encode) (VarE a_1)))))
                     []
            ]
```

こうなっているので、 `FunD` の型を見てみます。

```haskell
FunD :: Name -> [Clause] -> Dec
```

これは、関数名と定義節(`Clause`)のリストを取って函数定義の構文木を返すと云うことでしょう。定義節とは、例えば上の例であれば、

```haskell
encode Nothing = [O]
encode (Just a) = I : encode a
```

の各行、つまり `Nothing`に対する場合、`Just` に対する場合の函数の定義それぞれのことです。
GHCi
で Clause の情報を見てみると、

```haskell
> :info Clause
data Clause = Clause [Pat] Body [Dec]
      -- Defined in Language.Haskell.TH.Syntax
instance Eq Clause -- Defined in Language.Haskell.TH.Syntax
instance Show Clause -- Defined in Language.Haskell.TH.Syntax
instance Ppr Clause -- Defined in Language.Haskell.TH.Ppr
```

となっています。データ構築子は `Clause` のみで、引数(のパタン)のリスト、定義の式本体と、いくつかの定義のリストを取るようになっていますね。

最後の定義のリストはなんでしょうか？THの構文木は実際の構文に習って組み立てられていることから類推すると……？実は、これは `where` 節に取られる定義のリストです。

Haskell
を書いていると、時々「あれっ、`where` 節って関数定義全体にわたるんだっけ……？」と思うときがありますが、この構文木を見れば一目瞭然に各パターンに対してだけであることがわかりますね。
このように、THをやっているとHaskellの構文木でアイマイにしか理解していなかったところを理解できると云う副作用があります。わいわい。

さて、必要な道具は大体そろった気がします。あとは今迄の要領で知らない構文木を調べていけばいいわけです。と、云う訳で一般の
インスタンス
導出に移りましょう。

一般的なデータ型をどうやってバイナリにエンコードするか、と云う方法を上の
`Maybe` の例から考えみてましょう。`Maybe` の場合、

```haskell
data Maybe a = Nothing | Just a
```

と云う定義に対して、

1. 一番左の `Nothing` だったら `O` だけでおわり
2. 二番目の `Just a` だったら `I` に続けて `a` をエンコードしたものをくっ付ける

と云う感じの定義になっていました。これをより一般の場合に敷衍してみましょう。たとえば次の型を上のマナーの類推でエンコードするとどうなるか？

```haskell
data WeightTree a = Nil | Leaf Int a | Branch (WeightTree a) (WeightTree a)
```

1. 一番左の `Nil -> [O]`
2. その右の `Leaf i a -> I + O + i` のエンコード + `a` のエンコード
3. 一番右の `Branch t s → I + I + t` のエンコード + `s` のエンコード

こんな感じになりそうです。要は構築子の左から順に、`O`、`IO`、`IIO`、`IIIO`……と区別のつくようにタグを割り振って、それに続けて引数を順番にエンコードして置いていけばよいわけです。そして、最後のタグについては
`O` を付けると冗長なのでそれをとってしまえばいい、と。


こんな原理に従って encode の自動生成まで書いたのがリポジトリの
[`BinTH.hs`](https://github.com/konn/template-haskell-tutor/blob/master/BinTH.hs)
です。

そこから核になる部分を取り出してみましょう。

```haskell
deriveBin :: Name -> Q [Dec]
deriveBin dName = do
  DataD cxts name vars cons _ <- normalizeInfo <$> reify dName
  (cs, pats, exps) <- unzip3 <$> mapM implForCon cons
  let cxts = return $ map (ClassP ''Bin . pure) $ nub $ concat cs
      funs = genFuns pats exps
  return <$> instanceD cxts (appT (conT ''Bin) (appsT $ conT name : map (varT . tvName) vars))
                       [funD 'encode funs]
```

`deriveBin` は `Bin` のインスタンス
にしたい型の名前を取ってその定義を返す函数です。`reify`
で型情報を取得して、自前で定義した `normalizeInfo`
でデータ型の定義に変換しています。変換と云うと大袈裟に聴こえますが、基本的に
`data` 定義と`newtype`定義以外を弾いて、面倒なので結果は `data`
定義のものに統一するように処理をしているだけです(詳しくはソース参照)。

次の行では、下で定義されている `implForCon`
函数にデータ構築子の情報を渡して、各構築子に対するエンコード函数の定義に必要な型制約、引数のパターン、定義本体(のもとになるもの)を取得しています。必要な型制約、と云うのは
`Maybe a` の `a` や `WeightTree a` の `a` などの
型変数
のように、`Bin`
クラス
の
インスタンス
になっていなくては困るものを抜き出してきていると思ってください。

```haskell
implForCon :: Con -> Q ([Type], PatQ, ExpQ)
implForCon con = do
  let (name, typs) = conTypesAndName con
  vars <- replicateM (length typs) $ newName "x"
  let pats = map varP vars
      exps = map varE vars
      expr = [| concat $(listE $ map (appE [| encode |]) exps) |]
  return (filter isVarType typs, conP name pats, expr)
```

自前で定義した `conTypeAndName`
で構築子の名前とその引数型のリストを取り出し、引数の数だけパタンマッチに必要な変数名を
`newName` で用意しています。返値タプルは左から順に、引数に含まれている
型変数
のリスト、パタン、エンコードする式です。

そうして得た情報を基に、制約の生成や函数定義部分の生成を行っているのが
`BinTH.hs` の 14, 15行目です。

15行目で呼んでいる `genFuns`
は、パタンのリストと式のリストを取って、各構築子に対するO,Iのタグを付加した定義節を定義する函数です：

```haskell
genFuns :: [PatQ] -> [ExpQ] -> [ClauseQ]
genFuns []     []     = [ ]
genFuns [p]    [e]    = [ clause [p] (normalB e) []]
genFuns (p:ps) (e:es) =
  clause [p] (normalB $ [| O : $(e) |]) [] : map modifyBody (genFuns ps es)
  where
    modifyBody :: ClauseQ -> ClauseQ
    modifyBody cq = do
      Clause pat (NormalB e) [] <- cq
      clause (map return pat) (normalB [| I : $(return e) |]) []
```

こうして関数定義節本体を得たら、あとは `instanceD`
で包んで返してあげればよいだけです。第二引数の

```haskell
appT (conT ''Bin) (appsT $ conT name : map (varT . tvName) vars)
```

と云うのが少し読みづらいですが、これは型
インスタンス
宣言の部分で、例えば `Either` に対しては

```haskell
Bin (Either a b)
```

の部分にすぎません。呼ばれている `appsT`
と云うのは、このモジュールで勝手に定義した次の便利函数です：

```haskell
appsT :: [TypeQ] -> TypeQ
appsT [] = error "appsT []"
appsT [x] = x
appsT (x:y:zs) = appsT ( (appT x y) : zs )
```

つまり、型のリストを取って、それを順に
適用
してった型を返す函数です。正しくないコードですがこんな感じ。

```haskell
appsT [Either, Int, String] == Either Int String
```

式の構文木に関しては、`Language.Haskell.TH` で[`appsE`](http://hackage.haskell.org/packages/archive/template-haskell/latest/doc/html/Language-Haskell-TH.html#v:appsE)
と云う同様のことをやってくれる便利函数が居るんですが、型に対しては用意されてなかったのでその定義をちょここっとかえて自前で定義してみました。

ここで気付いたと思いますが、パタンでの
適用
と式・型でのそれの構文木は構造が違います。パタンではデータ構築子に引数全てが完全
適用
されていなくてはいけないので

```haskell
ConP :: Name -> [Pat] -> Pat
```

と、構築子の構文木に引数が含まれている形でしたが、式と型に関してはデータ構築子の他にも函数に対する
適用
があったり、
部分適用
があったりするので、

```haskell
ConE :: Name -> Exp
VarE :: Name -> Exp
AppE :: Exp -> Exp -> Exp
```

のように構築子、函数、
適用
がそれぞれ別れた形になっています。型についても同様です。


さて、駆け足＆省略が多い感じではありましたが、一応これで `BinTH.hs`
の解説は終わりです。説明を省いた部分についても読んでいけばどういうことをしているのか大体わかると思います。

実際にここで定義したマクロを使っているところを見てみましょう。
`BinTHTest.hs` を見てください。

```haskell
{-# LANGUAGE TemplateHaskell #-}
import Bin
import BinTH
import Language.Haskell.TH

data Tree a = Nil | Leaf a | Branch (Tree a) (Tree a)
data Unit a = Unit a

data Phantom a = Phantom Int

data Hoge a = Huga [a] | NilHoge

deriveBin ''Tree
deriveBin ''Unit
```

マクロを使っているのは、最後の二行の部分です。これはトップレベル宣言なので接合作用素が省略されていて、

```haskell
$(deriveBin ''Tree)
$(deriveBin ''Unit)
```

の略記法です。ところで、 `Tree` の宣言を `deriveBin`
の後に持ってきて読み込み直そうとすると、

```haskell
/Users/hiromi/haskell/advent/2011/BinTHTest.hs:12:11:
    Not in scope: type constructor or class `Tree'
Failed, modules loaded: Bin, BinTH, TupTH.
```

と怒られます。こんな具合に、TH のマクロを使うには、それに渡す名前がマクロ呼び出しより物理的に前に出て来ている必要があります。

さて。ちょっと、ここで意欲のあるひとのために幾つか課題を挙げておこうと思います[^10]。

1. `encode` の例にならって `decode` 函数の自動生成も実装してください。(難易度：ふつう)
2. 実は、上の型インスタンス制約の処理では上手く動かないところがあります。例えば、

   ```haskell
   data Tree a = Leaf [a] | Branch (Tree a) (Tree a)
   ```
  
   のような型に対してのインスタンス制約の文脈が正しく指定出来ません。克服するにはどうすればいいでしょう？(難易度：至難[^11])

      1. 方針1：面倒なので型引数を全部要求しちゃう
      2. 方針2：`reify` でとれる
          [`ClassInstance`](http://hackage.haskell.org/packages/archive/template-haskell/2.5.0.0/doc/html/Language-Haskell-TH-Syntax.html#t:ClassInstance)
          の情報を利用して文脈を正確に指定するようがんばる

3. 必要なインスタンスが定義されていなかった場合自動的にそれも定義する機能を付ける。(難易度：難)
   1. 方針：`reify` の `ClassInstance` の情報や
      [`isClassInstance`](http://hackage.haskell.org/packages/archive/template-haskell/2.5.0.0/doc/html/Language-Haskell-TH-Syntax.html#v:isClassInstance)
      、
      [`classInstances`](http://hackage.haskell.org/packages/archive/template-haskell/2.5.0.0/doc/html/Language-Haskell-TH-Syntax.html#v:classInstances)
      函数などを使ってみる
       * ただし、2.5.0.0 の classInstances 函数はとても使い辛い


## Template Haskell の落とし穴

ここでは、 THをいじっていると遭遇する落とし穴について解説したいと思います。

### 接合作用素と `($)` が紛らわしい

前も書きましたが接合作用素 `$()` の括弧が省略出来るため、函数
適用演算子`($)` と紛らわしく、プログラムが malformed になることがよくあります。

TH を使うときは、中置演算子の `($)`の前後にはしっかり空白を置くか、一切使わないようにしましょう。接合作用素についても、省略出来る場合でも括弧はしっかり書くようにしましょう[^12]。

### Stage Restriction とのたたかい

前に触れましたが、構文木を接合する際に Stage Restriction
と云う制限がかかります。それは、
**トップレベル接合中で呼び出せる函数は外部から `import` したものだけ**
という制限です。

何故こんな制限があるのかと云うと、コンパイル時に接合に循環が起きないかをチェックするのが技術的に非常にコストがかかるため、このような形になっています。では、ローカルの変数などを渡したい場合はどうすればいいの……？と云うのが当然次に出て来る疑問ですが、その場合は変数の名前を渡してやるように変更すれば、大抵の場合うまくいきます。

どういうことか。簡単な例ですが、

```haskell
multi :: Integer -> ExpQ
multi i = [| (i *) |]
```

のようなマクロを定義したとします。貰った数倍する式を返すマクロですね。これを
GHCi
で読み込んで遊んでみます。

```haskell
> $(multi 2) 3
6
> let a = 2 in $(multi a) 3

<interactive>:1:22:
    GHC stage restriction: `a'
      is used in a top-level splice or annotation,
      and must be imported, not defined locally
    In the first argument of `multi', namely `a'
    In the expression: multi a
    In the expression: $(multi a)
```

一つめの例では単に値を渡しただけなので上手くいきましたが、二つめの例では
Stage Restriction に引っ掛かってしまい通りません。

そこで、multi の定義を次のように変更しましょう。接合の中で引数の n
を読んでいる様に見えて restriction
に引っ掛かりそうですが、式クォート中なのでセーフです。

```haskell
multi :: Name -> ExpQ
multi n = [| ($(varE n) *) |]
```

すると、一番最初の `$(multi 2)`
のような使い方は出来なくなりますが、二番目の例は、

```haskell
> let a = 2 in $(multi 'a) 3
6
```

と、名前を渡してやることでちゃんと動くようになります。或いはこの例の場合はもっと簡単に、

```haskell
multi :: ExpQ
multi = [| \a -> (a *) |]
```

のようにすることも出来ます。まあこれはこの例が簡単だからと云うのもありますが、
Stage Restriction
に引っ掛かった場合は、実値渡しではなく名前渡しで解決出来ないか、と云うことを考えてみると良いでしょう。

ただ、これでも解決出来ないことがあります。例えば、コンパイル時に遺伝的プログラミングをしたくて、合成した構文木の
評価
をしたかったとします。この場合、TH
の機能だけではどうしてもその場で合成した構文木をトップレベル接合に入れてやる必要があるため、上の技法は使えません。

一般に、合成したばかりの構文木を
評価
するようなマクロは TH だけでは書けません。まず TH 自身を TH
で扱うことが出来ないので、メタマクロのようなものもかくことが出来ません。どうしてもやりたい……？その場合は
[hint](http://hackage.haskell.org/package/hint)
パッケージなり
[haskell-src-exts](http://hackage.haskell.org/package/haskell-src-exts)
パッケージなりを調べてみると幸せになれるかもしれませんね。お勧めはしませんが。。。

### マクロがコンパイル出来たからって全てが上手くいくと思うな

THの函数と体力を駆使して次の様なマクロを書いたとします。

```haskell
wtf :: ExpQ
wtf = infixApp (litE (stringL "hello ")) [| (++) |] (conE 'True)
```

GHCi
に実行させてみましょうか。

```haskell
Prelude > :l MyGreatMacro.hs
[1 of 1] Compiling Main             ( MyGreatMacro.hs, interpreted )
Ok, modules loaded: Main.
Main* Prelude> $(wtf)
<interactive>:1:1:
    Couldn't match expected type `[Char]' with actual type `Bool'
    In the second argument of `(++)', namely `True'
    In the expression: ("hello " ++ True)
    In the result of the splice:
      $wtf
    To see what the splice expanded to, use -ddump-splices
```

失敗してしまいました……！？ナンデ！？マクロナンデ！？

と思いますが、そりゃ失敗する筈ですよ。だって、wtfを良くみたら、

```haskell
"hello" ++ True
```

って云う構文木なんですから、そりゃ型が合わないんだから死にます。`Exp`
型の型付けが弱いのでこういうことが起きるわけです。

そういう仕組みなので、マクロで型がちゃんと合うかどうかを自分で見てやる必要があります。

これは余り不便だと云うので、新しく型をパラメタ化した [`TExp` 型を作ったらどうかと云う Proposal](http://hackage.haskell.org/trac/ghc/blog/Template%20Haskell%20Proposal)
もあります[^13]。

大分前の例でも出て来ましたが、引数パターンの名前が被っていたりと云ったのも接合してみるまでエラーがわかないので注意が必要です。

どうしてもどこで食い違いがおこっているのかわからない……。と云うような場合は、マクロを使用しているプログラムをコンパイルする際に `-ddump-splices` オプションを付けてコンパイルすると、構文木の接合が起きるたびにどんな風にそれが展開されたかを表示してくれるので、試してみると良いでしょう。

例えば最後の `BinTH` の例である `BinTHTest.hs` を `-ddump-splices` を付けてコンパイルすると、

```haskell
BinTHTest.hs:1:1: Splicing declarations
    deriveBin 'Tree
  ======>
    BinTHTest.hs:13:1-16
    instance Bin a[a3GR] => Bin (Tree a[a3GR]) where
        { encode Nil = (O GHC.Types.: concat [])
          encode (Leaf x[a3Hr])
            = (I GHC.Types.: (O GHC.Types.: concat [encode x[a3Hr]]))
          encode (Branch x[a3Hs] x[a3Ht])
            = (I
             GHC.Types.:
               (I GHC.Types.: concat [encode x[a3Hs], encode x[a3Ht]])) }
BinTHTest.hs:1:1: Splicing declarations
    deriveBin 'Unit
  ======>
    BinTHTest.hs:14:1-16
    instance Bin a[a3GQ] => Bin (Unit a[a3GQ]) where
        { encode (Unit x[a3HW]) = concat [encode x[a3HW]] }
```

こんな感じでマクロ展開後のコードを表示してくれるようになります。便利でしょ？

#### 結語

長々と＆期限を大幅に破って続けて参りましたこの記事ですが、何とかこの辺で終わりにしたいと思います。……最初のテーブルにはもっと内容があった気がするんですが、きっとそれは気のせいです。こんな記事を何度も読むより、自分で何度も書き直したりしたほうが
TH は身に付くと思うので、是非みなさん楽しんでください。

Happy Template Haskelling and have a nice year！

[^1]: 完成したのはその十日後です。
[^2]: 正確には Haskell 処理系の一種であるGHC で
[^3]: これについては本稿では説明しないので、以前書いた [準クォートでもてかわゆるふわメタプログラミング！ -はてな使ったら負けだと思っている deriving Haskell](http://haskell.g.hatena.ne.jp/mr_konn/20101210/quasiquotes)
を参照してください（サルベージ後註：現在サルベージ中）
[^4]: 厳密にはもっと沢山ありますし、粗く分けるならも少ない分類もありえます
[^5]: リテラルを表わす `Lit`、節を表わす `Clause` や函数本体を表わす `Body`、文脈を示す `Cxt` など
[^6]: 鳴らして嬉しいかどうかは別問題ですが．．．
[^7]: 正確には `Quasi` クラスのインスタンスとなるファンクタですが、`IO` 以外に用いることはほぼないでしょう。
[^8]: ダブルクォートではないことに注意！
[^9]: りーえふぁい、と読むみたいです。具体化するとか云う意味らしい
[^10]: 出題者が答えを知っているとは思わないでくださいね？
[^11]: 出題者が答えを（ry
[^12]: **（2024年からの註）**最近のGHC（GHC 9.2 以降）では、Template Haskell が有効でない文脈で `f $x` みたいに `$` と識別子が隣接していると警告するようになりました。てか、フォーマッタを掛けましょう。
[^13]: **（2024年からの註）**最近の Template Haskell では型付式クォートが実装されています。インタフェースは何度か変遷を経、現在では `Code Q a` が「spliceされると型 `a` の値になる副作用つきマクロ式」の意味になり、この型の値を生成するためのクォート `[|| hoge ||]` があります（左右の `|` が二個になっていることに注意）。
[^14]: **（2024年からの註）** GHCのジェネリクスの機能が十分強力になったので、ここで書いているような例であれば、実は Template Haskell を使わずとも Generics と `DeriveAnyClass` を使って直接 `deriving` 節で導出させるロジックを書くことができます。Template Haskell を使ってもいいですが、TH はクロスコンパイラなどでサポートされていなかったりしますので、こういう例では Generics を使うのが一般的です。ただし、Genericsは途中で中間表現を挟むので、パフォーマンス面を気にする場合はこれが消去されるように腐心する必要があったり、また TH でしか使えない型の情報などでより効率的なインスタンスが導出可能な場合もあるため、Template Haskell で実装を導出させることもよくあります。また、Generics は GADTs や型族に対して使えないため、こうしたデータ型のインスタンスの導出にも TH は有効です。
