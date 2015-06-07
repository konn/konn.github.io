------
title: さらば愛しき JavaScript —— 愛と欲望の果てに Haskell は fay と出逢う。
author: 石井大海
date: 2012/12/25 14:33:00 JST
description: Haskell のプログラムからJavaScriptを自動生成する Fay という言語処理系についての紹介記事。[Haskell Advent Calendar 2012]( http://partake.in/events/45a01d39-af5e-42f1-91c7-e8fcc91db244 ) の七日目の記事。
tags: Haskell,Fay,JavaScript,Web
------
これは [Haskell Advent Calendar 2012]( http://partake.in/events/45a01d39-af5e-42f1-91c7-e8fcc91db244 ) の七日目の記事です[^1]。

[^1]: 日付がおかしいと云う向きもあろうが、それは私の正格性解析器が壊れていたせいであり、私自身の責任ではない。

導入：JavaScript Problem
----------------------------------
[JavaScript Problem](http://www.haskell.org/haskellwiki/The_JavaScript_Problem) と呼ばれるウェブ開発上の問題があり、手短にまとめてそれは概ね次のようなパラドックスの形を取る。

* JavaScript はクソである。
* JavaScript は必要である。

何を云うのだ、JavaScript は決してクソなどではない、そんなことを云う Haskell の方こそがクズなのだと云う方も居られるだろう。しかし、これを云ったのは私ではないし、私はフレーム合戦を始めるためにこの文章を書こうという訳では勿論ない。

上の要約を存在命題の形に言い直せば、幾分わかりやすくなるかもしれない。

> JavaScript の代わりに Haskell でプログラムを書けたら幸せになる人種は、確かにこの世に存在する。

そして、この記事はそのような人種に向けてかかれたものだ。

Fay とは？
--------------
JavaScript Problem に対する一つの解として、ここでは [Fay](http://fay-lang.org/) を採り上げる。公式サイトによれば、Fay の特徴は概ね次の通りだ。

* Haskell の構文的・意味論的サブセット
	* 静的型付き、遅延評価、純粋
* JavaScript にコンパイルされる
* JS と互換性のある基本的データ型のサポート
* 簡単な FFI 機構
* GHC を用いた型検査

つまりは、「JavaScript の代わりにほぼ Haskell を用いることが出来る」と云うことだ。この**ほぼ**と云うのがどの辺りにあるのかを明らかにするのが、本稿の目的の一つだ。

以下では、[有限オートマトンシミュレータの例](http://konn-san.com/haskell/automaton/)を使って、Fay についての簡単な紹介をしていく。[GitHub にソースコード](https://github.com/konn/fay-automaton-demo/blob/fb4528c424977891fe6ae726f665e3feb35d33f3/Automaton.hs)がある。一通り遊べるような段階まで作ったつもりであるので、以下の記事を読むのがかったるいと云うような場合には、こいつで日がな一日ひたすら遊び倒すと云うのも一興ではないだろうか。

### 余談：オートマトンとは
オートマトンと云うのは正規表現のことだと思って頂いて間違いはない。間違いはないのだが、まあそうは見えないだろう。噛み砕いて、菱形の頂点から出発して入力された文字に従って枝を辿っていったとき、最後に二重線で囲まれた頂点に居れば勝ち、そうでなければ負け、と云うようなゲームだと思えばよい。例えば、初期状態で "10010" を入力して "Run" を押せば、左下から出発して「0→1→2→4→3→0」と辿ることになり、最後に二重四角に辿り着いているので勝ち。"1011001" なら「0→1→2→5→5→4→2→5」となりただの丸で終わるので負け。シミュレータで実行した場合、緑色のマスが今いる頂点で、最終的に赤で止まれば負け、青で止まれば勝ちとなる。適当に文字列を入力したり、図を組替えたりして遊ぶ内にどういうものかうっすらとでもおわかり頂けるのではないかと思う。

閑話休題。そろそろオートマトンの記事なのか Fay の記事なのか解らなくなってきたところで、本編に移ろう。

Fay の概観
--------------
まずは

```
$$ cabal install fay
```

として `fay` をインストールしよう。

どんな感じでプログラムが書けるのかと云うところに関しては、[ソースコード](https://github.com/konn/fay-automaton-demo/blob/master/Automaton.hs)の方を見て貰うのが早いと思う。至って普通の Haskell プログラムに見える。これを `fay` コマンドに喰わせると、[このような JavaScript](https://github.com/konn/fay-automaton-demo/blob/fb4528c424977891fe6ae726f665e3feb35d33f3/Automaton.js) が生成されて、あとはこれを HTML で読み込めば良い。

Fay を使った開発のワークフローとしては、

1. お気に入りのエディタを起動する
2. 一見 Haskell っぽいプログラムを書く
3. GHCi に読み込ませて型検査をする
4. `$$ fay Hoge.hs` などとして JavaScript に変換する
5. ブラウザや Node.js を使って動作を検証するre

と云う流れになる。GHCi は型検査にしか使わないところが要なのだが、詳細は後程ということにして、早速コードを見ていこう。まず大まかにコードの設計について紹介しよう。主なデータ型の意味は以下の通り。

[`Trans`{.haskell}](https://github.com/konn/fay-automaton-demo/blob/fb4528c424977891fe6ae726f665e3feb35d33f3/Automaton.hs#L626)

:	状態遷移を表す。図で云う矢印。遷移前の状態(`transFrom`{.haskell})、入力、遷移後の状態を持つ。

[`State`{.haskell}](https://github.com/konn/fay-automaton-demo/blob/fb4528c424977891fe6ae726f665e3feb35d33f3/Automaton.hs#L624)

:	内部状態を表す。図で云う菱形や丸の部分。`Int`{.haskell} の別名。

[`Automaton`{.haskell}](https://github.com/konn/fay-automaton-demo/blob/fb4528c424977891fe6ae726f665e3feb35d33f3/Automaton.hs#L635)

:	オートマトンを表す。状態遷移のリスト(`transs`{.haskell})、図では菱形で表される初期状態(`initial`{.haskell})、二重線で囲まれた受理状態のリスト（`accepts`{.haskell}）を持つ。

[`AutomatonState`{.haskell}](https://github.com/konn/fay-automaton-demo/blob/fb4528c424977891fe6ae726f665e3feb35d33f3/Automaton.hs#L587)

:	オートマトンのレイアウトや、選択中の頂点・辺の情報、実行中の場合は現在の状態などを持つ。

では早速最初の方から見ていこう。

```haskell
{-# LANGUAGE EmptyDataDecls, NamedFieldPuns, NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards                                   #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns  #-}
module Automaton (main) where
import Language.Fay.FFI
import Language.Fay.Prelude
import MyPrelude
```

はおまじないみたいなものだ。Fay では `Prelude`{.haskell} の代わりに `Language.Fay.Prelude`{.haskell} を使うので、`NoImplicitPrelude`{.haskell} 言語拡張を用いる必要がある。また、`EmptyDataDecls`{.haskell} 拡張はJavaScript 側のデータに対応する型を作るのに使われる[^2]。どういうことか。例えば [680 行目](https://github.com/konn/fay-automaton-demo/blob/fb4528c424977891fe6ae726f665e3feb35d33f3/Automaton.hs#L680)以降を見てみよう。

[^2]: 他の言語拡張については、書くのが楽だから指定してあるだけだ。Fay では全ての言語拡張が使える訳ではないので注意。

```{.haskell}
data Element
instance Foreign Element
instance Show Element
```

ここで、Element は JavaScript の DOM （ないしは jQuery）API での HTML の要素を表わすデータ型だ。Haskell 側ではこの子の定義をしらないので、型の名前だけ作って、中身は空にしている。JavaScript では関数に型が付いていないので、存在しない関数を呼ぶと実行時エラーになるが、こうして Fay の側で型を分けておけばそういったエラーをある程度排除出来る。

`Foreign`{.haskell} は Haskell と JavaScript とやり取りが出来る、と云う印のようなものと思っておけばよい。`Show`{.haskell} インスタンスの関数定義がないように見えるが、これは通常の `Show`{.haskell} クラスとは若干違うもので、JavaScript の側で印字出来る、ということを表しているにすぎない。印字は JavaScript で行われるので、一度JavaScript にコンパイルされてしまえば、全ては Object なので自然に印字出来るから、`show`{.haskell} 関数などを実装する必要はない。そうそう、Show の結果は JavaScript の方式に則ってなされる。そもそも `Fay.Prelude`{.haskell} では `Show`{.haskell} のメンバ関数はエクスポートされていない。GHC であるていどの Printf デバッグがしたいと思ったら、`deriving`{.haskell} 節を使えばよい。

さて、`Element`{.haskell} と云う型を定義したからには、それを弄る DOM 関数を定義したい。それには Fay の ffi 機構を使えば簡単に出来る。例えば、イベントハンドラを登録する `bind`{.haskell} という jQuery の関数があるが、これを呼び出す関数 `bind`{.haskell} は次のように実装出来る（[690行目](https://github.com/konn/fay-automaton-demo/blob/fb4528c424977891fe6ae726f665e3feb35d33f3/Automaton.hs#L690)）。

```{.haskell}
bind :: Element -> String -> (Event -> Fay Bool) -> Fay ()
bind = ffi "%1.bind(%2, %3)"
```

`ffi`{.haskell} 関数は、GHC 側から見れば実体としては `undefined`{.haskell} と同じだ。これが意味を持つのは fay に処理させた時で、型注釈から引数の数を推定して、`%1`, `%2`, `%3` などのプレースホルダに入れた状態の JavaScript を生成してくれる。関数や文字列なども自動的に変換してくれる。`Fay`{.haskell} は `IO`{.haskell} モナドの代わりのようなもので、副作用を伴うような操作はすべて `Fay`{.haskell} モナドに包まれている。だから、`main`{.haskell} 関数の型も、ここでは `Fay ()`{.haskell} になっている。`bind`{.haskell} の使用例は例えば、`run`{.haskell} 関数を見てみよう（[16行目](https://github.com/konn/fay-automaton-demo/blob/fb4528c424977891fe6ae726f665e3feb35d33f3/Automaton.hs#L16)）。

```{.haskell}
run :: Event -> Fay Bool
run _ = do
  -- set up canvas
  canvas <- jQuery "canvas"
  cxt <- flip getContext "2d" =<< getIndex canvas 0
  mps <- newRef defAutomatonState
  renderAutomaton cxt mps
  bind canvas "mousedown" (onMouseDown mps cxt)
  bind canvas "mousemove" (onMouseMove mps cxt)
```

こんな感じに、本当に普通の関数のように呼べる。呼ばれている `onMouseDown`{.haskell} 関数を見てみると、

```{.haskell}
onMouseDown :: Ref AutomatonState -> Context -> Event -> Fay Bool
onMouseDown rps cxt ev = do
  ps <- readRef rps
  pos <- getMousePos ev
  let state = getStateAt ps pos
      trans = getTransAt ps pos
  if not (null trans)
    then
      if mouseState ps == TransSelected trans
      then setMouseIdle rps
      else setTransSelected trans rps
    else
      case state of
        Just q  ->
          if mouseState ps == StateSelected q
          then setMouseIdle rps
          else setMouseState rps (PointAtState q)
        Nothing -> setMouseIdle rps
  renderAutomaton cxt rps
  return False
```

こんな感じになっていて、これも普通の Haskell のコードそのものだ。`Ref`{.haskell} と云うのが出て来ているが、これは可変参照を表す型で、さっき出て来た `Foreign`{.haskell} のインスタンスの値を仕舞っておく事が出来る。`Ref`{.haskell} は Fay の側で提供されている訳ではなく、こちらでその API を呼び出してやる必要がある。その辺りの実装は [647行目](https://github.com/konn/fay-automaton-demo/blob/fb4528c424977891fe6ae726f665e3feb35d33f3/Automaton.hs#L647)以降にある。

```{.haskell}
data Ref a
instance Foreign a => Foreign (Ref a)

-- | Make a new mutable reference.
newRef :: Foreign a => a -> Fay (Ref a)
newRef = ffi "new Fay$$$$Ref(%1)"

-- | Replace the value in the mutable reference.
writeRef :: Foreign a => Ref a -> a -> Fay ()
writeRef = ffi "Fay$$$$writeRef(%1,%2)"

-- | Get the referred value from the mutable value.
readRef :: Foreign a => Ref a -> Fay a
readRef = ffi "Fay$$$$readRef(%1)"
```

Fay では `State`{.haskell} や `Reader`{.haskell} は使えないので、この `Ref`{.haskell} を使って、状態を引き回すことになる。他の関数を見ると、アプリの内部状態や canvas の文脈を引数として引き回しているのが判ると思う。

Fay では、Haskell のデータ型と JavaScript のデータ型をシームレスに連携させる事が出来る。文字列・配列・数値型などは Haskell のをそのまま使える。例えば、矢印の描画位置を計算している [`getTransShape`{.haskell} 関数](https://github.com/konn/fay-automaton-demo/blob/fb4528c424977891fe6ae726f665e3feb35d33f3/Automaton.hs#L437)を見てみよう（438行目）。

``` {.haskell}
getTransShape :: AutomatonState -> Trans -> Maybe TransShape
getTransShape AutomatonState{..} Trans{transFrom = src, transTo = targ} =
  case (lookup src stateMap, lookup targ stateMap) of
    (Just p0@(x, y), Just p1@(x', y')) ->
      if p0 == p1
      then Just (Arc p0)
      else let theta = if x <= x' then atan ((y-y') / (x-x')) else pi + atan ((y-y') / (x-x'))
      in if any (\t -> transFrom t == targ && transTo t == src) (transs automaton)
      then Just $$ Line (p0 %+ stateRadius %* angle (theta + pi/8))
                       (p1 %- stateRadius %* angle (theta - pi/8))
      else Just $$ Line (p0 %+ stateRadius %* angle theta)
                       (p1 %- stateRadius %* angle theta)
    _ -> Nothing
```

ここでやっているのは、

1. 自分自身への矢印は円形を描く。
2. 反対向きの矢印がなければ、頂点の半径の分だけ間を開けて線を引く。
3. もし反対向きの矢印があれば、更に15度ずつズラして線を引く。

という計算だ。`atan`{.haskell} や `pi`{.haskell} を使って幾何の計算をしている。これを GHCi で読み込んで実行しようとすると、

```haskell
ghci> getTransShape defAutomatonState (Trans 0 '0' 1)
Just (Line (*** Exception: Language.Fay.FFI.foreignFay: Used foreign function not in a JS engine context.
```

と叱られる。これは、内部的には `atan`{.haskell} や `pi`{.haskell} は `ffi`{.haskell} 関数（の仲間）を使って定義してあるためで、これらのデバッグをしようと思ったらブラウザや Node.js を使うか、或いは一旦 `import Language.Fay.Prelude`{.haskell} をコメントアウトして、`Prelude`{.haskell} を読み込むようにする必要がある。必要な計算がわかっている場合は、最初は GHC だけで開発して、最終的に Fay に合うように書き換えると云う手段も取れるだろう。

データ型の変換で注意する必要があるのが、文字列の取り扱いだ。Haskell のリストはリンクドリストに変換されていて、String は文字列型にマップされている。この辺りの齟齬が出て来るのは、例えば文字列を `map`{.haskell} で書き換えたり、文字を集めてリストとして文字列を作ったときだ。例えば、`setTransSelected`{.haskell} 関数を見てみよう（[248行目](https://github.com/konn/fay-automaton-demo/blob/fb4528c424977891fe6ae726f665e3feb35d33f3/Automaton.hs#L248)）。

``` {.haskell}
setTransSelected :: [Trans] -> Ref AutomatonState -> Fay ()
setTransSelected ts asRef = do
  as <- readRef asRef
  writeRef asRef as { mouseState = TransSelected ts }
  hideInspectors
  expose =<< jQuery "#trans-inspector"
  [tFrom, tInps, tTo] <- mapM jQuery ["#trans-info-from", "#trans-info-inputs", "#trans-info-to"]
  setValue tFrom (show $$ transFrom $$ head ts)
  setValue tTo (show $$ transTo $$ head ts)
  setValue tInps =<< arrToStr (map transAlphabet ts)
```

これは、クリックされた所にある矢印のリストを受け取って、それらを選択状態にする関数だ。選択したらその情報をテキストフィールドに反映する必要がある。`transAlphabet :: Trans -> Char`{.haskell} と云う型なので、矢印（`Trans`{.haskell}）のリストに対して `map transAlphabet ts`{.haskell} としてやれば、文字のリストが得られて、Haskell ではこれが文字列になる訳だが、JavaScript レベルではこれは **文字の配列** であって文字列ではない。なので、 `arrToStr`{.haskell} と云う便利関数を定義して、これで変換している。Haskell 上では型があっているので `arrToStr`{.haskell} がなくても検査は通るのだが、最後の行を

```haskell
  setValue tInps (map transAlphabet ts)
```

などとすると、`abc` と表示されるべきところ、 `[Object object]` のような形になってしまう。因みに、`arrToStr`{.haskell} は

```haskell
arrToStr :: [Char] -> Fay String
arrToStr = ffi "%1.join('')"
```

と云う風に `ffi`{.haskell} を使って `join`{.javascript} を呼び出す形で定義している。

あとのプログラムの詳細については、どちらかと云うと jQuery の使い方や幾何計算の説明になってしまうので、プログラムについてはこの辺りにしておく。


Fay を使ってみて
---------------
以上を踏まえて、Fay を使ってみた感想を書いてみたい。

Fay は基本的には Haskell のサブセットなので、普段 Haskell を書くように Fay を書けば問題はない。これは非常に嬉しい。しかし、サブセットである悲しさとして、以下のような違いがある。

### `Prelude`{.haskell} の代わりに `Language.Fay.Prelude`{.haskell} を使う
`Prelude`{.haskell} 関数とほぼ同等か、それ以外にも `Data.List`{.haskell} や `Data.Maybe`{.haskell} などから引っ張ってきた関数もエクスポートされているが、やはり足りないので、必要な関数はコピペしてやる必要がある。
上の例では、[MyPrelude.hs](https://github.com/konn/fay-automaton-demo/blob/master/MyPrelude.hs)にそのような欠けている関数のコピペ群がある。

また、`Read`{.haskell} クラスもちゃんと動かないので、数値などをパーズしようと思ったら、[`parseInt`{.haskell} 関数](https://github.com/konn/fay-automaton-demo/blob/master/MyPrelude.hs#L45) のように、ffi でJS の対応するパーズ関数を読んでやる必要がある。

ローカルのモジュールは読み込むことが出来るので、頻繁に使うような関数は、`MyPrelude.hs` みたいな形で切り出して用意しておくと良いだろう。

### 外部パッケージに依存出来ない。
上でもちらっと触れたが、全てを JavaScript にコンパイルする都合上、`Language.Fay.*`{.haskell} のモジュールと、ローカルのモジュール以外の外部ライブラリのモジュールはインポート出来ない。外部ライブラリと云うのは、`base` パッケージなど基本的なものも含めてインポート出来ない。これは割と不便だ。Monadic だったり Applicative でないものに関しては、上で云ったようにコピペすればいいが、では `Monad`{.haskell} は……詳細後述。

### モナドはほぼ `Fay`{.haskell} 限定
現時点では、`Fay`{.haskell} 以外のモナドは実質使えない。

Fay で提供されている `(>>=)`{.haskell}, `return`{.haskell} などは全て `Fay`{.haskell} モナドに特化した形に書き換えられている。`do`{.haskell} 構文はどのモナドに対しても使えるのだが、`return`{.haskell} が使えないので余り意味を成さない。

この制限は正直かなしかった。特に、今回のように `canvas`{.haskell} を引き回して描画命令を出す必要がある場合、`Reader`{.haskell} モナドが使えないと正直書きづらくてしょうがないし、`Maybe`{.haskell} も `lookup`{.haskell} を `>>=`{.haskell}} でチェーンしたり、`(,) <$$> lookup a hoge <*> lookup b fuga`{.haskell} のような書き方が出来ないのは割と辛いものがある。`Reader`{.haskell} や `State`{.haskell} は諦めて `Ref`{.haskell} を噛ませて引数として引き回すしかない。

### GHC で型検査は出来るが、実行は出来ない関数がある。
前節でも触れたが、 GHC では実行出来ない関数がある。主に数値計算や文字列処理の辺りだ。これらは ffi を用いて実装されていて、GHC 内部では `undefined`{.haskell} 扱いなので、こういった操作を行おうとすると失敗する。したがって、ブラウザで実行しながらがんばって printf デバッグをするか、或いは一旦 `Language.Fay.Prelude`{.haskell} をコメントアウトして `Prelude`{.haskell} を読み込むなどして動かしてみて試すしかない。間違っても吐き出された JS のソーコードを読もうと思ってはいけない。というか読めない。`$$ fay Automaton.hs --pretty`{.sh} と云う具合に `--pretty` オプションを付ければちょっとは読み易くはなり、公式サイトのほうにシンボルの読み方等はあることにはある。

このような関数をデバッグするのは、ちょっと慣れないと大変だ。

### 使えない言語拡張がある。
サブセットなので、全ての言語拡張を使える訳ではない。 Template Haskell 系の拡張はまあ残念ながら全滅。PatternGuards も無理だった。また、NamedFieldPuns などは使えるようだが、レコード構文がネストすると上手くパーズ出来ないようだ。DoAndIfThenElse とかも上手く動かない。あと、

```haskell
case hoge of
  Fuga a | a `elem` target -> ....
```

のような構文もサポートされていないらしい。どの構文がサポートされていてどれがサポートされていないかは、`fay`{.sh} コマンドにソースを通すと判るので、適宜それを使わないように書き換えていく必要がある。

他の候補
-------
ここでは「JavaScript の代わりに Haskell を使いたい」と云う欲望の形で JavaScript Porblem を紹介したが、ようは「リッチな型システムや柔軟性といったものを追加した Better JavaScript がほしい」と云う要望としても表現出来る。というかそうした立場のほうが一般的だ。そうした視点も加味して、以下のような選択肢もあるらしい[^3]。

[^3]: らしい、と云うのはまだ試していないので。

[Roy](http://roy.brianmckenna.org/)

:	型推論や函数型言語的な要素を含んだ言語。

[CoffeeScript](http://coffeescript.org/)

:	Ruby のような感じで JavaScript を書けるらしい。

[TypeScript](http://www.typescriptlang.org/)

:	JavaScript のような構文に、型注釈や型推論を追加した言語。

[Haxe](http://haxe.org/)

:	型推論や subtyping などの強力な型システムやクロージャなどを備えた言語。JavaScript 以外にも swf や C++、PHP へのコンパイルも可能なもよう（2012/12/26 追記）。

[js_of_ocaml](http://ocsigen.org/js_of_ocaml/)

:	Haskell と双璧をなす函数型言語、[OCaml](http://caml.inria.fr/) のバイトコードから JavaScript へのコンパイラ（2012/12/26 追記）。

もっと Haskell っぽい選択肢、となると以下のようなものがある。

[GHCJS](https://github.com/ghcjs/ghcjs)

:	JavaScript を吐くように改造された GHC。やばい。まだ未完成ではあるが、完成すれば Fay よりも凄いだろう。GHC を本当に改造したバージョンと、GHC API を使うバージョンの二つがあるが、インストール作業がかなり時間が掛かる。残念ながら時間がなかったので筆者はインストールを断念した。今後に期待。

[UHC](http://uu-computerscience.github.com/uhc-js/)

:	GHC に次ぐ有力な Haskell Compiler、UHC の JavaScript バックエンド。

[Elm](http://elm-lang.org/)

:	Haskell にかなり近い構文を持った、函数型リアクティヴ言語。まだちゃんと試してみたことはないが、より JavaScript の機能を使い易いように注力しているようだ。意味論的には正格言語で、遅延評価はないので、厳密なサブセットではない。

あるいは、JavaScript を生成するようなライブラリも沢山ある。それを使うのも一つの手か。

結論
----
Fay を使えば、ほとんど JavaScript を書かずに Haskell で済ませることが出来る。
ほとんど、と云うのは、まだ Fay 上に構築された jQuery などのラッパーが十分に開発されていないため、自分で `ffi`{.haskell} 関数で JavaScript のコード断片を書く必要があるということだ。コード断片を書く必要があると云うことは、JavaScript について調べなくてはならず、そこが揃っていないと結局 JavaScript を書いているような気分になってしまう。まあ、この辺りはライブラリが充実してくれば大丈夫だろう。というわけなので、表題の「さらば愛しき JavaScript」と云うのは嘘だ。おさらばは出来ない。というかそこまで愛しくなかった。

[2012/12/26 追記] まだ Hackage に登録はされていないようだが、GitHub に [fay-jquery](https://github.com/faylang/fay-jquery) というレポジトリーがあった。まだ設計を試行錯誤している最中のようだが、完成すればかなり便利だろう。GitHub には、他にも [DOM API のラッパー](https://github.com/faylang/fay-dom)もあった。また、今回の例では canvas のラッパーを頑張ってこちらで書いたが、GitHub の examples 以下にも [canvas を有効活用している例](https://github.com/faylang/fay/blob/master/examples/CodeWorld.hs)があって、ここから持ってくるのもよいかもしれない。

ただ、Fay の FFI インターフェースはシンプルだが使い勝手がよいと思う。きちんと引数の数が合っているかも `fay` コンパイラがチェックしてくれるし。`ffi`{.haskell} でしっかり API を組んでおけば、あとはそれを使うだけでよい。もっとも、ここで設計をミスると JavaScript の方でエラーが起きるので注意が必要だ。また、特定の JavaScript 環境に依存しない形になっているので、お気に入りの JavaScript ライブラリと組み合わせて使うことが出来る。[Yesod Blog](http://www.yesodweb.com/blog/2012/10/yesod-fay-js) では、 AngularJS と組み合わせる方法が紹介されていた。

あと、やはり Fay 以外のモナドも自由に扱えたほうが嬉しい。何度も云うようだが、ここで `Reader`{.haskell} モナドが使えればすっきり書けるのに……と云ったような場面によく遭遇した。また、`ffi`{.haskell} を使っている関数でも、基本的な Prelude 関数とかは GHC で読み込んで実行出来ると、手軽にデバッグ出来ると嬉しいなあと思う。JavaScript コンソールや Node.js に読み込ませる方法もあるが、Haskell で定義した関数が呼べたり呼べなかったりするのでちょっと難しい。

また、ここでは紹介出来なかったが、[yesod-fay](http://hackage.haskell.org/package/yesod-fay) パッケージを使うと、[Yesod](http://www.yesodweb.com/) アプリとブラウザ間で Ajax 通信を行う時に、どちらの処理も Haskell で書けて、しかもデータ型を共有出来るというスゴイ旨みがある。他の Yesod の要素と同じように継続ビルド環境もある。詳細はパッケージの説明を参照して貰いたい。Yesod の開発者である Michael も Fay と ghcjs に大きな期待を寄せているようだ。

私も同じ気持ちで、GHCJS が発展してくれたらこんなに嬉しいことはない。まずはインストールが簡単になってほしい。もちろん、GHCJS を新たに入れ直すのとかは面倒なので、簡単なものは Fay を使えばいいと思う。

という訳で、結論としては JavaScript とおさらばすることは出来ないが、Better JavaScript としての Haskell[^4] としては、そろそろ役者が揃って実用出来る段階に入ってきていると思う。今回のプログラムは900行弱だが、コンパイルした JS もあまりもっさりせず軽快に動く。Haskeller 諸氏におかれましては、何か JavaScript を書く必要に迫られた際に、是非 fay を試してみてほしい。

それでは。Happy Haskelling!

[^4]:  なんだか牛刀めいた表現だなあ。

