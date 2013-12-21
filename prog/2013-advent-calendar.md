---
title: 定理証明系 Haskell
date: 2013/12/20 00:00:00 JST
author: 石井大海
description: C85 の TCUG に寄稿させて頂いた Haskell で定理証明を行う記事の販促記事です。Haskell Advent Calendar 2013 および Theorem Prover Advent Calendar 2013 参加記事。
tag: haskell, theorem prover, 定理証明, 宣伝
---

この記事は [Haskell Advent Calendar 2013](http://qiita.com/advent-calendar/2013/haskell) および [Theorem Prover Advent Calendar 2013](http://qiita.com/advent-calendar/2013/theorem_prover) 二十日目の記事であり、更にTCUGの新刊「[Coqによる定理証明](http://tcug.jp/books/2013-12/)」の[販促記事](#last)でもある。

型システム再考{#type-system-revisited}
====================================
Haskell は静的型付き言語だ。それだけでなく、強力な型推論や表現力の高い型システムを備えている。

型とは何だろうか。

こうした質問に対してよくある答えは、「値の種類を区別するためのタグ」になるだろうか。`Int`{.haskell}型は整数だし、`Bool`{.haskell}型は真偽値で、`[Int]`{.haskell}型は整数値リストを表す型だ。なるほど、値の種類を区別するものに見える。

しかし、この答えは間違ってはいないが、もっと相応しい云い方が出来るだろう。それは、「**型は不変条件である**」というものだ[^1]。この言明は別に私固有の見方というわけではなく、ある程度の型レベルプログラミングをやった事のある人間ならみんな思っているだろう。
`Int`{.haskell}型は「整数である」という不変条件だし、`[Int]`{.haskell}型の値は「要素が全て整数である」という不変条件を満たすリストだ。このような型が不変条件として実際に機能するということが、一般にいう型安全だということだ。実際、C言語やその眷属、あるいは `import Unsafe.Coerce`{.haskell} をしたHaskellのように無制限に cast を許すような言語では、簡単に不変条件が破れて安全性を担保出来なくなる。つまり、異なる型が異なる型として区別されて、簡単に変換出来ないようになっていることが大事なのだ。

不変条件と依存型{#invariants-and-dependent-types}
==============================================
型を「不変条件」と見做せるということを前節では主張してきたが、そこで出している例が `Int`{.haskell} や `[Int]`{.haskell}といった代わり映えしないものばかりだったので今一その旨味がわからなかったかもしれない。というわけで、こうした場合の常套句である**長さ付きベクトル**の例を見てみよう。

Haskellでは、リストの操作関数として `head`{.haskell} や `tail`{.haskell} といった関数が定義されているが、これらは空リストに対して用いると例外が飛んでしまう。型の上では空リストとそうでないリストの区別が付かないからだ。そこで、いっそのこと型に長さの情報を付加してしまったらどうだろうか？という発想で得られるのが長さ付きベクトルである。Haskell における伝統的な方法では、次のようにして定義される：
```haskell
{-# LANGUAGE GADTs #-}
data Z
data S n
data Vector a n where
  Nil  :: Vector a Z
  (:-) :: a -> Vector a n -> Vector a (S n)
```
型レベルで自然数を表現する為に、ダミーの型として `Z`{.haskell} や `S n`{.haskell} を定義し、`Vector`{.haskell} の長さを表すパラメータとして使っているのだ。これらを使えば、より「型安全」な `head`{.haskell} や `tail`{.haskell} を書くことが出来る：
```haskell
tail :: Vector a (S n) -> Vector a n
tail (_ :- as) = as

head :: Vector a (S n) -> a
head (a :- _) = a
```

このように書くことで、空リストを `tail`{.haskell} や `head`{.haskell} に喰わせようとしても、コンパイル時に型エラーとして弾かれるようになる：
```haskell
ghci> Nil
<interactive>:4:24:
    Couldn't match type Z with S n0
    Expected type: Vector a0 (S n0)
      Actual type: Vector a0 Z
    In the first argument of `head', namely `Nil'
    In the expression: head Nil

ghci> tail Nil
<interactive>:5:24:
    Couldn't match type 'Z with 'S n0
    Expected type: Vector a0 ('S n0)
      Actual type: Vector a0 'Z
    In the first argument of tail, namely Nil
    In the expression: tail Nil
```

だが、一般に長さがそこまで自明でないようなリストを扱わなくてはならない時はどうすればよいだろうか？つまり、GHCの側にとっては `Vector a n`{.haskell} 型の値がわたってくるということしかわからないような場合だ。勿論、これが空リストも取り得る場合については、弾かれて当然だ。では、そのリストが空でない事を証明出来るときには、どのようにしてGHCにその事実を教えてやればよいのだろうか？

また、上では `data Z`{.haskell} や `data S n`{.haskell} などと定義して型レベル自然数を定義したが、`Vector`{.haskell} の第二引数は他の値も取り得る。例えば、`Vector Int Bool`{.haskell} や `Vector a ()`{.haskell} のような型も（値は持たないが）Haskell としては合法な型になってしまう。

まず二番目の問題について解決してみよう。問題は、型がちゃんと「型付け」されていないことである。いいかえれば、本来異なる型である筈のものがきちんと区別されていないことが問題なのだ。値に関する不変条件を型で解決したように、型に関する不変条件は「型の型」をつけて解決したい。ちなみに、Haskellでは、「型の型」のことを**種**（*kind*）と呼ぶ。

たとえば、もし値レベルで自然数とそれ以外を区別したいのなら、次のように自然数を表す型 `Nat`{.haskell} を定義してしまえばよい：
```haskell
data Nat = Z | S Nat
```
では、これと同じことを型レベルでも出来ないだろうか？CoqやAgda、idrisといった定理証明系では**依存型**という機構を用いてこれを可能にしている。これらの処理系では型と値の区別は実質ほとんどないといってよく、型のパラメータとして値を取ったり、型それ自身を値として扱うようなプログラムを記述することが出来る。他方、通常のHaskellにおいてはそうしたプログラムを書くことはできない。これは、依存型を導入すると強力な型推論を犠牲にしなくてはならないとか、そういった実用上の設計選択によるものだ。

残念、Haskellでも使えればよかったのに──と諦めるのはまだ早い。完全な依存型ではなく、それをエミュレートすることが出来る機能が最近GHCに実装された。それが**データ型の昇格**および**多相種**である。これらはそれぞれ`DataKinds`{.haskell}および`PolyKinds`{.haskell}言語拡張を指定することにより有効化出来る。

`DataKinds`{.haskell} 拡張は、上のように定義した代数的データ型を型レベルに持ち上げることが出来る言語拡張である。より厳密には、`Nat`{.haskell}型を種レベルに持ち上げた`Nat`{.haskell}種と、その種に属する型コンストラクタ`Z :: Nat`{.haskell}および`S :: Nat -> Nat`{.haskell} がGHCによって自動的に定義されるようになるのだ。これらは全く同型なものであるため、あたかも`Nat`{.haskell}データ型の値が、型レベルに昇格されたように見える訳である。これを使えば、先程のベクトルの例は次のように書き直せる：
```haskell
{-# LANGUAGE GADTs, DataKinds #-}
data Nat = Z | S Nat
data Vector a n where
  Nil  :: Vector a Z
  (:-) :: a -> Vector a n -> Vector a (S n)
```
`Vector`{.haskell} の定義には変更がないが、今まで別個に型として `Z`{.haskell} や `S n`{.haskell} を定義していた部分がただのデータ型の定義に変わっている。しかし、これによって、`Vector`{.haskell}型の第二引数の種は `Nat`{.haskell} だけに限定されるようになっている。それを確かめるには、ghci を使ってその種を確認してみればよい：
```haskell
ghci> :k Vector
Vector :: * -> Nat -> *
```
最初の例を読み込ませてみると `Vector :: * -> * -> *`{.haskell} となることがわかるので、どうやら先程よりもよりしっかりと種が付いていることがわかる。ここで、`*`{.haskell}という種は値を持つ型を表す種である。

`PolyKinds`{.haskell} 拡張は、その名の通り多相的な種を持つ型を許すようにするものだ。この時点においてはまだ余り有難味がわからないかもしれないが、データ型の昇格をより総称的に扱うには、種に多相性を許したほうが統一性が取れるのだ[^2]。

不変条件、証明、Singleton {#invariants-proofs-and-singletons}
==========================================================
さて、これで型の不変条件を種として表現するための道具は揃った。残る問題は、不変条件に関する証明をどのようにして記述するかということだ。一般に、型レベルの値についてその性質を証明しようと思ったら、その値に対するパターンマッチを行いたくなる。しかし、Haskellは型を値に「降格」するための機構は標準では用意されていない。`DataKinds`{.haskell}で可能になるのは値→型方向の「昇格」だけだ。何か手はないだろうか？

これを解決するのが**Singleton パターン**だ。Singleton の基本的な考え方は、型と一対一に対応する同型なデータ型を作ってやることだ。たとえば、`Nat`{.haskell} のSingleton は次のようになる：

```haskell
data SNat n where
  SZ :: SNat Z
  SS :: SNat n -> SNat (S n)
```

データ型の定義をつぶさに分析すれば、`SNat n`{.haskell} 型の値は唯一つだけ存在して、それは`n`{.haskell}と同型になっていることがわかる。例えば `SNat Z`{.haskell} に属する値は `SZ :: SNat Z`{.haskell} だけであり、$n = 3$ の時は `SS (SS (SS SZ)) :: SNat (S (S (S Z)))`{.haskell} が唯一の `SNat (S (S (S Z)))`{.haskell} の要素である、といった具合だ。

シングルトン自然数に対する加法や順序関係なども定義することが出来る：

```haskell
{-# LANGUAGE TypeOperators, TypeFamilies #-}
type family   (+) (n :: Nat) (m :: Nat) :: Nat
type instance Z   + m = m
type instance S n + m = S (n + m)

(%:+) :: SNat n -> SNat m -> SNat (n + m)
SZ   %:+ m = m
SS n %:+ m = SS (n %:+ m)

data (<=) n m where
  LeqZero :: Z <= m
  LeqSucc :: n <= m -> S n <= S m
```

こうした算術や述語を使った証明を書くためには、先程いったように型レベルの値`n` に対するパターンマッチが必要になる。しかし、各 `n`{.haskell} に対して `SNat n`{.haskell} 型の値を返す型クラスを帰納的に定めてやれば、型に対するパターンマッチを実現出来る：
```haskell
class Singleton (n :: Nat) where
  sing :: SNat n

instance Singleton Z where
  sing = SZ

instance Singleton n => Singleton (S n) where
  sing = SS (sing :: SNat n)
```

これを使えば、自由に自然数の性質を証明してやることが出来るようになる。

宣伝 〜買ってくれたら、それはとっても嬉しいなって〜 {#last}
=================================================
以上のようにして、Haskell においても依存型を用いたプログラムや定理証明を行うことが出来る。とはいっても、Haskell は定理証明系ではないので、普通に定理を証明したいだけであれば Coq や Agda を使った方がよい。勿論、頑張れば Haskell でも Coq や Agda の代わりを勤めることは出来ないでもないのだが。寧ろ、Haskell で定理を証明する必要がある時というのは、依存型を用いたプログラムにおいて、実行時の安全性を担保したいときである。

ここではかなり駆け足で Haskell でも定理の証明が可能であることを説明してきた。より詳しい方法に関しては、C85で頒布するTCUG『Coq による定理証明』第三章でかなり細かく説明した。なので、興味を持たれた方は是非お買い求め頂きたい。Haskellで依存型プログラミングを実現し定理証明系として用いる方法を解説した、恐らく唯一の日本語文献であろうと思う。

ここに書かなかったこととしては、例えば Haskell で等式証明を快適に書くための技法についても紹介している。例えば、この本を読むと次のようにして「$S (n + m) = n + S m$」ということを証明出来るようになる：

```haskell
plusSR :: SNat n -> SNat m -> S (n :+: m) :=: n :+: S m
plusSR n m =
  start (sS (n %+ m))
     === (n %+ m) %+ sOne  `because` sAndPlusOne (n %+ m)
     === n %+ (m %+ sOne)  `because` symmetry (plusAssociative n m sOne)
     === n %+ sS m         `because` plusCongL n (symmetry (sAndPlusOne m))
```

本書は他にもCoqで証明を書くための技がいっぱい詰まっているので、Haskellに興味はないが定理証明には興味があるという方も是非[^3]。そんなワクワクが詰まった書籍の詳細は以下の通り：

書名
:    『[Coq による定理証明](http://tcug.jp/books/2013-12/)』（第三章をわたしが書きました；[サンプル](http://tcug.jp/books/2013-12/example.pdf)）
サークル名
:    [Tsukuba Coq Users' Group](http://tcug.jp/)
頒布イベント
:    コミックマーケット85（ぼくはいません）
配置
:    12/31 火曜日(3日目) 西し33-a
価格
:    未定（800円程度）

著者（@[pi8027](http://twitter.com/pi8027)さんとか@[pirapira](http://twitter.com/pirapira)さんとかわたしとか）に直接云って貰っても、在庫を持ってれば多分売れます。買ってやってください m(_ _)m


[^1]: ここでいう「不変条件」というのは、「関数呼び出しの前後で不変な条件」だけではなく、事前条件や事後条件も乱暴含めている。

[^2]: 他にも、多相種によって今までわかれていた `Typeable, Typeable1, Typeable2, ...`{.haskell} といった型クラスを一つのクラスとして統一したり、更には `Monad`{.haskell} と `Monoid`{.haskell} を一つのクラスで扱えるようにもなったりする。

[^3]: そういえばこれは [Theorem Prover Advent Calendar 2013](http://qiita.com/advent-calendar/2013/theorem_prover) の記事でもあった。
