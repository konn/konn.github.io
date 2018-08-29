---
title: Rustで型レベルPeano自然数とサイズ付きベクトルを書いた
author: 石井大海
description: |
  あるていど強い型システムを持った言語を見ると、**型レベルペアノ自然数**と**サイズ付きベクトル**を書きたくなる、というのは**人類普遍の欲求**だと思います。
  という訳で欲求に忠実であることとし、**型レベルPeano自然数とサイズ付きベクトルRustで実装**しました。実用性？皆無です。
tag: Rust,型レベルプログラミング,type-level programming,依存型
date: 2018/08/29 18:45:17 JST
---

# はじめに：欲求に忠実
最近は研究の傍ら[Rust言語][rust-lang]を勉強していて、[Haskeller観点からのチートシート](/prog/rust-memo.html)などを書きつつ日常の小品みたいなものを書いたりしています。

Rust に触ってみたのは、主に所有権の概念やスコープの考え方が、線型型やアファイン型とリージョンモナドの考え方に近く、それらを実用的なレベルに持ってきてGCとかをなくしている、という話に興味をそそられたからです。
そうした resource-sensitive な思想の下で設計されながら、Haskell などの関数型言語で言うところの代数的データ型のようなものもあり、静的解析で正しさを可能な限り保証しようとするというのですから、触ってみない訳にはいきません。

そうやって触っていると、どうも **Rust の型システムは結構強力**なようだ、という事がわかりました。
で、十分強力な型システムを見ると、**型レベル自然数とサイズ付きベクトルを実装したくなる**というのは**人類普遍の欲求である**といって差し支えないでしょう。
というわけで、欲望に忠実に生きることにしている私は、早速 **Rust で型レベル自然数とサイズ付きベクトルを実装**してみることにしました。

完全なコードは Gist にアップしてあります：

[](https://gist.github.com/konn/f43a3ed710f61e17d12d64d979a292ab)

# 普通に Array じゃだめなの？
ところで Rust には *array* と呼ばれる固定長配列の型が予め用意されていて、`let a: [i64; 5] = [1,2,3,4,5]`{.rs}のような形で利用出来ます。

これでいいんじゃね？と思う訳で、まあ部分的にはそれでいいんですが、でもやっぱりそれでは駄目です。

というのも、Rustはどうやら**標準の型レベル自然数に対する四則演算がない**、というか、そもそも型やジェネリクスの引数として**型レベル自然数を取ることが出来ない**ようです。
これは、標準的な型レベル自然数では、例えば次のような**連結関数すら書きようがない**、ということになります。

```rust
fn append<T, n, m>(left: [T; n], right: [T; m])  // n, m ってなんだよ
   -> [T; n + m]                                 // n + m とは？？？？
{
  ...
}
```

折角型レベル自然数っぽいのがあるのにこれは悲しいですね……。
そんなに悲しくないという方も、空気を読んでここは悲しんでください。悲しいですね。

# そこで自前自然数
という訳で、連結関数や平坦化関数を書きたいので、自前で型レベル自然数を実装することを考えましょう。
こういう時は**Peano数項**として自然数を表現するのが常套手段です。
```rust
pub struct Zero {}
pub struct Succ<T> {}
```
などとあった時に、$0$に当るのが`Zero`{.rs}で、$3 = 1 + 1 + 1 + 0 = S(S(S(0)))$に当るのが`Succ<Succ<Succ<Zero>>>`{.rs}ですね。

あとは、型レベルの足し算があれば嬉しく、例えば GHC だったら次のように書きます：

```haskell
type family Plus (n :: Nat) (m :: Nat) where
  Plus Zero     m = m
  Plus (Succ n) m = Succ (Plus n m)
```

つまり、第一引数をみていって、`Succ` を一枚ずつ剥しては外側に持っていく、というのが足し算の定義です。
これは、以下の要因から、そのままでは Rust には移植出来ません。

* Rustでは Haskell の閉じた型族に当る機能がない。
    * 型上の「パターンマッチ」をして直に型関数を定義出来ない。
* Haskellのようなデータ型の昇格もなく、依存型の機能もない。
    *  値を型レベル、型を種レベルに持ち上げて`:: Nat`{.hs} のようなことは出来ない。
* Rust のトレイトは一つの型引数しか持てない。
    * なので、たとえば `impl Plus for N, M`{.rs} のような書き方は出来ない。

これらを乗り越えるには幾つかやりようがあると思いますが、ここでは以下のような作戦を取ることにしました：

* まず自然数の計算を扱う**抽象構文木に当るダミーの型**を沢山用意する。
* 「Peano数項に簡約可能」を表すトレイト `Nat`{.rs} を使い、これをあたかも自然数に対応する種（Kind）であるかのように扱う。
    * `Nat`{.rs} には「計算結果」を表す**関連型** `Eval`{.rs} と、対応する整数の値を返す `fn as_int() -> usize`{.rs} を持たせておく。
* トレイトの `where`{.rs} 節を使って必要な帰納法の仮定を書き下し、特定の形の型に対して `Nat`{.rs} トレイトを実装する。

## 実際の実装
まずは、上で言ったように「ペアノ数項に簡約される」型のトレイト `Nat`{.rs} を定義しておきます。

```rs
pub trait Nat {
    type Eval;
    fn as_int() -> usize;
}
```

まず、一番簡単な「ゼロ」に当る型 `Zero`{.rs} は次のようになります：

```rs
pub struct Zero {}

impl Nat for Zero {
    type Eval = Zero;
    fn as_int() -> usize {
        0
    }
}
```

では次に $1 + n$ に当る型、`Succ<N>`{.rs} を見てみます：

```rs
pub struct MkSucc<N> {
    _maker: PhantomData<fn() -> N>,
}

impl<N: Nat> Nat for MkSucc<N> {
    type Eval = MkSucc<<N as Nat>::Eval>;
    fn as_int() -> usize {
        1 + <N as Nat>::as_int()
    }
}

pub type Succ<N> = <MkSucc<N> as Nat>::Eval;
```

ここでは二種類の型 `MkSucc<N>`{.rs} と `Succ<N>`{.rs} が定義されていますね。
前者は単に抽象構文木上で「$1 + N$ っぽい何か」を表す構築子になっていて、`Succ<N>`{.rs} は「`MkSucc<N>`{.rs}を実際に `Eval`{.rs}を呼んで簡約させた型」を返すようになっています。
実際、`Nat`{.rs} トレイトの `as_int()`{.rs} の実装を見ると、「内側の `N` の値を計算して $1$ を足す」ものになっています。
`Eval`{.rs} も同様ですが、一つだけ注意しないといけないのは、完全に簡約しないといけないので、**まず `N`{.rs} を簡約してから `MkSucc`{.rs}を包む**ようになっている、ということです。

ところで、`MkSucc<N>` は型としての意味しかないので、フィールドを持たない筈ですが、ここでは `PhantomData`{.rs} 型のフィールドを持っています。
これはダミーのフィールドで、かいつまんでいえば型引数の `N`{.rs} が所有権に何の影響も及ぼさないことをコンパイラに伝えるためのものです。
詳しくは以下の qnighy さんの記事に載っていますので、そっちを御覧ください：

[](https://qnighy.hatenablog.com/entry/2018/01/14/220000)

## コンストラクタの実装
ここまでで、`Sized::new(arg)`{.rs} 関数が実装できます。
これは、`Vec<T>`{.rs} 型の引数を取って、長さが型レベル自然数と等しければ包んで返し、そうでなければ `None`{.rs} を返すものです：

```rust
impl<N: Nat, T> Sized<N, T> {
    pub fn new(v: Vec<T>) -> Option<Sized<N, T>> {
        if <N as Nat>::as_int() != v.len() {
            return None;
        } else {
            Some(Sized {
                _maker: PhantomData,
                _vector: v,
            })
        }
    }

    pub fn size(&self) -> usize {
        <N as Nat>::as_int()
    }
}
```

ではこれを実行してみましょう：

```main.rs
extern crate type_naturals;
use type_naturals::*;

fn main() {
    let mfive: Option<Sized<Five, _>> = Sized::new(vec![1, 2, 3, 4, 5]);
    println!("Sized<Five, _> = {:?}", mfive);
    let mthree: Option<Sized<Three, _>> = Sized::new(vec![6, 7, 8]);
    println!("Sized<Three, _> = {:?}", mthree);
}
```

```zsh
$ cargo run
Sized<Five, _> = Some(Sized { _maker: PhantomData, _vector: [1, 2, 3, 4, 5] })
Sized<Three, _> = Some(Sized { _maker: PhantomData, _vector: [6, 7, 8] })
```

たとえば、これで `mthree` の定義を以下のように書き換えると、`None`{.rs} が返ります：

```main.rs
    let mthree: Option<Sized<Three, _>> = Sized::new(vec![6, 7]); // to short!
```

```zsh
$ cargo run
Sized<Five, _> = Some(Sized { _maker: PhantomData, _vector: [1, 2, 3, 4, 5] })
Sized<Three, _> = None
```

## 足し算の実装
では次に足し算を実装してみたいと思います。

まずは、足し算を表現する型 `MkPlus`{.rs} と、その最終的な簡約結果に当る `Plus`{.rs}を定義します：
```rust
pub struct MkPlus<N, M> {
    _maker: PhantomData<fn() -> (N, M)>,
}

pub type Plus<N, M> = <MkPlus<N, M> as Nat>::Eval;
```

さて、ではあとは `as_int()`{.rs} と `Eval`{.rs} を実装しましょう。

上で見たように、ペアノ自然数の足し算は、第一引数についてパターンマッチをして再帰的に計算していきます。
まずは第一引数が $0$ の自明な場合を書きましょう。

```rust
impl<M: Nat> Nat for MkPlus<Zero, M> {
    type Eval = <M as Nat>::Eval;
    fn as_int() -> usize {
        <M as Nat>::as_int()
    }
}
```

これは簡単ですね。次に帰納法のステップで、第一引数が `MkSucc<N>` の形をしている場合です：

```rust
impl<N, M> Nat for MkPlus<MkSucc<N>, M>
where
    N: Nat,
    M: Nat,
    MkPlus<N, M>: Nat,
{
    type Eval = <MkSucc<MkPlus<N, M>> as Nat>::Eval;
    fn as_int() -> usize {
        <MkSucc<N> as Nat>::as_int() + <M as Nat>::as_int()
    }
}
```

`where`{.rs} 節に `MkPlus<N, M>: Nat`{.rs} という制約がありますね。
我々からすれば、`N, M: Nat`{.rs} だけあれば十分動くように見えますが、他の型があとから `MkPlus` の引数に入ってくる可能性もあり、これだけで場合が取り尽せている、ということは、コンパイラには伺い知れず、`MkSucc<MkPlus<N, M>`{.rs}の`Nat`{.rs}トレイトを見付けさせるには、このように明示的に `where`{.rs} 節に指定する必要があるようです。

他の方法を採用すれば何か巧いこといくのかもしれません。有識者の意見を待ちます。

## ベクトルの結合
いずれにせよ、これで足し算が出来たので、上記の `append` が書けますね。書きましょう。

```rust
pub fn append<N, M, T>(l: Sized<N, T>, r: Sized<M, T>) -> Sized<Plus<N, M>, T>
where
    MkPlus<N, M>: Nat,
{
    let Sized { _vector: mut l, .. } = l;
    let Sized { _vector: mut r, .. } = r;
    l.append(&mut r);
    Sized {
        _vector: l,
        _maker: PhantomData,
    }
}
```

試してみましょうか。

```main.rs
fn main() {
    // snipped ...
    if let Some(five) = mfive {
        if let Some(three) = mthree {
            let eight = append(five, three);
            println!("Five + Three = {:?}, of size {}", eight, eight.size());
        }
    }
    println!("Must be None: {:?}", Sized::<Five, ()>::new(vec![]));
}
```

ところが、これをコンパイルしようとすると……

```zsh
$ cargo run
error[E0275]: overflow evaluating the requirement `_: std::marker::Sized`
  --> src/main.rs:13:25
   |
13 |             let eight = append(five, three);
   |                         ^^^^^^
   |
   = help: consider adding a `#![recursion_limit="128"]` attribute to your crate
   = note: required because of the requirements on the impl of `type_naturals::Nat` for `type_naturals::MkPlus<type_naturals::MkSucc<_>, _>`
   = note: required because of the requirements on the impl of `type_naturals::Nat` for `type_naturals::MkPlus<type_naturals::MkSucc<type_naturals::MkSucc<_>>, _>`
```

ギャオス！なんかよくわからないけどエラーが出ます。
どうやら、`MkPlus`{.rs} がトレイト `Eval`{.rs} に属するかどうかの条件を列挙しようとして無限ループに陥っているようで。
`five`{.rs} や `three`{.rs} の型はもう決まっているし、それが決まれば `Plus`{.rs} の値も決まる筈ですから、なんかそこの定義をズバっと持ってくれば良さそうな気がするのですが、トレイトの解決が内部でどう実装されているのかわからないので、今一なんでこうなるのかわかりません。
**Rust有識者の解説をお待ち**しています！！！

いずれにせよ、これは以下のようにそれぞれの型を明示すると通るようになります：

```rs
            let eight = append::<Five, Three, _>(five, three);
```

```zsh
$ cargo run
... snip ...
Five + Three = Sized { _maker: PhantomData, _vector: [1, 2, 3, 4, 5, 6, 7, 8] }, of size 8
```

ここでは $5 + 3$ でちゃんと $8$ 要素を持つ、ということが計算できているようです。
試しに、返値の型を結局$8$に簡約されるけど違う式で置き換えてみましょう。

```rs
            let eight: Sized<Plus<Four, Four>, _> = append::<Five, Three, _>(five, three);
```

```zsh
$ cargo run
... snip ...
Five + Three = Sized { _maker: PhantomData, _vector: [1, 2, 3, 4, 5, 6, 7, 8] }, of size 8
```

ちゃんと同じ結果が出ていますね！

## ここから先
同様にして、掛け算やネストされたリストを均す `flatten` も実装出来ます：

```rs
pub struct MkMult<N, M> {
    _maker: PhantomData<Fn() -> (N, M)>,
}
...
type Mult<N, M> = <MkMult<N, M> as Nat>::Eval;

pub fn flatten<N, M, T>(vs: Sized<N, Sized<M, T>>) -> Sized<Mult<N, M>, T>
where
    MkMult<N, M>: Nat,
{
    let Sized { _vector: vs, .. } = vs;
    let v: Vec<_> = vs.into_iter().flat_map(|a| a._vector.into_iter()).collect();
    Sized {
        _vector: v,
        _maker: PhantomData,
    }
}
```

```main.rs
fn main() {
    // ... snip ...
    let nested: Sized<Three, Sized<Four, _>> = Sized::new(vec![
        Sized::new(vec![1, 2, 3, 4]).unwrap(),
        Sized::new(vec![5, 6, 7, 8]).unwrap(),
        Sized::new(vec![9, 10, 11, 12]).unwrap(),
    ]).unwrap();
    println!("Flatting {:?} of size {} to...", nested, nested.size());
    let fla = flatten::<Three, Four, _>(nested);
    // let fla = flatten(nested);
    println!("...flattened into {:?}, with length {}", fla, fla.size());
}
```

```zsh
$ cargo run
Flatting
  Sized { _maker: PhantomData,
          _vector: [Sized { _maker: PhantomData, _vector: [1, 2, 3, 4] },
                    Sized { _maker: PhantomData, _vector: [5, 6, 7, 8] },
                    Sized { _maker: PhantomData, _vector: [9, 10, 11, 12] }]
        }
of size 3 to...

...flattened into
  Sized { _maker: PhantomData, _vector: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12] },
with length 12
```

`Iterator`{.rs} の `size_hint`{.rs} とか `ExactSizeIterator`{.rs} とかも `N` の情報を適宜使って書ける気がしますが、複雑すぎると `as_int()`{.rs}の計算コストがかかってあまり嬉しくない気もする。

# まとめ
なんかこの辺りで面倒になってきたので終わりにしますが、Rust の型システムは型レベルのPeano自然数を表現出来、それを使った長さ付きベクトル演算が定義出来る程度には強力である、ということがわかりましたね！
とはいえ、これ全部 Peano 自然数で表現してますし、とても効率が悪く、コンパイル時にスタックが溢れたりすると思うので実用には向かないでしょう。

もし Rust に型レベル自然数引数がちゃんと入って、加減乗あたりが入る予定があるのなら、この辺りの話は要らなくなりますが、実際どうなんでしょうか。
任意の型を持ち上げあられるように……ではなく、自然数ぐらいなら何とかなるんじゃないでしょうか。識者の解説を（ry

[rust-lang]: https://www.rust-lang.org
