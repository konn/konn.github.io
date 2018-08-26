---
title: Haskellerの私的Rustチートシート
date: 2018/08/27 01:45:47 JST
author: 石井大海
description: Rustに入門したので、適宜HaskellのこれはRustのこれ、ということをここに蓄積していって、個人的なチートシートにする。
tag: Rust,Haskell,Programming,Tech,Programming Language
---
Rustに入門したので、適宜HaskellのこれはRustのこれ、ということをここに蓄積していって、個人的なチートシートにする。

# Borrow と Ownership
* 基本的に Region Monad と線型型（呼ばなくても良いので正確にはアファイン型）が組込みだと思えばよい。
    * `{` から `}` までの間でスコープが区切られ、スコープごとに `Drop` （＝`free`）がされる。

# プロジェクト管理
* Stack に当たるのが cargo
* `stack.yaml` は `Cargo.toml`
* コードは `src` 直下固定。`src/main.rs` が `Main.hs` 相当で、`src/lib.rs` がライブラリのルートに当る。

# モジュールシステム
* 一つの `.rs` ファイル内で複数のモジュールを、ネストして定義出来る。
* `mod a;` のように定義を省略した場合、`a.rs`の内容がモジュール `a` の定義として扱われる。
* `foo::bar::buz`{.rust} に当るファイルは `src/foo/bar/buz.rs`。
    * このファイルの存在下では、`foo::bar` に当るのは `src/foo/bar/mod.rs`。
* Haskellの `import`{.hs} に近いのは `use`{.rs}。
    * `use a::b::c`{.rs} と宣言しておくと、以後モジュール `a::b` 以下の定義は `b::fun`{.rs} などと呼び出せるようになる。

# 字句構造
* 基本的に全て式だが、`()` 返すのが文。
* 関数の最後で値を返す際には `return`{.rust} は不要。
* ただし式の最後に `;` を付けると文になってしまうので、生の式で値を返す場合は `;` を付けないように。
* Haskell と違い、変数はバシバシ shadowing していく文化。
    * __注意__：Shadowing された変数にアクセスする方法はないが、shadowing されてもスコープが区切られなければ Drop はされない。
* コメントは一行コメントのみ：`// これはコメントです`{.rs}

# 変数束縛
* `let a: T = b`{.rs} は不変変数宣言。
* `let mut a: T = b`{.rs} は可変変数。
* `static A: T = b`{.rs} は定数宣言。
* `static`{.rs} 以外は型註釈はある程度省略出来る。
* あとは `&`{.rs} とか `ref`{.rs} とかの修飾子が適宜つく。

# 制御構造
* `if`{.haskell} に相当するのは `if (...) { } else { .. }`
    * `else` 節は省略可。複数の `else if` を連ねることもできる。
* `case`{.haskell} に相当するのは `match expr { case => alt, ...}`{.rust} __および__ `if let case = expr { ... }`{.rust}。
    * `match`{.rust} 式は Haskell の `case` 相当だが、**exhaustive （全場合列挙）でないとエラー**になる。

      ```rs
      match a {
        A(x) if x == 0  =>  "zero of A",       // ガードみたいなのも出来る
        A(x@10 ... 13)  => "10...13"
        // アズパターンみたいのがあるが、所有権の関係で「別名」付けるのは無理そう
        
        A(x)            => {
          println!("Non zero A({}) found", x);
          "boo"
        },
        B(x) | C(x) => "Or!" // オアパターンが出来る
        B(x) | C(x) if x == 0 => "Or!" // if はオアより結合が弱い。
        
        _ => "baz"
      }
      ```
    * `if let`{.rust} はHaskellでの**partialな`case`{.haskell}**に当る。
        * `else if let p = e { ... } else { ... }`{.rust} のように連ねたりfallback で else が使える。
* ループ：`while p { ... }`{.rust} および `loop { ... }`{.rust}
    * スコープとlifetimeの関係で、条件判定やパタンマッチを `while`{.rust} の条件部ではなく `loop{ ... }`{.rust} 内部でやった方が適切な場合あり。

# データ型
* `Int16, Int32, Int64, Word16, Word32, Word64`{.rust} に当るのが `i16, i32, i64, u16, u32, u64`{.rust}。
  ポインタアドレス等、アーキテクチャ依存の整数型（`Int`{.hs} や `Word`{.haskell}）は `isize`{.rs} (符号付) や `usize`{.rs} (符号無)。
* `struct`{.rust} や `enum`{.rust} が Haskell の `data`{.hs} や `newtype`{.hs} に当る。
  ```rs
  struct Struct {
    fld: i64,
    ...
  }

  enum Enum {
    A(i64),
    B { val: i32, dull: i16, },
  }
  ```

* `struct`{.rs} は直積型、`enum`{.rs} は直和型。
    * `enum`{.rs}の各コンストラクタを**variant**と言う。
    * フィールドラベルのついた `struct F { a: i64 }`{.rs} と、ついていないタプル構造型 `struct F(i64)`{.rs} は全く違うもの。
* 再帰型は `Box<a>`{.rs} などに包んで間接的に参照の形で持つ。
    * フィールドの値や関数の引数は、**メモリ上でのサイズが確定**していないといけないので。
* コンストラクタは `Enum::A`{.rs} のように呼ぶ。
    * ここでも `use`{.rs} が使え、`use Struct::*`{.rs} や `use Enum::*`{.rs} のようにすると、単に`A(12)`{.rs}や`B { val: 12, dull: 54 }`{.rs} のように呼べる。
* `NamedFieldPuns`{.haskell} 拡張のようなことが出来、`Foo { bar } `{.rust} とやるとスコープにある `bar` の値が `bar`フィールド`bar` の値になる。
    * 逆にパターンの来る文脈で `let P {x , y} = p`{.rust} とすればフィールド `x`, `y` の値が変数 `x`, `y` にバインドされる。
* ベクトル：`Vec<T>`。リストリテラルのように作るのは `vec![1,2,3]`{.rs}。
    * 固定長ベクトルは `[T; 3]`{.rs} のよう。

# モナド
* モナドはない。
    * でも将来に向けて `do`{.rs} は予約語になっているようだ。
* `Maybe`

# オーバーロード
* Haskell の型クラスに当るのがトレイト（`trait`{.rs}）

  ```rust
  trait Trait<'a, T, U>: this::is::SuperTrait {
    type F  // 関連型（associated type）もある
    fn foo<T>(args: i64, ...)  -> result;
  }

  impl<'a, T> Trait<'a, T, T> for MyStruct {
    ...
  } 
  ```
* Rust は「ゼロコスト抽象」を謳っており、使用したトレイトの実装は自動的に特殊化されコードが生成される。
    * いわば、`SPECIALISE`{.hs} プラグマが適宜有効化されたような状態になっている。
* デフォルトで `OverlappingInstances`{.hs} 状態。
* 存在型：トレイト制約付きの存在型（existential type）は`dyn`{.rs}で作る。
  ```hs
  data Showable where
    MkShowable :: Show a => a -> Showable
  ```
  に対応するのは、
  ```rs
  struct Showable(dyn Display);
  ```
    * `dyn`{.rs} は動的にディスパッチするので、ゼロコスト抽象は効かない。
