---
title: Extensible Effects はモナド変換子に対する救世主になり得るか？
date: 2013/07/21 21:26:00 JST
author: 石井大海
tag: haskell, monad, programming, paper
description: Oleg らによる、モナド変換子に対する大体手法、Extensible Effects の論文を読んだ感想紹介記事です。
---

Oleg, Sabry and Swords らによる [Extensible Effects: An Alternative to Monad Transformers](http://www.cs.indiana.edu/~sabry/papers/exteff.pdf ) の論文を読んだメモ的な何かです。モナド変換子に関する簡単な現状確認から入ってはいますが、想定読者層は日常的にモナドやモナド変換子を用いたプログラムを書いている人達です。

どちらかというと自分向けのメモの性格が強いので、詳しい部分は論文を参照してみてください。

背景：モナド変換子とその問題
=======================
Haskell を中心に、関数型言語では副作用のある**函数を合成**するための手段として**モナド**が広く用いられている。モナドは非常に強力な抽象化で、およそ副作用と呼べるものはモナドを使って定式化することが出来た。例えば、大域的な環境 `r`{.haskell} を持った計算は `Reader r`{.haskell} モナドを使うし、操作のログを残すような計算は `Writer [String]`{.haskell}、失敗する恐れのある計算は `Maybe`{.haskell}、外界との入出力全般を使うのなら `IO`{.haskell}を使う、といった具合に。

他方、複数の種類の副作用を持つようなモナドを作りたいと思うときがある。必要な機能を持ったモナドを一から書いても良いが、モナドを自前で定義するのは面倒だし、既に用意されているモナドを組合せることが出来れば便利だ。そこで、Haskell では**モナド変換子**と呼ばれる手法を使って、既存の**モナドを合成**して得ることが Haskell では一般になっている。つまり、既存の「ピュア」なモナドに対して、他のモナドに対して垂直に合成できるような変換子を用意してやって、それを積み上げて新しいモナドを作るというわけだ。例えば、「大域的な環境を参照しつつ、操作のログを残しながら、ユーザとの入出力を行なう失敗するかもしれない計算」であれば `MaybeT (WriterT [String] (ReaderT r IO))`{.haskell} というような感じに。

こうしたモナド変換子を用いた方法は一般的に用いられているが、次のような欠点があった：

1. 合成するモナドが増えるたびに、計算の効率が落ちる
2. モナドを合成する順番によって、計算の意味が変わってくる
3. 下位のモナドの処理を行うのに、一々 `lift`{.haskell} を行う必要がある
4. モナドの合成順は固定されてい、途中で入れ替えることができない

それぞれを手短かに説明しよう。**1. 合成するモナドが増える度に、計算の効率が落ちる**というのは、モナド変換子は**別種の種類のモナドの組合せ**であるので、例えば `return`{.haskell} が呼ばれたり、`>>=`{.haskell} で合成したりする度にそれぞれの層に対する処理が発生してしまうという事だ。一つ一つの計算ステップで必要になるモナドの処理は限られていても、毎回全階層での処理が発生してしまうので、一般にモナドスタックが深くなる程効率が落ちてしまう。これを回避するには、最終的に自前でモナドを定義したりしなくてはいけなくなり、余り嬉しくない。

**2. モナドを合成する順番によって、計算の意味が変わってくる**というのはどういうことか。例えば、`StateT Int Maybe`{.haskell} という計算と `MaybeT (State Int)`{.haskell} という計算を比較してみよう。

```haskell
action :: (MonadPlus m, MonadState Int m) => m b
action = modify (+1) >> mzero

ghci> runState (runMaybeT (modify (+1) >> mzero)) 10
(Nothing,11)

ghci> runMaybeT (runStateT (modify (+1) >> mzero) 10)
Nothing
```

最初の実行例では、`MaybeT (State Int)`{.haskell} の順で合成されている[^1]。一番下のモナドは `State Int`{.haskell} なので、計算が失敗してもその直前の内部状態は保存されている。それに対し、二番目の例は `StateT Int Maybe`{.haskell} の順になっている。一番下のモナドは `MaybeT IO`{.haskell} なので、計算が失敗するとその前までの内部状態は全て破棄されてしまう。上のように、元のアクションが polymorphic な形で定義されていれば、実行する際に `run`{.haskell} する順番を選んで適切な意味に変えることも出来るが、何か具体的なアプリケーションをモナドで作ろうとする時は、まずモナドスタックを構成して `newtype`{.haskell} で包むというのが一般的な手順であるように思うし、その場合は最初に合成順について考えなくてはいけないということになる。

[^1]: モナドスタックの上から順に実行していくので、`runHoge`{.haskell} の順番とスタックの合成順は一般に逆順になることに注意。

**3. 下位のモナドの処理を行うのに、一々 `lift`{.haskell} を行う必要がある** について。例えば、`mtl`{.haskell} では `ask`{.haskell} とか `mzero`{.haskell} とかは `MonadReader`{.haskell} や `MonadPlus`{.haskell} といった形でクラスを使って一般的に定義されてはいる。しかし、それは各モナド変換子に対して、例えば下のレイヤーに `MonadReader`{.haskell} なり `MonadPlus`{.haskell} なりのインスタンスが居れば `lift`{.haskell} してそれを使う、というような形になっている。だから、表面上はなくても、`lift`{.haskell} は常に本質的にモナド変換子に張り付いている。また、こういった型クラスがないような場合は、やはり `lift`{.haskell} を連発する必要がある。例えば、`MaybeT IO`{.haskell} の下で `IO`{.haskell} を実行するには、やっぱり `lift`{.haskell} なり `liftIO`{.haskell} を使ってやる必要がある。更に、二つ以上下のモナドの機能を使うには `lift`{.haskell} を多重に使ってやる必要があり、これは中々辛いものだ。

**4. モナドの合成順は固定されてい、途中で入れ替えることができない** というのは、上の問題と関連していて、下の層のモナドの副作用を使うには `lift`{.haskell} で潜る必要があり、なおかつその順番を跨ぐような処理は出来ない、ということだ。これについては、のちほど論文で言及されている例を通じて詳しく説明しよう。

さて、ここまでモナド変換子の抱える問題について整理してきた。今回 Oleg らが紹介している **Extensible Effects** の手法（以下 *EE* と呼ぶ）は、これらの問題を一挙に解決することを目論んだものだ。

Extensible Effects の仕組み
--------------------------
上の問題に対して、Oleg らの手法がどういった解決を与えているのかを見てみよう。


### モナドの合成と効率性：モナドスタック = 委譲関係
一番目。**モナド合成による効率性の低下問題**について。EE がモナド変換子と根本的に異なるのが、**全ての計算を一つのモナドの中で行う**ことだ。この中心的な役割を果すモナドは、原著では `Eff`{.haskell} モナドと呼ばれている。

一つのモナドの中で行うといっても、一つの超万能なモナドがあってその中で操作をするという訳ではない。それではモナドの旨味がなくなってしまう。ではどうするのか？EE の基本的な考え方は、**副作用とはクライアントとハンドラの相互作用だ** という物だ。一つ一つのアクションは、それを処理出来るハンドラへのリクエストとして見る事が出来る。例えば、`ask`{.haskell} は大域環境を持つ計算を司るハンドラへのリクエストと見れるし、`IO`{.haskell} 計算なんかは正に外部との入出力を行うハンドラへの命令と見ることが出来る。

そこで EE では、`Eff`{.haskell} モナドの型パラメタとして、「**実行に必要な副作用ハンドラの一覧**」をタグとして持たせている。そして、そのリクエストを処理するハンドラが今までの `runStateT`{.haskell} や `runMaybeT`{.haskell} に当たる。ハンドラは自分の処理できるコマンドを全て処理して、処理し終えた印としてタグを取り除く。これを繰り返していって、最終的にピュアな計算か、`IO`{.haskell} や `ST`{.haskell} のような基盤モナドの計算までに辿り着いたら、`run`{.haskell} や `runLift`{.haskell} で結果を取り出す。これが EE の基本的な戦略だ。

ちょっとわかりづらいかもしれない。これは、（著者の五年前くらいで止まっている知識を総動員すれば）OOP でいう *Chain of Responsibility* パターンを Haskell で実現したものになっている。EE は、モナド変換子のモナドスタックを、委譲関係のチェーンとして捉え直したものとも云うことが出来るのだ。例えば、`StateT Int Maybe`{.haskell} なら `State Int :> Try :> Void`{.haskell} という委譲関係のチェーンだし、`WriterT [String] (ReaderT Int IO)`{.haskell} なら `Writer [String] :> Reader Int :> Lift IO`{.haskell} というチェーンになる（`:>`{.haskell} は左から右に委譲関係がある、という風に読めばよい）。

直観的には、`runReader`{.haskell} なり `runState`{.haskell} なりを噛ませる度に、それぞれのハンドラが処理出来る命令を「処理」して、出来ないものはそのまま残しておくことになる。下位のハンドラへ処理を流すのに、結局ハンドラの数だけ使ってしまいそうな気がするが、EE ではこれを継続渡し形式を使ったコルーチンとして実現しているため、結果的にはハンドラ数に依存しない定数時間で処理が可能になるそうだ[^2]。

[^2]: 継続渡しはいまいちちゃんと理解出来ていないので、これで大丈夫な理由はちゃんと腑に落ちていない。

### 副作用の合成順：Open Union
次に**2. モナドを合成する順番によって、計算の意味が変わってくる** について。上の説明だけでは、「モナドスタックを一つのモナドの中に押し込めただけで、結局階層になってるんじゃないの？」という突っ込みが成立しうる。確かに、`runReader`{.haskell} などを呼んで実際に計算を実行する際には階層は確定していることになる。しかし、EE がモナド変換子と大きく異なるのは、*Open Union* という考え方を用いていることだ。

どういうことか？それを説明するために、各函数のシグネチャを見てみよう。

```haskell
-- | Computation with the global environment (corresponds to Reader)
ask       :: (Typeable e, Member (Reader e) r) => Eff r e
local     :: (Typeable e, Member (Reader e) r) => (e -> e) -> Eff r a -> Eff r a
runReader :: Typeable e => Eff (Reader e :> r) w -> e -> Eff r w

-- | computation which may fail (corresponds to MaybeT)
failure   :: Member Try r => Eff r a
recover   :: Member Try r => Eff r a -> Eff r a -> Eff r a
runTry    :: Eff (Try :> r) a -> Eff r (Maybe a)

-- | Computation with underlying monad
lift      :: (Monad m, Typeable1 m, MemberU2 Lift (Lift m) r) => m a -> Eff r a
runLift   :: (Typeable1 m, Monad m) => Eff (Lift m :> Void) a -> m a

-- | evaluate @Eff@ to pure value.
run       :: Eff Void a -> a
```

この函数を見ると、`runHoge`{.haskell} 系の函数以外は、`Member Hoge r`{.haskell} という型制約を使って記述されていることがわかる。これは、「`r`{.haskell} の中の何処かに `Hoge`{.haskell} という副作用のタグが含まれいてる」という意味の制約だ。`:>`{.haskell} は型レベルのリストと見做すことが出来るので、その中に `Hoge`{.haskell} が入っているかどうか？という判定だと思えばよい。このように、`ask`{.haskell} や `failure`{.haskell} などの「副作用を持った」函数は単にそのチェーンの中に必要な副作用が含まれていることしか要求しないのだ。これは、モナド変換子を使う際に `MonadReader`{.haskell} や `MonadPlus`{.haskell} クラスを使ったやり方と似ていると云えば似ているが、このような仕組みにしたことで、**3. 一々 `lift`{.haskell} を行う必要がある** という問題が解決されている。モナドの階層は、各函数のレベルで見ればあくまでもフラットで対等なものなので、いちいち `lift`{.haskell} を呼ぶ必要がないのだ。もっとも、最終的に `IO`{.haskell} や `STM`{.haskell}、`ST`{.haskell} といった値を計算するようなモナドを合成したい場合はあるが、そのときは `liftIO`{.haskell} と同じ要領で上の `lift`{.haskell} を呼ぶことになる。だが、こうした基底モナドの命令を呼び出す場合を別にすれば、合成された他の副作用を呼ぶときに一々 `lift`{.haskell} をつける必要がなくなる。

どういう事か？例えば、余りよい設計とは云えないが、`ReaderT Int (Reader String) a`{.haskell} のような例を考えてみよう。EE で対応するのは `Eff (Reader Int :> Reader String :> Void) a`{.haskell} という型になる。例えば、従来のモナド変換子を用いた方法では、次のように書くことになる：

```haskell
action :: ReaderT Int (Reader String) (Int, String)
action = do
  int <- asks (*2)
  str <- lift (asks reverse)
  return (int, str)
```

つまり、「一個下」の文字列の環境を取り出すのに、ここでは `lift`{.haskell} を使う必要があった。だが、EE ではこれは次のように簡単に書ける：

```haskell
action :: (Member (Reader Int) r, Member (Reader String) r) => Eff r (Int, String)
action = do
  int <- asks (*2)
  str <- asks reverse
  return (int :: Int , str :: String)
```

面倒な `lift`{.haskell} が消えて、どちらも `asks`{.haskell} だけになっている！ただ、値を返す際にそれぞれの値に型注釈をしている。これは、`asks`{.haskell} の型の曖昧性をなくすためで、`int`{.haskell} や `str`{.haskell} がどの `Reader`{.haskell} に向けての命令なのかをハッキリさせるためだ。このような簡単なプログラムだと注釈を書く必要があったが、現実的なもっと長いプログラムであれば、こういった型は推論により確定できるようになるだろうから、余り問題にはならない。それに、型の注釈は簡単に書けるが、`lift`{.haskell} は階層の数だけ書かなくてはいけなくて、`ReaderT Int (ReaderT String (ReaderT (Int, Bool) (ReaderT Env IO))) a`{.haskell} みたいな型があったら、いったいどこの値を取り出すのに何回 `lift`{.haskell} を書けばいいのかパッとみよくわからないだろう。EE では、そういったことを考える必要性はない。

ここで、あるていど使っているひとは疑問に思うかもしれないことがある。それは、「`ReaderT Int (Reader Int) a`{.haskell} のように同じ型の状態を持つ Reader が重なってるような状況をどう表現するの？」ということだ。これには、お馴染の `newtype`{.haskell} ハックを使えばよい。そもそも、こういった型の設計はプログラムとして余りたちのいいものではないし、それぞれの環境の意味をハッキリさせる意味でも、それぞれを `newtype`{.haskell} で包むべきだ。

```haskell
newtype Page = Page Int deriving (Show, Eq, Ord, Num, Integral)
newtype Line = Line Int deriving (Show, Eq, Ord, Num, Integral)

lineLen :: Int
lineLen = 40

linePerPage :: Int
linePerPage = 40

currentPos :: (Member (Reader Page) r, Member (Reader Line) r) => Eff r Int
currentPos = do
  Page p <- ask
  Line l <- ask
  return (p * linePerPage * lineLen + lineLen * l)
```

こうすれば、 `lift`{.haskell} も要らないし、パターンマッチの所で型が確定するので、型注釈も要らない。

また、上のコードを見て気付くひとは気付くかもしれないが、多少の柔軟性を犠牲にすれば、`Eff`{.haskell} 型を mtl の `MonadReader`{.haskell} や `MonadState`{.haskell} のインスタンスにすることも簡単に出来る。こうした準備をしておけば、`mtl` の API を使って実質的に EE を使ったプログラムが掛ける。また、mtl から EE への移行を本格的にしようと思っても、コードに殆んど手を入れる必要はない。型注釈の所を書き換えて、`lift`{.haskell} を取り除いたりすれば大抵の場合ちゃんと動くようになる。EE はモナド変換子とは本質的には異なる実装ではあるが、ユーザの側では殆んど変更なしで採り入れることが出来るのだ。

この `lift`{.haskell} と関連して、モナド変換子のアプローチにはもう一つ限界がある。それは、セマンティクスが柔軟ではないということだ。その例を見てみよう（以下、論文からの例）。

問題設定は簡単だ。ここでは、非決定計算とエラー処理とを組み合わせることにしよう。非決定的にリストの計算を進めて、途中で 5 より大きな数が出て来たらそこで処理を中断することにしよう：

```haskell
newtype TooBig = TooBig Int deriving (Show, Eq, Ord)

instance Error TooBig

choice :: MonadPlus m => [a] -> m a
choice = msum . map return

ex2 :: MonadError TooBig m => m Int -> m Int
ex2 m = do
  v <- m
  if (v > 5)
  then throwError (TooBig v)
  else return v

ghci> runIdentity (runErrorT (runListT (ex2 (choice [5,7,1]))))
Left (TooBig 7)

ghci>  runIdentity  (runErrorT (runListT (ex2 (choice [5,4,1]))))
Right [5,4,1]
```

ところで、ここでお上の都合で「やっぱり 7 以下の数は大きくないんじゃね？」ということになったとする。そこで、例外をキャッチして、 7 以下だったら処理を再開するようにしてみよう。

```haskell
exRec :: MonadError TooBig m => m Int -> m Int
exRec m = catchError m handler
  where
    handler (TooBig n) | n <= 7 = return n
    handler e = throwError e

ghci> runIdentity (runErrorT (runListT (exRec (ex2 (choice [5,4,1])))))
Right [5,4,1]

ghci> runIdentity (runErrorT (runListT (exRec (ex2 (choice [5,7,1])))))
Right [7]
```

おや……？最後の計算では、`[5,7,1]`{.haskell} が返って欲しいのに `[7]`{.haskell} しか返ってきていない。なんでだろう！

これは、この函数の型が `ListT (ErrorT TooBig Identity) [Int]`{.haskell} であって、`ErrorT`{.haskell} がより底の方にあるからだ。つまり、例外 `TooBig 7`{.haskell} が飛んだ時点で、上に重ねられていた非決定計算としての `ListT`{.haskell} での演算は忘れ去られてしまうのだ！

どうしよう……と悩んで、じゃあ `ErrorT TooBig`{.haskell} を二重に張り巡らせたらどうだろう？という気分になったとしよう：

```haskell
ErrorT TooBig (ListT (ErrorT TooBig Identity)) a
```

一旦非決定計算上の各ブランチで例外を見てあげて、それからそれを下の基盤に近いほうのエラーモナドに伝播してやろうと云う戦略をとる訳だ：

```haskell
runErrorRelay :: MonadError e m => ErrorT e m a -> m a
runErrorRelay m = runErrorT m >>= check
  where
    check (Right x) = return x
    check (Left e) = throwError e

ghci> runIdentity (runErrorT (runListT (runErrorRelay  (exRec (ex2 (choice [5, 7, 1]))))))
Right [5]
```

えーーーこれどういうことなの……。何で 5 だけ残るの……。

よぉく考えてみよう。`ex2`{.haskell} のコードを見ると、一番最初に `v <- m`{.haskell} としてアクションの値を取り出している。でも、今このコードでの `m`{.haskell} は「`ErrorT TooBig (ListT (ErrorT TooBig Identity)) Int`{.haskell}」 という型の、一番左の `ErrorT TooBig`{.haskell} に包まれた値なのだ。ここで使われている `MonadPlus`{.haskell} のインスタンスは、`ListT`{.haskell} ではなく、`ErrorT a`{.haskell} に対するインスタンスなのだ。これを本来非決定計算の文脈に持っていきたいのだから、ここで `lift`{.haskell} を使わなくてはいけないことがわかる：

```haskell
ex1 :: Monad m => m Int -> ErrorT TooBig m Int
ex1 m = do
  v <- lift m
  if v > 5 then throwError (TooBig v) else return v

ghci> runIdentity (runErrorT (runListT (runErrorRelay  (exRec (ex2 (choice [5, 7, 1]))))))
Right [5,7,1]
```

や、やっと動いた……。

これは、モナド変換子が本質的に抱える限界を表している例だ。では、これを EE を使って書いてみよう。

```haskell
ex2 :: Member (Exc TooBig) r => Eff r Int →Eff r Int
ex2 m = do
  v <- m
  if v > 5 then throwError (TooBig v) else return v

runErrBig :: Eff (Exc TooBig :> r) a -> Eff r (Either TooBig a)
runErrBig m = runError m

exRec :: Member (Exc TooBig) r => Eff r Int -> Eff r Int 
exRec m = catchError m handler
  where 
    handler (TooBig n) | n <= 7 = return n 
    handler e = throwError e

ghci> run (runErrBig (makeChoice (exRec (ex2 (choose [5,7,1])))))
Right [5,7,1]
```

`lift`{.haskell} なんて書く必要もないし、また型の部分以外は、mtl で書いたプログラムと殆んど変わっていないことがわかるだろう。このように、EE は継続渡しを基本として、最終的に一つのモナドとして実行されるフラットな設計になっているので、直観的にモナドの効果を組み合わせることが出来るのだ。

### 階層を超えた副作用
ここまでの例で、「単純に階層構造を委譲関係と捉えたもの」という訳ではないことがわかったという。更に、EE では**階層を超えた副作用のハンドリング**が出来る。つまり、モナド変換子の**4. モナドの合成順は固定されてい、途中で入れ替えることができない** という限界を乗り越えることが出来るのだ。

この解説を書くつもりでいたのだが、段々面倒になってきたので詳細は[論文を参照](http://www.cs.indiana.edu/~sabry/papers/exteff.pdf )して欲しい。論文では、大域環境とコルーチン的な副作用を合わせもったプログラムを書こうとしている。まず、全体でひとつの環境を共有した状態でスタートするが、各スレッドの中で環境が変更されたら、その環境は以後外から切り離されるような仕組みを作りたいとする。詳しい説明は面倒だしちゃんと理解出来ていないのでここでは書かないが、実は、モナド変換子を使った方法ではこのようなプログラムは**書けない**。理由は論文を参照してほしい。しかし、EE を使うとこのような処理も書けるようになるらしい。

おわりに
-------
よくわかっていない部分もあった（特に最後の例）が、駆け足で Extensible Effects について解説してきた。改めて特徴をまとめると次のようになる：

* 変換子を重ねるのではなく、一つのモナドの中でハンドラを組み合わせて記述する
    * ある種の Server-Client モデルや Chain of Responsibility パターンと見做せる
    * 副作用を幾つ組み合わせてもオーバーヘッドは生じない
* 面倒な `lift`{.haskell} は不要
* 単純な一本鎖ではなく、階層を跨いだ処理も可能（？）
* mtl の API の上に載せることも出来、あるいは mtl からも型を変えるだけで簡単に移行出来る
* 内部的には継続渡しに基づいたコルーチンとして実装されている

読んでいて気になった点は以下の通り（私の理解不足もあるだろうので、こうじゃないの？というのがあれば教えて頂けると幸い）：

函数が `Typeable`{.haskell} インスタンスを要求する
:    EE は委譲の判断をする際に内部的に `gcast`{.haskell} を用いており、これを使うのに `Typeable`{.haskell} のインスタンスが必要になる。大抵の型は `deriving`{.haskell} なりを使ってやれば簡単に `Typeable`{.haskell} に出来るが、必ずしもそうは行かないものもあり、また型制約部分が汚なくなるような気がするので、余り嬉しくないような気がする。

ハンドラは継続渡しで書く必要がある
:    既存の物を組み合わせてプログラムを組む場合は良いが、自分で新たな副作用を追加したいと思ったときに、継続渡し形式でプログラムを書くのは慣れていないとちょっと手間がかかるのではないだろうか。

`MonadPlus`{.haskell} や `Applicative`{.haskell} スタイルとの兼ね合い
:   モナド変換子の場合、`Applicative`{.haskell} や `MonadPlus`{.haskell} のインスタンスは、上の例でも出て来たように先頭のモナド変換子が決定する。しかし、`Eff`{.haskell} モナドの場合はその辺りのセマンティクスはどのように決定されるのだろうか？セマンティクスが柔軟だと書いてあるが今一よくわからない。多分、上の例を見る限り、一番最初に `run`{.haskell} したもののセマンティクスが選ばれる？余りありそうもないが、例えば上で「失敗」とされた挙動をして欲しいような場合はどう書けばいいのだろう。

階層を跨いだ実行（interleave）の意味がイマイチよくわからない
:    どういうことなんだろう……。

既存のモナド変換子との兼ね合い
:    `Conduit`{.haskell} であるとか、 `Parsec`{.haskell} であるとかとの連携の仕方？この `Eff`{.haskell} の上に載せられるだろうか。

理論的な取り扱い
:    EE の手法は、応用上は確かにわかりやすい（継続渡しを書くことを除けば）が、理論的にはどのように取り扱われるのだろうか？より詳しく、数学的にどのように定式化されるのだろうか？その辺りがよくわからなかった。個人的に、こうしたものには数学的にしっかりとした基礎付けが欲しく思う（この辺りは異論も色々あるだろうとは思う）。

個人的な感触としては、EE はかなり頑張っていて中々良い代替案になりそうな気がする。ただ、モナド変換子の方がわかりやすいような局面もまだあるように思うし、これが救世主かと云われるとそうでないような気がする。理論的な背景がもう少し欲しくもあるし。いずれにせよ、一つの新しい方法で、それなりに面白く使い易そうなものであることは確かだし、何回か使ってみて感触を確かめてみたい。継続ベースなだけあって、例外処理や限定継続との相性は良さそうだ。

モナド変換子や合成に関する取り組みは他にも幾つかあるようで、たとえば [mmroph](http://hackage.haskell.org/package/mmorph) や [effects](http://hackage.haskell.org/package/effects ) といったものもあるようなので、これらも時間があったら調べて比較してみたいと思う。
