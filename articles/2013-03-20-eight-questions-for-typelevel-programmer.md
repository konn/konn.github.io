------
title: ８つの質問で、GHC の type-level language 業界の現状を知る
author: 石井大海
date: 2013/03/20 15:01:50 JST
description: 「８つの質問」という釣り記事が技術クラスタで昔流行って、それに便乗してGHCの型システムの話を書いた。
tags: Haskell,typelevel
------


Web サービス系の隆盛があって、GHC の type-level language をふんだんに用いた [Yesod](http://www.yesodweb.com) のリリースが騒がれたのが１−３年くらい前だろうか。  
GHC の type-level language が、今どうなってるかって？

<span style="color: red;">**大方の予想より凄惨ですよ。**</span>

それが分かる方法がある。GHC の type-level language技術者に技術力を問う８つの質問によってだ。  
GHC の type-level language 業界のエンジニアの平均レベルを知りたくって、いろんな会社さんの type-level language 開発者（経験者）向けに以下のような８つの質問を継続的にしたいけどそんな人がいない。  
対象者としては、type-level language 経験３から１０年ぐらいの現役バリバリのはずの型レベルエンジニアだ。

その８つの質問というのはこんな問題だ。

GHC の type-level language 技術者に技術力を問う８の質問
--------------------------------------------------
1. Type Family のメリットを一言で表してください。（筆記解答）
2. `wai` package で POST されたデータを取得する方法は何ですか？（筆記解答）
3. Scoped Type Variables を端的に説明してください。（筆記解答）
4. 次のうち、

	```haskell
	(<=) :: Nat -> Nat -> Bool
	Z   <= _   = True
	S _ <= Z   = False
	S n <= S m = n <= m
	```

	で定義される自然数の順序の、型レベル版として用いることが出来るものはどれですか？ GHC の型レベル機能によりその証明を記述し、それが証明になっていることの根拠を述べるか、GHC で記述出来ない場合はその理由を述べ、メタ的な証明を与えてください。余力のある方は、それぞれどのような場合に用いるのに適しているか答えてください。（筆記・選択解答；複数回答可）
	1. そのいち

        ```haskell
		data (n :: Nat) :<= (m :: Nat) where
          ZeroLeq     :: SNat m  -> Zero :<= m
          SuccLeqSucc :: n :<= m -> S n  :<= S m
        ```

	2. そのに

        ```haskell
		type family   (n :: Nat) :<<= (m :: Nat) :: Bool
		type instance Z   :<= n   = True
		type instance S n :<= Z   = False
		type instance S n :<= S m = n :<<= m
        ```

	3. そのさん

        ```haskell
		class (n :: Nat) :<= (m :: Nat)
		instance Zero :<= n
		instance (n :<= m) => S n :<= S m
        ```

	4. そのよん

        ```haskell
		(%<=) :: SNat n -> SNat m -> SBool (n :<= m)
		SZ   %<= _    = sTrue
		SS _ %<= SZ   = sFalse
		SS n %<= SS m = n %<= m
        ```

5. プログラムをコンパイルしていたところ、[GHC がメモリを食い潰して OS がフリーズ](http://d.hatena.ne.jp/ku-ma-me/20130303/p1)しました。原因として何が考えられますか？（筆記解答）
6. GADTs や存在型を `let`{.haskell} や `where`{.haskell} でパターンマッチするのはなぜ NG なのかメカニズムを説明してください。（筆記解答）
7. シングルトンを用いた依存型プログラミングを利用する上で利用すべきライブラリは何ですか？また、それが GHC 7.4.1 で使えない理由を教えてください。（筆記解答）
8. JavaScriptでHTML要素をid属性の指定により取得するメソッドは何ですか？（筆記解答）

過去に実施した平均点
-----------------
この８問について、僕が出会ったエンジニアに解答してもらった平均正解数は、なんと**<span style="color: red;">８点満点中 `NaN`{.haskell} 点</span>**である。

内訳的な話
--------
うろ覚えになってしまってるだろうから満点は無理でも、ちゃんと GHC の type-level language をやってるエンジニアさんであれば4問は解けるだろう。  
筆記問題については相当甘くつけるから尚更だ。  
例えば１問目「Type Family のメリット」は、「型関数の実現および表現力の増加」ならニジュウマルだが、「関数従属性を使わないで綺麗に書けるようになる」でも「型の意味がわかりやすくなる」でも「[型関数たのしい](http://research.microsoft.com/en-us/um/people/simonpj/papers/assoc-types/fun-with-type-funs/typefun.pdf)しなんとなくかっこいい」でもマルで、それらに類する説明ならちょっと分かりにくくても、なんでも正解にしてる。

GHC がメモリ食い潰す問題は、経験していなかったから知らないから何も言えなくてもまぁいいだろう。  
（10年やってても型検査の込み入ったトラブルを経験しないなんて運がいい人なのだろうか、悪い人なのだろうか）  
「`getElementById`{.javascript}」なんてほとんど白紙解答。  
「GADTs はおまじない的に `let`{.haskell} でパタンマッチしちゃダメと思うようにしている」  
「`case`{.haskell} を使うべきと教えられたから」  
そんな状況である。１、２年目ではなく３−１０年生であり、型レベルバリバリとの触れ込みなのだ。  
Template Haskell やWeb開発を得意としている人ではない。**<span style="color: red;">型プログラマー対象</span>**なんだぜ。  
（どの会社でも型レベルプログラミングをやってればたぶん満点ばかりじゃないか。これは[関](http://research.microsoft.com/en-us/um/people/simonpj/papers/assoc-types/fun-with-type-funs/typefun.pdf) [連](http://research.microsoft.com/en-us/people/dimitris/fc-kind-poly.pdf) [論](http://www.cis.upenn.edu/~eir/papers/2012/singletons/paper.pdf) [文](http://dreixel.net/research/pdf/trkgp_nocolor.pdf)を読んでいれば当たり前の点数だろう）


これぞ現在の type-level language 業界の実態
----------------------------------------
でも私は、ここ数年 [GHC での等式証明](https://github.com/konn/equational-reasoning-in-haskell) や[依存型を用いた計算代数ライブラリ](https://github.com/konn/computational-algebra) の開発などをやっていた経験から納得がいく。これぞ「**<span style="color: red;">Haskell Platform 同梱の GHC の型レベル機能がまだまだ足りない現在の GHC の type-level language 業界の実態</span>**」なのだから。

十分な停止性チェッカーなしに、帰納法のベースケースの証明→帰納部の証明→型検査通った！という状況のなかで、それを使ったコードを書き続ける→実行してみたら無限ループに入った、という状況に山ほど出会ってきた。

**<span style="color: red;">そういう状況で、GHC で証明をバリバリ書こうというのは無茶である現実</span>**ではないか。

淘汰がない業界
------------
統計としては母数が少なすぎる（[-∞桁](http://fumieval.hatenablog.com/entry/2013/03/19/165737)程度）「し」、母集団を変えれば別の所感が得られるだろう。  
しかしながら、ヤバいのは GHC の型レベル機能がまだまだ buggy な事だ。特に GHC 7.4.1 ではリコンパイルしようとしただけで panic が起きるから、すぐに GHC で型レベルプログラミングをしようという気がなくなってしまう[^1]。  
このところ景気がいいらしいが、実家の収入が減って学費に困っている私にとってはそんなもん糞喰らえと思う。それはさておき、GHC の型レベル機能がどんどん強化されて、例えば組込みの型レベル自然数の計算がもっとリッチになるなどして、その機能をふんだんに使って開発ができたらどれだけ良いことだろう。

結果として、GHC の開発陣には余計に負担がかかるだろうなと思う。

いままで僕は楽観主義者だから、

> GHC の型レベル言語は今まで型なしだったけど、これからは型付きになっていく。  
必然的に依存型の時代になり、より堅牢なプログラムが書けるようになるだろう。

と思っていた。

しかし**<span style="color: red;">現実は、もっと凄惨な世界を経て時代が進んでいく</span>**ようだ。


元ネタ
-----
* [８つの質問で、Java SI業界の現状を知る](http://d.hatena.ne.jp/iad_otomamay/20130318/1363596244)

あわせて読みたい：

* [8つの質問で、Lazy K業界の現状を知る](http://fumieval.hatenablog.com/entry/2013/03/19/165737)
* [8つの質問で、Go業界の現状を知る](http://ymotongpoo.hatenablog.com/entry/2013/03/18/224753)
* [8つの質問で、Hadoop業界の現状を知る](http://d.hatena.ne.jp/shiumachi/20130318/1363618321)
* [8つの質問で、筋トレ業界の現状を知る](http://www.affility.co.jp/archives/1317)
* [8つの質問で、F#業界の現状を知る](https://gist.github.com/Gab-km/5194199)

[^1]: GHC 7.6.1 以降ではこうした問題は修正されており、いくぶん快適な型レベルプログラミングが行える。

