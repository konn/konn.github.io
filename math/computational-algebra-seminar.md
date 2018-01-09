---
title: 計算機代数ゼミ発表資料
date: 2014/01/24 18:52:02 JST
author: 石井大海
description: 2013年度の計算機代数ゼミの発表資料です。主にGröbner基底の基礎理論から、それを応用した消去理論、連立代数方程式の解法などについてを取り扱っています。
tag: 数学,計算機代数,代数幾何,Gröbner基底,グレブナー基底,数値計算,Groebner基底,代数学,アルゴリズム,Haskell,プログラミング
---

説明
====
2013年度の研究室ゼミで発表した計算機代数関連の資料一覧です。主にGröbner基底の基礎理論から、それを応用した消去理論、連立代数方程式の解法などについてを取り扱っています。ここでの成果は [computational-algebra](http://github.com/konn/computational-algebra) パッケージとして実装・公開しています。


資料一覧と説明
--------------

[第1回 Buchberger アルゴリズムの改良法](./computational-algebra-seminar/2013-04-07.pdf)
:    初回。Buchberger 判定法の復習などからはじめて、ナイーブな Buchberger アルゴリズムの速度を syzygy やSugar Strategy を使って改善する話題。

     * 追加資料（ベンチマーク）
	     * [ナイーブなBuchberger、syzygy判定、sugar strategyの速度比較](./computational-algebra-seminar/2013-04-07-bench-syz-sug.html )。
	     * [Sugar Strategy の原論文のデータを用いたベンチマーク](./computational-algebra-seminar/2013-04-07-bench-sugar.html)。[簡略版](./computational-algebra-seminar/2013-04-07-bench-sugar-simple.html)（グラフがわかりやすいように最後のケースだけ除外）。
         
[第2回 消去理論：消去定理と一変数版拡張定理](./computational-algebra-seminar/2013-04-15.pdf)
:    Gröbner基底を用いて多項式系から文字を消去する方法について。基礎概念である消去順序・消去型について紹介後、基本的な定理である消去定理の証明を紹介。

     * 追加資料（ベンチマーク）：[各消去順序の性能比較](./computational-algebra-seminar/2013-04-15-bench.html)。

[第3回 消去定理：部分解と完全解、一変数版拡張定理](./computational-algebra-seminar/2013-05-13.pdf)
:    前回積み残した一変数版拡張定理を示す。これにより、変数消去により得られた部分解から完全解が得られる条件などがわかる。消去型順序と消去順序に関する注意など。

[第4回 消去理論：拡張定理の別証明と応用例、消去理論の幾何的意味](./computational-algebra-seminar/2013-05-20.pdf)
:    一変数版拡張定理の特別な場合について別証明を与える。実際に与えられた連立代数方程式の解を求めてみる。後半では、消去・拡張定理の幾何的意味について説明する。完全解と部分解の間のギャップに付いて述べる閉包定理について紹介する。

[第5回 消去理論の幾何：零点定理と閉包定理](./computational-algebra-seminar/2013-06-03.pdf)
:    前回紹介した閉包定理の証明のため、Hilbert の零点定理の証明を与える。弱閉包定理の証明を行い、強閉包定理に必要となる多変数版の拡張定理の証明も紹介する。

[第6回 消去理論の幾何：閉包定理の証明](./computational-algebra-seminar/2013-06-10.pdf)
:    一般拡張定理の証明を踏まえて、強閉包定理の証明を完遂する（証明は逆向きの整礎帰納法で行われる）。代数閉体でない場合の閉包定理の反例についても紹介する。

[第7回 消去理論の応用：陰関数表示](./computational-algebra-seminar/2013-06-24.pdf)
:    前回までの消去定理を応用し、パラメタ表示された代数的集合に対し、それを定義する定義方程式を求める方法を開発する。パラメタ表示された図形と、それを含む最小の代数的集合の間のギャップについても考察する。ねじれ三次曲線や Enneper Surface、Whitney の傘などを例に出す。

[第8回 消去理論の応用：有理写像の陰関数表示](./computational-algebra-seminar/2013-07-01.pdf)
:    前回は多項式によりパラメタ表示された図形の陰関数表示を得る方法を取り扱ったが、今回はそれを有理写像に一般化する。

[第9回 消去理論の応用：代数曲線の特異点](./computational-algebra-seminar/2013-07-08.pdf)
:    消去理論を応用し、代数曲線の特異点を消去理論を用いて求める方法について考察する。

[第10回 消去理論の応用：曲線族の包絡線](./computational-algebra-seminar/2013-07-15.pdf)
:    消去理論を用いて、与えられた曲線族に対する包絡線を求める方法を開発する。包絡線とは曲線族に属する全ての曲線族と接するような曲線のことである。

[第11回 消去理論と終結式](./computational-algebra-seminar/2013-10-02.pdf)
:    終結式の概念を定義し、共通因子判定問題を解く方法などを開発し、消去理論との関係を考察する。行列式を経ずに終結式を求めるアルゴルリズムも与える。

[第12回 終結式と拡張定理](./computational-algebra-seminar/2013-10-09.pdf)
:    多変数の終結式を用いて拡張定理の別証明を与える。

[第13回 消去による連立代数方程式の解法とNewton法](./computational-algebra-seminar/2013-10-23.pdf)
:    今回から "Using Algebraic Geometry" 第二章に移る。特に、消去理論などを応用して連立代数方程式を解く方法について考察していく。今回は、今までに開発した消去定理を用いた素朴な解法と高次方程式の解法に必要となる簡単な数値計算法について紹介する。

[第14回 剰余環の構造：有限次元代数](./computational-algebra-seminar/2013-10-30.pdf)
:    より洗練された解法を開発するため、特に $V(I)$ が有限の場合の剰余環の代数（多元環）としての構造について詳しく分析していく。$k[\mathbb{X}]/I$が有限次元となる場合の条件に関する有限性定理を証明する。このようなイデアルを零次元イデアルと云う。

[第15回 零次元イデアルに関するアルゴリズム (1)](./computational-algebra-seminar/2013-11-06.pdf)
:    零次元イデアルに関するアルゴリズムについて紹介する。ここでは、各消去イデアル$I \cap k[x_i]$のモニックな生成元を求めるアルゴリズムと、零次元イデアルの根基を計算するための方法を与える。

[第16回 零次元イデアルに関するアルゴリズム (2)](./computational-algebra-seminar/2013-11-20.pdf)
:    零次元イデアルの根基と解の個数の関係や、直積環としての構造について考察する。根基の判定法についても紹介する。

[第17回 FGLMアルゴリズム：零次元イデアルのGröbner基底変換](./computational-algebra-seminar/2013-11-27.pdf)
:    消去理論を応用する際には辞書式順序が役に立つが、一般に直接辞書式順序に関する Gröbner 基底を計算するのは高価である。今回は、零次元イデアルの場合は他の順序に関する Gröbner 基底が与えられた際に、それを辞書式順序に関する基底に効率的に変換する方法を紹介する

[第18回 FGLMアルゴリズムの一般化](./computational-algebra-seminar/2013-12-04.pdf)
:    前回紹介したFGLMアルゴリズムを一般化し、像が有限次元となるような一般の線型写像に対し、その核の辞書式順序に関するGröbner基底と像のベクトル空間としての基底を計算する方法を与える。

[第19回 固有値による連立代数方程式の解法](./computational-algebra-seminar/2013-12-11.pdf)
:    前回までの知識を踏まえ、固有値問題を用いて連立代数方程式の解を求める方法について紹介する。

[第20回 固有値の計算法と左固有ベクトル法](./computational-algebra-seminar/2014-01-08.pdf)
:    固有値問題の数値的解法を紹介した後、固有値のみならず固有ベクトルも連立代数方程式の解法に大きな役割を果すことを紹介する。

[第21回 線型代数を用いた他の求解法と比較](./computational-algebra-seminar/2014-01-22.pdf)
:    前回までの方法の他に、変数の冪の張る部分空間や、第$n$座標が皆異なる場合の解法について紹介し、それぞれの結果について比較する。本来高速な筈の左固有ベクトル法が上手くいかなかった理由についても少し考察する。

     * 追加資料（ベンチマーク）：[各手法の速度比較](./computational-algebra-seminar/2014-01-22-bench.html)。

参考文献
========
* D. Cox, J. Little, and D. O'Shea. [Ideals, Varieties, and Algorithms: An Introduction to Computational Algebraic Geometry and Commutative Algebra](asin:0387356509). 3rd. Springer-Verlag, 2007.
* D. Cox, J. Little, and D. O'Shea. [Using Algebraic Geometry](asin:0387207333). 2nd. Springer, 2005.
* Alessandro Giovini et al. ""One sugar cube, please" or Selection strategies in the Buchberger algorithm". In: Proceedings of the ISSAC'91, ACM Press. 1991, pp. 5-4.
* 楫元. "グレブナー基底は面白い！ ──「代数幾何学入門」への入門──". 講義録. 2013.

