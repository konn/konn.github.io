---
title: Pos 圏の射影的対象と選択公理
author: 石井大海
description: 「Pos圏における射影的対象のなす充満部分圏が集合圏と同型である」ことと選択公理の同値性の証明。
tag: 圏論,集合論,選択公理,ロジック,数理論理学,数学基礎論
date: 2012/03/02 23:17:00 JST
---

概要
====

Awodey [@Awodey:2010] の演習問題を解いていたところ，選択公理と同値な
命題を見付けたので証明を試みました．手許の文献でこの同値性に言及している
ものはなかったと思います．

準備
====

以下では，圏 $\mathbf{Pos}$ とは，半順序集合（partially ordered
set，以下poset）を 対象，その間の単調写像を射とした圏であるとします．
恒等写像は明らかに単調ですし，単調写像と単調写像の合成が再び単調となるこ
とからこれは実際に圏となります．

<div class="definition">
圏 ${\bf C}$ の対象 $P$ が**射影的**(projective)であるとは，任意の
対象 $E, X \in {\bf C}$ と射 $f: P \to X$ およびエピ射 $e: E \twoheadrightarrow X$が与えら
れたとき，次の図式を可換にする（一意とは限らない）射 $\hat{f}: P \to E$
が必ず存在することである．

$$
\begin{xy}
\xymatrix{
  & E \ar @{->>} [d]^{e} \\
  P \ar @{.>} [ur]^{\hat{f}} \ar [r] _{f} & X
}
\end{xy}
$$
</div>

射影的対象の概念を用いて，選択公理を言い換えたものが以下です．

<div class="theorem">
以下の命題は選択公理と同値．

> 圏 $\mathbf{Sets}$ の任意の対象は射影的である．
</div>

<div class="proof">
@alg-d さんのサイト [@alg-d]を参照．
</div>

<div class="theorem">
圏 $\mathbf{Pos}$ のエピ射は全射単調写像と完全に一致する．
</div>

<div class="proof">
$e$ がエピ射でないとすると，$he = h'e$ かつ
$h \neq h'$ なる射が存在し，
$e$ の定義域の外に $h(x) \neq h'(x)$ なる $x$ が存在することになり全射となら
ない．また，エピ射かつ全射でない単調写像 $e$ が存在したとすると，$e$ 
の定義域の 外から一点を取り，その行き先を上手く違えた $h, h'$
を取ってやることで矛 盾を導くことが出来る．
</div>

また，以下では次の事実を用いる．

<div class="fact">
通常の集合 $S$ は，離散poset，即ち，各元 $x \in S$ についての反射律のみ
を仮定して得られる poset と見做すことで $\mathbf{Pos}$
の対象と見做すことが出来る．
</div>

$\mathbf{Pos}$ と $\mathbf{Sets}$ の関係
=====================================

<div class="theorem">
以下の命題は選択公理と同値．

集合を離散 poset と見做すことによって， $\mathbf{Sets}$ は $\mathbf{Pos}$ の射影的対象 からなる充満部分圏となる．
</div>

<div class="proof">
1.  集合の間の写像は，対応する離散 poset の間の単調写像と見做すこと
    が出来るので，結局は $\mathbf{Pos}$ の射影的対象と離散posetが一致すること
    を示せばよい．

    まず，任意の集合 $S$ は $\mathbf{Pos}$ で射影的であることを示そう．$S$ を集合，
    即ち離散poset，$E, X$ を任意のposetとし，単調写像 $f: S \to X$ およ
    びエピ射 $e: E \twoheadrightarrow X$ が与えらえているとする． 今，poset
    $A$ に対応する台集合を $|A|$，単調写像 $f$ の台写像を
    $|f|$ で表わすことにすると，定理 2 より $\mathbf{Pos}$ での
    エピ射は全射でもあることに注意すると，$\mathbf{Sets}$ で次の図式を可換に
    する射 $\hat{f}: |S| \to |E|$ が存在する．

	$$
    \begin{xy}
	\xymatrix{
	  & \left|E \right| \ar @{->>} [d]^{e} \\
	  \left|P\right| \ar @{.>} [ur]^{\hat{\left|f\right|}} \ar [r] _{\left|f\right|} & \left|X\right|
	}
	\end{xy}
	$$

    あとは，$|\hat{f}|$ が単調写像となっていることを示せばよい．$S$ は
    離散posetより，成立する順序関係は反射律のみであるので，特に
    $|\hat{f}|(a) \leq |\hat{f}|(a)$ が云えればよい．しかるに，$E$ は
    posetであったので反射律が成立し，特に $|\hat{f}|(a) \leq
        |\hat{f}|(a)$ が常に成立している．よって $|\hat{f}|$ は単調写像．
    よって状況を $\mathbf{Pos}$ に引き戻して

	$$
    \begin{xy}
	\xymatrix{
	  & E \ar @{->>} [d]^{e} \\
	  P \ar @{.>} [ur]^{\hat{f}} \ar [r] _{f} & X
	}
	\end{xy}
	$$

    が可換となる．よって任意の集合は $\mathbf{Pos}$ で射影的である．

    逆に，$P \in \mathbf{Pos}$ を射影的対象とする．$P$ の元からなる離散posetを
    $\mathrm{dis}(P)$ と書くことにする．写像
    $i:\mathrm{dis}(P) \twoheadrightarrow P$ を
    $i(a) = a$ で定めると，これはは明らかに全射単調写
    像であり，従って $\mathbf{Pos}$ のエピ射である．よって，以下を可換にするよ
    うな $\hat{1}_P: P \to \mathrm{dis}(P)$ が存在する．

	$$
    \begin{xy}
	\xymatrix{
	  & \mathrm{dis}(P) \ar @{->>} [d]^{i} \\
	  P \ar @{.>} [ur]^{\hat{1_P}} \ar [r] _{1_P} & P
	}
	\end{xy}
	$$

    すなわち，$i \circ \hat{1}_P = 1_P$ である．特に $|P| = |\mathrm{dis}(P)|$ 
    であり，定義から $|i| = 1_{|P|} = 1_{|\mathrm{dis}(P)|}$ となる．すると，

    $$\begin{aligned}
         | \hat{1}_P \circ i | &= |\hat{1}_P| \circ |i|
                                = |\hat{1}_P| \circ 1_{|P|}
                                = |\hat{1}_P|\\
                               &= 1_{|P|} \circ |\hat{1}_P|
                                = |i| \circ |\hat{1}_P|
                                = |i \circ \hat{1}_P| \\ &= |1_P|
                                = 1_{|P|} = 1_{|\mathrm{dis}(P)|} = |1_{\mathrm{dis}(P)}|\\
         \therefore \hat{1}_P \circ i &= 1_{\mathrm{dis}(P)}\;\;\text{in}\ \mathbf{Pos}\end{aligned}$$

    よって，$\hat{1}_P$ は $\mathbf{Pos}$ での同型射となる．今，$\mathrm{dis}(P)$ は離散
    poset だったので，それと同型となる $P$ もまた離散posetとなる．

    以上より示された．

2.  $\mathbf{Pos}$ の射影的対象と離散posetが一致すると仮定する．今，Factより
    任意の集合 $A$ は離散posetと同一視出来る．すると，A
    は $\mathbf{Pos}$ で射影
    的であり．他の集合 $E, X$ も $\mathbf{Pos}$ の対象と見做せ，その間の全射
    $e: E \to X$ と写像 $f: A \to X$ が存在すれば，それらはposetとして
    の $A, E, X$ 間の単調写像と同一視出来，とくに $e$ は $\mathbf{Pos}$ でエピ射と
    なる．すると，それらに対して下の図式を可換にする $\hat{f}:A\to E$
    が存在する．

	$$
    \begin{xy}
	\xymatrix{
	  & E \ar @{->>} [d]^{e} \\
	  A \ar @{.>} [ur]^{\hat{f}} \ar [r] _{f} & X
	}
	\end{xy}
	$$

    再び $\mathbf{Sets}$ に戻って考えれば，これは任意の集合は射影的であると云
    うことであり，定理 1 より選択公理が従う．
</div>
