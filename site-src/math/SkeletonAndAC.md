---
title: 圏の骨格と選択公理
author: 石井大海（早稲田大学基幹理工学部数学科二年[^author]）
description: 圏の骨格の存在定理が選択公理と同値であることの証明。
tag: 圏論,集合論,選択公理,ロジック,数理論理学,数学基礎論
date: 2012/03/04 23:17:00 JST
---


要旨
====

選択公理と同値な命題として，圏論における骨格の存在定理を採り上げる．
そのため，まず必要となる圏の知識を概説し，それから定理と選択公理の同値性証明に入る．
定理の存在自体は英語版 Wikipedia [@WikiChoice]の記事から見付けてきた．

圏の骨格の定義は MacLane [@MacLane:2005] および檜山 [@Hiyama] に拠る．
骨格の存在証明は，nLab [@nLab] および Awodey [@Awodey:2010] を参考にし
たが，これらの主眼は骨格ともとの圏の同値性であり，また nLab での骨格の定義
は我々の採用しているものと異なるので，ここで紹介する証明はこれらとは若干
異なるか簡略化されたものとなっている．逆に，骨格の存在定理から選択公理を
導く証明は nLab の方に載っていたものを，より詳細に厳密に書き直したものを
掲載してある[^1]．

前提知識
========

議論に入る前に，圏論の知識と選択公理についての幾つかの確認を行っておく．


圏論
----

ここで扱うのは圏論の命題であるので，簡単に圏論についての導入を行っておく．

<div class="definition" name="圏">
圏 ${\bf C}$ とは，

-   **対象**： $A, B, C,\ldots$ 

-   **射**： $f, g, h,\ldots$ 

の二つの構成要素からなり，以下の条件を満たすものである．

1.  任意の射  $f$  について，**ドメイン**・**コドメン** と呼ばれる対象
     $\mathrm{dom}(f),  \mathrm{cod}(f)$  がそれぞれ一意に与えられている．
    特に， $A = \mathrm{dom}(f), \,B = \mathrm{cod}(f)$ であるとき，
     $$f: A \to B$$  とかく．

2.   $f: A \to B, g: B \to C$ なる任意の射 $f, g$ に対し，その
    **合成射**と呼ばれる射  $$g \circ f : A \to C$$  が与えられている．

3.  任意の対象 $A$に対し，**恒等射** $1_A$  が存在し，射の合成
    について単位元となる．すなわち，次が成立する．
     $$f \circ 1_A = 1_B \circ f = f\ \ \ (\forall f: A \to B)$$ 

4.  射の合成の結合律が成立する．即ち，任意の $f: A \to B, g: B \to C, h: C \to D$ に対し，
		$$(h \circ g) \circ f = h \circ (g \circ f)$$ 
    である．以下，三つ以上の射の合成は括弧を省略し $h \circ g \circ
        f$ と書く．

特に混乱のない場合， $A$が${\bf C}$の対象であることを$A \in {\bf C}$  と
書く．また，射についても同様に  $f \in {\bf C}$ などと書いたりする．また，
圏は対象・射と区別するために太字で書かれる．
</div>

圏はモノイドや群，あるいは順序集合の概念を一般化したものである．圏はそれ
自身，射や対象と云った言葉を無定義述語と見做すことで一階述語論理上の理論
と見做すことが出来るが，ここでは集合論に翻訳したものを使う．つまり，圏
 ${\bf C}$ は，対象の**集まり**$C_0$と射の集まり$C_1$ からなるもの
と理解する．ここで「集まり」としたのは，圏論で扱う対象は一般的に大きく，
時として射や対象の全体が集合とならない場合がある為である．対象の全体も射
の全体も集合となっている圏を，**小さな圏**と云う．以下で単に圏と
云った場合，小さな圏を指すことが多い．また，以下では ${\bf C}$ の対象の集
まりを  $C_0 = |{\bf C}|$  と書くことがある．

さて，圏はモノイドや順序集合の一般化であると述べた．では，モノイド準同型
や順序集合に対する単調写像に対応するものは何か？それは**函手**で ある．

<div class="definition" name="函手">
圏 ${\bf C}$ から ${\bf D}$ への**函手** $F$ とは，写像
 $F_0: C_0 \to D_0,\,F_1: C_1 \to D_1$  の組であり，次を満たすものである．

1.  $\forall f: A \to B \in {\bf C}\,\left[F_1(f: A \to B) = F_1(f): F_0(A) \to F_0(B)\right]$

2.   $\forall A \in {\bf C}\,\left[F_1(1_A) = 1_{F_0(A)}\right]$ 

3.  $\forall f: A \to B\, \forall g:B \to C \in {\bf C}\,\left[ F_1(g \circ f) = F_1(g) \circ F_1(f)\right]$

大抵の場合，函手の対象・射に対する写像は区別せず，単に $F$ で表わす．例え
ば最初の条件式は，  $$F(f: A \to B) = F(f) : F(A) \to F(B)$$  などと書く．
また，函手 $F:{\bf C} \to {\bf D}$ が**充満函手**であるとは， 任意の
 $A, B \in {\bf C}$ および $g: F(A) \to F(B) \in {\bf D}$  に対し，
ある $f: A \to B \in {\bf C}$ が存在して，
 $$F(f:A \to B) = g: F(A) \to F(B)$$  となることである．
</div>

各 $F_0, F_1$ の写像を合成することにより，函手の合成を定義できる．また，任
意の圏 ${\bf C}$に対して，恒等函手$1_{\bf C}$ が存在する．以上より，すべて
の（小さな）圏を対象とし，とその間の函手を射とする（大きな）圏  $\mathbf{Cat}$ 
が 得られる．

群やモノイドに対する部分群・部分モノイドを考えることが出来るように，圏に
対しても**部分圏**を考えることが出来る．

<div class="definition" name="部分圏，充満部分圏">
 ${\bf D}$ が圏 ${\bf C}$ の部分圏であるとは，対象の集まり $D_0 \subseteq C_0$ 
と射の集まり $D_1 \subseteq C_1$ を持ち， ${\bf D}$ の任意の対象に対する恒等射が存在し，かつ射が合成について閉じていることである．
</div>

 ${\bf D}$ を ${\bf C}$の部分圏とすると，${\bf D}$ の対象・射をそのまま
 ${\bf C}$ へと移す自明な**包含函手**${\bf D} \to {\bf C}$ が存在
することがわかる．特に，この包含函手が充満函手であるとき，即ち ${\bf D}$ に
含まれる対象の間の射が ${\bf C}$のものと一致するとき，${\bf D}$ は
**充満部分圏**である，と云われる．

<div class="definition" name="同型">
射  $f: A \to B \in {\bf C}$  が**同型射**である，とは
$$\exists g: B \to A \in {\bf C}
   \ \ {\rm s.t.}\ \ g \circ f = 1_A,\; f \circ g = 1_B$$
となることである． $g$ は一意に定まるので$f^{-1}$ と書く．また，この時対
象 $A$ と $B$ は**同型**であると云い，$A \cong B$ と書く．

明らかに，任意の恒等射は同型射である．恒等射のように，ドメインとコドメ
インが同じ同型射を特に自己同型射と云う．同型射が自己同型射のみである
ような圏は**骨格的** （skeltal）であると呼ばれる．

また， ${\bf C}$の任意の対象が部分圏${\bf D}$ の対象と同型であるとき，
 ${\bf D}$ は**稠密な**部分圏であると云う．
</div>

以下の議論では，ある圏内での同型に加え，さらに圏 $\mathbf{Cat}$ における同型，つま
り対象を小さな圏とし射を函手とみたときの同型，圏同型を考える．

さて，本稿の主題である骨格の定義は次で与えられる．

<div class="definition" name="骨格">
圏 ${\bf C}$の稠密な充満部分圏であって骨格的であるような圏を，${\bf C}$  の
**骨格**（skeleton）と呼ぶ．
</div>

詳しくは立ち入らないが，英語版 Wikipedia [@WikiSkel]から一部抜粋した
骨格の例を以下に幾つか挙げる．

1.  集合全体を対象とし，その間の写像を射とする（大きな）圏 $\mathbf{Sets}$ の同
    型射は全単射写像である．したがって，その骨格は基数全体を対象とす
    る部分圏である．

2.   $\mathbb{R}$ 上ベクトル空間を対象，その間の線型写像を射とする圏 ${\bf Vect}_\mathbb{R}$
    の骨格は，任意次元のユークリッド空間を対象とする部分圏であ る．

3.  整列集合と単調写像からなる圏の骨格は，順序数を対象とする部分圏で
    ある．

4.  プレ順序集合を圏と見做すと，その骨格は半順序集合となる．

選択公理
--------

会の趣旨から，選択公理の主張そのものについては既知とする．選択公理につい
ては様々な同値な言い換えが存在するが，ここでは，下記の定義を採用すること
にする．

<div class="axiom" name="選択公理">
任意の非空集合の族 $(X_\lambda)_{\lambda \in \Lambda}$ について，その直積
 $$\prod_{\lambda \in \Lambda} X_\lambda = \left\{ f: \Lambda \to \bigcup_{\lambda \in \Lambda} X_\lambda\ \ \middle|\ \ f(\lambda) \in X_\lambda\right\}$$
 は空ではない．
</div>

### 選択公理の圏論的な言い換え

圏論において選択公理はどう表現されるのか？その紹介の為に更に幾つかの概念
を紹介しておきたい．

<div class="definition" name="モノ射，エピ射"> 圏  ${\bf C}$ の射 $m: A \to B$ 
が**モノ射**であるとは， $m$ が左 簡約可能であること，即ち
 $$m \circ f = m \circ g \Rightarrow f = g\ \ \forall f, g: C \to A$$ 
が成立することである．このとき  $m: A \rightarrowtail B$ と書く．

また，射 $e: A \to B$が**エピ射**であるとは，$e$ が右簡約可能であ
ること，即ち
 $$f \circ e = g \circ e \Rightarrow f = g\ \ \forall f, g: B \to C$$ 
が成立することである．このとき  $e: A \twoheadrightarrow B$  と書く．
</div>

集合圏 $\mathbf{Sets}$ でのエピ射，モノ射はそれぞれ全射，単射と一致する．
これを使って，集合の要素に触れずに射や対象の言葉だけを用いた，選択公理の
圏論的な言い換えが出来る．

特に，次の言い換えを以後の証明では用いる．

<div class="prop">
以下の命題は選択公理と同値．

 $\mathbf{Sets}$ の任意のエピ射は右逆射を持つ．
</div>

<div class="proof">
1.  今， $e: A \twoheadrightarrow B$ を任意の全射とする．このとき選択公理を仮
    定して， $e$の右逆写像$s: B \to A$ が存在し
     $e \circ s = 1_B$ となることを 示す．今，集合族
     $(E_b)_{b \in B}$を次で定める． $$E_b = e^{-1}(\{b\})$$ 
    ここで， $e$は全射より各$E_b$ は空ではない．よって選択公理より，
     $$\prod_{b \in B} E_b \neq \emptyset$$  となる．そこで
     $s \in \prod_{b \in B} E_b$  を一つ取れば，
     $$s: B \to \bigcup_{b \in B} E_b = A,\;\; s(b) \in E_b\ \forall b \in B$$ 
    であり，

    $$\begin{aligned}
         (e \circ s)(b) \in e(E_b) &= e(e^{-1}(\{b\})) = \{b\}\\
         \therefore (e\circ s)(b) &= b,\,\,\forall b \in B\\
         \therefore e \circ s &= 1_B
        \end{aligned}$$

    よって， $e$の右逆写像$s: B \to A$ が存在する．

2.  任意の全射 $e: A \twoheadrightarrow B$に対し右逆写像$s: B \to A$ 
    が存在するとす る．今，非空集合の族
     $(A_\lambda)_{\lambda \in \Lambda}$ が与えら れたとき，集合 $A$ 
    を次で定める．
     $$A = \left\{\ \langle \lambda,  x \rangle\ \ \middle|\ \ \lambda \in \Lambda, x \in A_\lambda\ \right\}$$ 
    ここで，写像

	$$
	  f:  A \to \Lambda; \langle \lambda; x \rangle \mapsto \lambda
	$$

    を考えると，明らかにこれは全射となっている．仮定より，この右逆写像
     $s:\Lambda \to A$ が存在し，$f \circ s = 1_A$ となる．そこで，
     $$h = \pi_2 \circ s: \Lambda \to \bigcup_{\lambda \in \Lambda}A_\lambda$$ 
    とすれば， $A$の定義より特に$h(\lambda) \in A_\lambda$  となるので，
    これは正に族 $(A_\lambda)_{\lambda \in \Lambda}$ に対する選択函数であり，従って
     $$\prod_{\lambda \in \Lambda} A_\lambda \neq \emptyset$$ 

以上より，選択公理の同値性が示された．
</div>

この言い換えは，初等トポスによって集合を圏論的に定義する際に，選択公理に相当する要請として採用される，らしい．
らしい，と云うのはそういう話が MacLane [@MacLane:2005] や竹内 [@Takeuti:1978] に書いてあるのをちらっと読んだことがある，と云う程度の意味で，実際きちんと理解していると云う訳ではない，と云う意味である．

### 完全代表系

圏の骨格の存在証明において，選択公理が本質的な役割を演じるのは対象の同型
関係に関する**完全代表系**を選ぶ場面である．そこで簡単の為，選択
公理から完全代表系の存在を証明しておく．

<div class="definition" name="完全代表系">
 $S$を集合，$\sim$を$S$上の同値関係とする．このとき，$S\supseteq A$ が $S$ 
における $\sim$ の**完全代表系**であるとは，同値関係による自然な 射影
 $\pi: S \twoheadrightarrow S/\sim$ 
について，その $A$への制 限 $\pi|_A : A \to S/\sim$ 
が全単射となるこ とである．
</div>

<div class="prop">
選択公理の下で，任意の集合  $S$とその上の同値関係$\sim$ に対して，完全代
表系  $A \subseteq S$ を取ることが出来る．
</div>
<div class="proof">
今， $\pi: S \twoheadrightarrow S/\sim$ は全射であり，特に選択公理を仮定してい
るので命題[epi splits]より右逆写像
 $s: S/\sim \to S$  が 存在し，
 $\pi \circ s = 1_{S/\sim}$となる．また，特に$s$ は単射
である．

そこで， $A = s(S/\sim) \subseteq S$  ととり，制限写像
 $\pi|_A:A \to S/\sim$  が全単射となることを示す．

全射性.
:    $[c] \in S/\sim$  について，

    $$\begin{aligned}
              \pi|_A(s([c])) &= \pi(s([c])) = (\pi \circ s)([c])\\
                             &= 1_{S/\sim}([c]) = [c]
             \end{aligned}$$

    となるので，明らかに全射である．

単射性.
:    $a, a' \in A = s(S/\sim)$ で $a \neq a'$ 
    とする．
    このとき， $\exists [b], [b'] \in S/\sim \ \ {\rm s.t.}\ \ a = s([b]), \, a' = s([b'])$ 
    と出来るから， $s$ の単射性より $[b] \neq [b']$となる．他方、$s$ は
     $\pi$ の右逆写像であった から、

    $$\begin{aligned}
              \pi|_A(a) &= \pi(s([b]))
                        = (\pi \circ s)([b]) = 1_{S/\sim}([b])\\
                        &= [b] \\
                        &\neq [b'] \\
                        &= 1_{S/\sim}([b'])
                        = (\pi \circ s)([b']) = \pi(s([b']))\\
                        &= \pi|_A(a')
             \end{aligned}$$

    となり，従って $\pi|_A$ は単射である．

以上より、制限写像 $\pi|_A: A \to S/\sim$が全単射となるので，$A$ 
は完全代表系となる．
</div>

圏の骨格
--------

それではいよいよ，骨格の存在定理と選択公理の同値性を証明する．

<div class="theorem" name="骨格の存在定理">
次の命題は選択公理と同値である．

> 任意の（小さな）圏 ${\bf C}$ に対し，その骨格が存在する．
</div>

<div class="proof">
1.  ${\bf C}$を小さな圏とする．今，${\bf C}$の対象全体の集合$C_0$ に，
    同型による同値関係を入れる．すると，第[representatives]節の
    Claimより完全代表系をとれるので，その内の一つを  $D_0 \subseteq C_0$ 
    とする．ここで，圏 ${\bf D}$ を，$D_0$ の元を
    対象とする ${\bf C}$の充満部分圏とする．即ち，${\bf D}$ に属する任
    意の二つの対象について， ${\bf C}$ でその二つの間に存在する射を全
    て取ってきて ${\bf D}$ の射とする．これは明らかに恒等射を含み，合
    成について閉じているので部分圏であり，充満性も明らかである．
    また， $D_0$の取り方から，${\bf C}$の対象は必ず${\bf D}$ に同型な対
    象を持つので， ${\bf D}$ は稠密である．

    さて， $A, B \in {\bf D},\, A \cong B$  とする．このとき同値類の取
    り方から  $A \in \pi|_{D_0}(A)$ かつ $B \in \pi|_{D_0}(A)$ となる．
    従って  $\pi|_{D_0}(A) \cap \pi|_{D_0}(B) \neq \emptyset$  であり，
    特に各  $\pi|_{D_0}\langle A),  \pi|_{D_0}(B \rangle$ は同値類であるので，同値類
    の性質から  $\pi|_{D_0}(A) = \pi|_{D_0}(B)$ となる．今，
     $\pi|_{D_0}$は全単射より，従って$A = B$．ゆえに，${\bf D}$  の同
    型な対象は全て自分自身に限られるので， ${\bf D}$ は骨格的である．

    今， ${\bf D}$は${\bf C}$ の稠密な充満部分圏であったから，これと併
    せて ${\bf D}$は ${\bf C}$ の骨格となる．

2.  ここでは，直接選択公理を示すのではなく，命題[epi splits]を示
    す．すなわち， $e: A \twoheadrightarrow B$  が与えられているとき，
     $\exists s:B \rightarrowtail A \ \ {\rm s.t.}\ \ e \circ s = 1_B$ 
    とできることを示す．

    ここで， $e$に対し圏${\bf C}_e$ を次で定める．

    対象.
    :   各 $a \in A$ に対し $\langle e(a),  a \rangle$  の形の順序対．

    射.
    :   $\exists ! \langle e(a),  a \rangle \to \langle e(a'),  a' \rangle \iff e(a) = e(a')$ とする．

    恒等射.
    :    $e(a) = e(a)\ \forall a \in A$  より上の射の
        定めかたから一意に存在する  $\langle e(a),  a \rangle \to \langle e(a),  a \rangle$ 
        を $\langle e(a),  a \rangle$ の恒等射とする．

    合成.
    :   二つの射 $\langle e(a),  a \rangle \to \langle e(b),  b \rangle,\,\langle e(b),  b \rangle \to \langle e(c),  c \rangle$ があれば，定義より $e(a) = e(b) = e(c)$ と
        なり射 $\langle e(a),  a \rangle \to \langle e(c),  c \rangle$  が一意的に存在するの
        で，これを合成射とする．

    射の一意性からただちに単位律・結合律は従うので，これは圏となる．
    また特に，任意の2対象の間の射は明らかに同型射となっている．何故
    ならば， $e(a) = e(b) \iff e(b) = e(a)$  より，射
     $\langle e(a),  a \rangle \to\langle e(b),  b \rangle$ が存在すれば $\langle e(b),  b \rangle \to \langle e(a),  a \rangle$ 
    が存在し，これらの合成は定義から
     $\langle e(a),  a \rangle \to \langle e(a),  a \rangle,\,\langle e(b),  b \rangle \to \langle e(b),  b \rangle$ となるが，こ
    れらもまた射の一意性より恒等射となるためである．

    特に， $A, B$は集合であるので，${\bf C}_e$  は小さな圏となる．よっ
    て，骨格の存在定理が使え， ${\bf C}_e$ の骨格を取れる．そこで，
    その一つを $\rm{sk}({\bf C}_e)$ とし，写像 $s: B \rightarrowtail A$ の
    グラフを
	$$
	  G(s) = |\mathrm{sk}({\bf C}_e)|
              = \Set{ \langle b,  a \rangle \in \mathrm{sk}({\bf C}_e)}
              \subseteq B \times A
    $$
    で定める．これにより $s$ が実際に写像となることを見る．

    まず， $e: A \twoheadrightarrow B$は全射より，$\forall b \in B$ 
    に対し  $e(a) = b\ \ (\exists a \in A)$ と出来，
     $\langle b,  a \rangle \in {\bf C}_e\ \ (\exists a \in A)$ が成立する．今，
     ${\rm sk}\langle {\bf C}_e)$は稠密であるので，${\bf C}_e$の対象$(b,  a \rangle$ 
    は必ず ${\rm sk}({\bf C}_e)$ 内に同型な対象を持つ．従って，任意の
     $b \in B$ について
     $$\langle b,  a \rangle \in {\rm sk}({\bf C}_e)\ \  (\exists a \in A)$$ 
    としてよい．

    そこで， $b\in B$  を任意にとり，
     $$\langle b,  a \rangle, \langle b,  a' \rangle \in {\rm sk}({\bf C}_e)$$ 
    とする．今， ${\rm sk}({\bf C}_e)$ は ${\bf C}_e$ 
    の部分圏であるので，対象の定め方から  $e(a) = b = e(a')$ となる．
    よって定義より同型射 $\langle b,  a \rangle \xrightarrow{\sim} \langle b,  a' \rangle$ が存在
    する．今，特に ${\rm sk}({\bf C}_e)$  は骨格的であるので，
     $\langle b,  a \rangle = \langle b,  a' \rangle$となり，従って $a = a'$ が云える．よって，グラフ
     $G(s)$ は任意の $b \in B$に対して唯一つの $a \in A$ を対応づける．
    従って， $s: B \to A$  は写像となる．

    すると， $s$ の定義より特に $\langle b,  s(b \rangle) \in {\bf C}_e$  となり，
     ${\bf C}_e$の定義から $b = e(s(b))\ \ \forall b \in B$ となる．
    よって，  $e \circ s = 1_B$となるので，$s$ が $e$ の右逆写像となってい
    ることが示せた．

以上より，圏の骨格の存在定理と選択公理の同値性が示された．
</div>

[^1]: 最初載っているのに気付かずに自力で証明していた．悲しい
