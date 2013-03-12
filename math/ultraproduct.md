---
title: 超積によるコンパクト性定理の証明と超準モデル ──君の知らない自然数──
author: 石井大海
date: 2013/03/09 00:00:00 JST
description: 超積によるコンパクト性定理の証明と、無限大を含む自然数の超準モデルの構成法について。
tag: モデル理論,超積,自然数,超準モデル,コンパクト性定理,ウルトラフィルター,ウルトラプロダクト,数学,ロジック,数理論理学,数学基礎論
---

[PDF版](./ultraproduct.pdf)

はじめに
========

超積によるコンパクト性定理の証明や超準モデルの構成が面白いので紹介します．超積とは，L-構造の族
$\{\mathfrak{A}_i\}_{i\in I}$
が与えられたときに，その「殆んど至るところ」で成立するような性質を持つ
L-構造を作る方法です．

フィルターとウルトラフィルター
==============================

超積を定義するため，まずウルトラフィルターの概念を定義する．任意のブール代数上で定義される概念だが，ここでは冪集合束上のもののみ考えれば十分なため，それに限定して話を進める．

<div class="definition" name="フィルター，ウルトラフィルター">
$\mathcal{F} \subseteq \mathfrak{P}(I)$
が次の三条件を満たすとき，集合 $I$
上の**フィルター**（*filter*）であると云う．

(1) <a id="ref-eigen"></a>$I \in \mathcal{F}, \emptyset \notin \mathcal{F}$

(2) <a id="filter-upper-closed"></a>$X \in \mathcal{F}, X \subseteq Y \Rightarrow Y \in \mathcal{F}$

(3) <a id="filter-meet"></a>$X, Y \in \mathcal{F} \Rightarrow X \cap Y \in \mathcal{F}$

更に $\mathcal{F}$ が次の条件を満たすとき，$I$
上の**ウルトラフィルター**（*ultrafilter*）であると云う．

(4) <a name="ultra"></a>$X \in \mathcal{F}$ または $I \setminus X \in \mathcal{F}$
</div>

(<a href="#ref-eigen">1</a>) の $\emptyset \notin \mathcal{F}$
の条件は，$\mathcal{F}$ が $\mathfrak{P}(I)$
と一致しないという事だ．$\mathfrak{P}(I)$
をフィルターに含める場合，$\mathfrak{P}(I)$
と異なるフィルターを固有フィルター（proper
filter）と呼ぶことがある．以下では固有フィルターのみを考える．

<div class="prop">
$\mathcal{U} \subseteq I$ がウルトラフィルター
$\Leftrightarrow \mathcal{U}$
は極大なフィルター．即ち，$\mathcal{U} \subseteq \mathcal{F}$
なるフィルター $\mathcal{F}$ が存在するなら，$\mathcal{U} = \mathcal{F}$
となる．
</div>
<div class="proof">
(⇒) を示す．$\mathcal{U}$ を $I$
上のウルトラフィルターとする．$\mathcal{U} \subseteq \mathcal{F}$
なるフィルター $\mathcal{F}$
があったとする．$\mathcal{F} \neq \mathcal{U}$
とすると，$X \in \mathcal{F} \setminus \mathcal{U}$ をとれば
$X \notin \mathcal{U}$ である．すると，条件 (<a href="#ultra">4</a>) より
$I \setminus X \in \mathcal{U} \subset \mathcal{F}$
となる．$X, I\setminus X\in \mathcal{F}$ なので，条件
(<a href="#filter-meet">3</a>) より
$\emptyset = X \cap (I \setminus X) \in \mathcal{F}$
となるが，これは条件 (<a href="#ref-eigen">1</a>) に反する．よって
$\mathcal{U} = \mathcal{F}$ となるので，$\mathcal{U}$
は極大フィルターである．

(⇒) を示す．$\mathcal{M}$ を $I$
上の極大フィルターであるとする．$I \setminus X \notin \mathcal{M}$
として，$X\in\mathcal{M}$ を示せば十分である．そこで，フィルター
$\mathcal{F}$ を次で定める．
$$\mathcal{F} \mathrel{:=}\left\{ Y \subseteq I\ \ \middle|\ \ \exists Z \in \mathcal{M}\ [X \cap Z \subseteq Y]\right\}$$
$Z \in \mathcal{M}$ とすれば，$X \cap Z \subseteq Z$ であるので，
$\mathcal{M} \subseteq \mathcal{F}$ である．$\emptyset \in \mathcal{F}$
とすると，$\exists Z \in \mathcal{M}\ [X \cap Z \subseteq \emptyset]$
となり，これは $Z \subseteq I \setminus X$
と同値であるので，$I \setminus X \in \mathcal{M}$ となってしまう．よって
$\emptyset \notin \mathcal{F}$．同様に (<a href="#filter-meet">3</a>)
も証明出来，$\mathcal{F}$ は $\mathcal{M}$
を含むフィルターであることが判る．よって $\mathcal{M}$ の極大性より
$\mathcal{F} = \mathcal{M}$ である．また，任意の集合 $A$ について
$X \cap A \subseteq X$ であるので
$X \in \mathcal{F} = \mathcal{M}$．よって $\mathcal{M}$
はウルトラフィルターである．
</div>

ウルトラフィルターには同値な特徴付けがあり，『ゲーデルと20世紀の論理学』<sup><a href="#bib-Tanaka">[3]</a></sup>などでは次のものを定義として採用している．

<div class="prop">
$\mathcal{U} \subseteq \mathfrak{P}(I)$ が $I$
上のウルトラフィルターである必要十分条件は，次が成立することである．

(1) $\mathcal{U}$ は有限交叉性を持つ．即ち， $\mathcal{F}$
    の任意の有限部分集合 $F \Subset \mathcal{U}$
    に対し，$\cap F \neq \emptyset$．

(2) $\mathcal{U}$ は有限交叉性を持つ $I$
    上の冪集合族の中で極大である．即ち，$\mathcal{U}$ に属さないような
    $X \subseteq I$ を取ってくると，$\mathcal{U} \cup \left\{X\right\}$
    は有限交叉性を持たない．
</div>
<div class="proof">
フィルターが有限交叉性を持つことは明らかである．ウルトラフィルター
$\mathcal{U}$ に属さない元を $X$
とすると，$I \setminus X \in \mathcal{U}$
であるので，$\mathcal{U} \cup \left\{X\right\}$
は有限交叉性を持たない．よって，ウルトラフィルターは有限交叉性を持つ極大な部分集合族である．

逆に $\mathcal{U}$
を極大な有限交叉的部分集合族であるとする．$\mathcal{U}$
がフィルターであることを示せば，任意のフィルターが有限交叉性を持つことから極大性は従う．(<a href="#ref-eigen">1</a>)
は明らか．$X \in \mathcal{U}$ かつ $X \subseteq Y$
とする．$A_1, \dots, A_n \in \mathcal{U}$ とすると，$\mathcal{U}$
の有限交叉性から
$Y \cap A_1 \cap \dots \cap A_n \supseteq X \cap A_1 \cap \dots A_n \neq \emptyset$．よって
$\mathcal{U} \cup \left\{Y\right\}$ は有限交叉性を持ち，特に $\mathcal{U}$
は極大であるので $Y \in \mathcal{U} \cup \left\{Y\right\} = \mathcal{U}$．よって
(<a href="#filter-upper-closed">2</a>) は成立．(<a href="#filter-meet">3</a>)
についても，$\mathcal{U}$ の有限交叉性より明らか．よって $\mathcal{U}$
はフィルターである．
</div>

フィルターは「大きな部分集合の集まり」だと思っておけば大体間違いはない．大きい集合を含む集合は大きいだろうし，空集合は大きくない．大きい物の共通部分を取っても大きい，と云うのはちょっと直観的ではないかもしれないが，そういうものかなとも思える．ウルトラフィルターは，特にめいっぱいまでキメの細かいフィルターだと思えばよい．

以下では，単なるフィルターをウルトラフィルターに拡張して用いる．そういった事が出来る，というのが次のウルトラフィルターの補題である．

<div class="lemma" name="ウルトラフィルターの補題">
-   任意の $I$ 上のフィルター $\mathcal{F} \subseteq \mathfrak{P}(I)$
    に対し，それを含むような $I$ 上のウルトラフィルター $\mathcal{U}$
    が存在する．

-   任意の有限交叉性を持つ $I$ の部分集合族
    $\mathcal{E} \subseteq \mathfrak{P}(I)$ に対し，それを含む $I$
    上のウルトラフィルター $\mathcal{U}$ が存在する．
</div>

証明には Zorn
の補題を用いる．命題論理のコンパクト性定理を示した際の，極大無矛盾集合の存在定理と殆んど同じように証明出来るので，証明は省略する．

超積の定義と基本性質
====================

いよいよ超積の概念を定義する．「はじめに」述べたように，超積は「殆んど至るところ」で成立する性質を取り出したものであり，フィルターは「大きな集合」の集まりだったから，これらを念頭に置けば次の定義は自然なものだと思えるだろう．以下，言語 $L$ を一つ固定する．

<div class="definition" name="超積">
$\{\mathfrak{A}_i\}_{i\in I}$：
$L$-構造の族（$I \neq 0$），$\mathcal{F}$：$I$
上のフィルターとする．この時，$\prod_{i \in I} |\mathfrak{A}_i|$
上の二項関係 $\sim_\mathcal{F}$ を次で定義する．
$$u \sim_\mathcal{F} v \overset{\mathrm{def}}{\Leftrightarrow}\left\{i \in I\ \ \middle| u(i) = v(i)\right\} \in \mathcal{F}\ \ \ \left(u, v \in \prod_{i \in I} |\mathfrak{A}_i|\right)$$
このとき，$\sim_\mathcal{F}$ は同値関係となっている．$u, v$
が「大きい集合で一致すれば」，つまり殆んど至るところで一致すれば，$u, v$
を等しいと見做すのである．

$\{\mathfrak{A}_i\}_{i \in I}$ の $\mathcal{F}$ による**縮積**（*reduce
product*）$\mathfrak{C} = \left.{\prod_{i \in I} \mathfrak{A}_i}\middle/{\mathcal{F}}\right.$
とは，$\prod_{i\in I} |\mathfrak{A}_i|$ の $\sim_\mathcal{F}$
による商集合のことである．即ち，$u$ の $\sim_{\mathcal{F}}$
による同値類を $[u]_{\mathcal{F}}$ と書けば，
$$\left.{\prod_{i\in I} \mathfrak{A}_i}\middle/{\mathcal{F}}\right. = \left\{ [u]_\mathcal{F}\ \ \middle|\ \ u \in \prod_{i\in I} \left|\mathfrak{A}_i\right|\right\}$$
のことである．このとき，$\mathfrak{C}$ の
$L$-構造としての述語記号，函数記号に関する解釈は次により定める．

$$\begin{gathered}
  P^\mathfrak{C} = \left\{([u_1], \dots, [u_n])\ \ \middle|\ \ \left\{i \in I\ \ \middle|\ \ (u_1(i), \dots, u_n(i)) \in P^{\mathfrak{A}_i}\right\} \in \mathcal{F}\right\}\\
 f^\mathfrak{C}([u_1], \dots, [u_n]) = [u]\\
  ただし u(i) = f^{\mathfrak{A}_i}(u_1(i), \dots, u_n(i))
 \end{gathered}$$

特に，ウルトラフィルター $\mathcal{U}$
に関する縮積を**超積**（*ultraproduct*）と云う．
</div>

上の $\sim_\mathcal{F}$ が実際に同値関係となることは，フィルターの条件
(<a href="#filter-meet">3</a>) などからすぐに判る．また，上の縮積の定義が
well-defined であることは本来ならば示すべきだが，面倒なのでやめる．

超積が「殆んど至るところで成立する性質を取り出したもの」ということを定式化したのが次の定理である[^1]．

<div class="theorem" name="Łośの定理">
$\mathcal{U}: I\text{上のウルトラフィルター},\quad \mathfrak{C} = \left.{\prod_{i \in I}\mathfrak{A}_i}\middle/{\mathcal{U}}\right.$

$A: a_1, \dots, a_n \text{以外に自由変数を持たない}L\text{-論理式},\quad [u_1], \dots, [u_n] \in |\mathfrak{C}|$
とすると，次が成立．

$$\mathfrak{C} \models A[\begin{smallmatrix}
              a_1 & \dots & a_n\\
              {[u_1]} & \dots & [u_n]
             \end{smallmatrix}]
 \Leftrightarrow \left\{ i \in I\ \ \middle|\ \ \mathfrak{A}_i \models A[\begin{smallmatrix}
                               a_1    & \dots & a_n\\
                               u_1(i) & \dots & u_n(i)
                              \end{smallmatrix}]\right\} \in \mathcal{U}$$
</div>

定理の証明の為に，次の補題をまず示そう．

<div class="lemma">
$t: a_1, \dots, a_n \text{以外に自由変数を持たない} L\text{-論理式}, \tau(i) = (t_{}[\begin{smallmatrix} a_1 & \dots & a_n \\ u_1(i) & \dots & u_n(i) \end{smallmatrix}])^{\mathfrak{A}_{i}}$
とする．このとき，
$$(t[\begin{smallmatrix} a_1 & \dots & a_n \\ {[u_1]} & \dots & [u_n] \end{smallmatrix}])^{\mathfrak{C}} = [\tau]$$
</div>

<div class="proof">
$L(\mathfrak{C})$ の項の構成に関する帰納法で示す．$t_1, \dots, t_n$ を
$a_1, \dots, a_n$ 以外に自由変数を持たない $L(\mathfrak{C})$
の項とし，$\tau_j(i) = (t_{j}[\begin{smallmatrix} a_1 & \dots & a_n \\ u_1(i) & \dots & u_n(i) \end{smallmatrix}])^{\mathfrak{A}_{i}}$
とする．帰納法の仮定は
$(t_j[\begin{smallmatrix} a_1 & \dots & a_n \\ {[u_1]} & \dots & [u_n] \end{smallmatrix}])^{\mathfrak{C}} = [\tau_j]$
である．$f$ を $n$-変数関数記号とすると，

$$\begin{aligned}
  (f(t_1, \dots, t_n)[\begin{smallmatrix} a_1 & \dots & a_n \\ {[u_1]} & \dots & [u_n] \end{smallmatrix}])^{\mathfrak{C}}
  &\equiv (f(t_1[\begin{smallmatrix} a_1 & \dots & a_n \\ {[u_1]} & \dots & [u_n] \end{smallmatrix}], \dots, t_n[\begin{smallmatrix} a_1 & \dots & a_n \\ {[u_1]} & \dots & [u_n] \end{smallmatrix}]))^{\mathfrak{C}} & (\text{置換の定義})\\
  &\equiv f^{\mathfrak{C}}(t_1[\begin{smallmatrix} a_1 & \dots & a_n \\ {[u_1]} & \dots & [u_n] \end{smallmatrix}]^{\mathfrak{C}}, \dots, t_n[\begin{smallmatrix} a_1 & \dots & a_n \\ {[u_1]} & \dots & [u_n] \end{smallmatrix}]^{\mathfrak{C}}) & (\text{解釈の定義})\\
  &\equiv f^{\mathfrak{C}}([\tau_1],\dots,[\tau_n]) & (\text 帰納法の仮定)\\
  &\equiv [i \mapsto f^{\mathfrak{A}_i}(\tau_1(i), \dots, \tau_n(i))] & (\text{超積の定義})\\
  &\equiv [i \mapsto f^{\mathfrak{A}_i}((t_{1}[\begin{smallmatrix} a_1 & \dots & a_n \\ u_1(i) & \dots & u_n(i) \end{smallmatrix}])^{\mathfrak{A}_{i}}, \dots, (t_{1}[\begin{smallmatrix} a_1 & \dots & a_n \\ u_1(i) & \dots & u_n(i) \end{smallmatrix}])^{\mathfrak{A}_{i}})] & (\tau_j \text{の定義})\\
  &\equiv [i \mapsto (f(t_1, \dots, t_n)[\begin{smallmatrix} a_1 & \dots & a_n \\ u_1(i) & \dots & u_n(i) \end{smallmatrix}])^{\mathfrak{A}_i}]
 \end{aligned}$$

よって示された．
</div>

<div class="proof" name="Łośの定理の証明">
$L(\mathfrak{C})$の論理式の構造帰納法で示す．以下，$|\mathfrak{C}|$ の元と
$L(\mathfrak{C})$ での名前を同一視する．

(i) $A \equiv P t_1 \dots t_n$ のとき．$P$ を $n$-変数述語記号，$t_1, \dots, t_n$ を $L(\mathfrak{C})$
    の項とすると，

    $$\begin{aligned}
         & \mathfrak{C} \models (P t_1 \dots t_n)[\begin{smallmatrix} a_1 & \dots & a_n \\ {[u_1]} & \dots & [u_n] \end{smallmatrix}]\\
         & \Leftrightarrow \mathfrak{C} \models P(t_1[\begin{smallmatrix} a_1 & \dots & a_n \\ {[u_1]} & \dots & [u_n] \end{smallmatrix}])\dots(t_n[\begin{smallmatrix} a_1 & \dots & a_n \\ {[u_1]} & \dots & [u_n] \end{smallmatrix}])\\
         & \Leftrightarrow (t_1[\begin{smallmatrix} a_1 & \dots & a_n \\ {[u_1]} & \dots & [u_n] \end{smallmatrix}]^{\mathfrak{C}}, \dots, t_n[\begin{smallmatrix} a_1 & \dots & a_n \\ {[u_1]} & \dots & [u_n] \end{smallmatrix}]^{\mathfrak{C}}) \in P^\mathfrak{C} & \quad (\models \text{の定義})\\
         & \Leftrightarrow ([\tau_1], \dots, [\tau_n]) \in P^\mathfrak{C} & (\text{補題}；但し補題の記号を用いた)\\
         & \Leftrightarrow \left\{ i \in I\ \ \middle|\ \ (\tau_1(i), \dots, \tau_n(i)) \in P^{\mathfrak{A}_i}\right\} \in \mathcal{U}& (超積の定義)\\
         & \Leftrightarrow \left\{ i \in I\ \ \middle|\ \ ((t_{1}[\begin{smallmatrix} a_1 & \dots & a_n \\ u_1(i) & \dots & u_n(i) \end{smallmatrix}])^{\mathfrak{A}_{i}}, \dots, (t_{n}[\begin{smallmatrix} a_1 & \dots & a_n \\ u_1(i) & \dots & u_n(i) \end{smallmatrix}])^{\mathfrak{A}_{i}}) \in P^{\mathfrak{A}_i}\right\} \in \mathcal{U}\\
         & \Leftrightarrow \left\{ i \in I\ \ \middle|\ \ \mathfrak{A}_i \models P(t_{1}[\begin{smallmatrix} a_1 & \dots & a_n \\ u_1(i) & \dots & u_n(i) \end{smallmatrix}])\dots(t_{n}[\begin{smallmatrix} a_1 & \dots & a_n \\ u_1(i) & \dots & u_n(i) \end{smallmatrix}])\right\} \in \mathcal{U} & (\models \text{の定義})\\
         & \Leftrightarrow \left\{ i \in I\ \ \middle|\ \ \mathfrak{A}_i \models (P t_1 \dots t_n)[\begin{smallmatrix} a_1 & \dots & a_n \\ u_1(i) & \dots & u_n(i) \end{smallmatrix}]\right\} \in \mathcal{U} & (\models \text{の定義})
        \end{aligned}$$

(ii) $A \equiv B \wedge C$ のとき．$B, C$ は命題を満たす論理式であるとする（帰納法の仮定）．

    $$\begin{aligned}
	\newcommand{\myreplace}{[\begin{smallmatrix} a_1 & \dots & a_n \\ u_1(i) & \dots & u_n(i) \end{smallmatrix}]}
	\newcommand{\ultreplace}{[\begin{smallmatrix} a_1 & \dots & a_n \\ {[u_1]} & \dots & [u_n] \end{smallmatrix}]}
	 &\mathfrak{C} \models (B \wedge C)\ultreplace\\
	 &\Leftrightarrow \mathfrak{C} \models B\ultreplace \wedge C\ultreplace\\
	 &\Leftrightarrow \mathfrak{C} \models B\ultreplace \mathbin{{かつ}} \mathfrak{C} \models C\ultreplace &\quad(\models \text{の定義})\\
	 &\Leftrightarrow \left\{ i \in I\ \ \middle|\ \ \mathfrak{A}_i \models B\myreplace\right\} \in \mathcal{U} \text{かつ} \left\{ i \in I\ \ \middle|\ \ \mathfrak{A}_i \models C\myreplace\right\} \in \mathcal{U} &\quad (\text{帰納法の仮定})\\
	 & \Leftrightarrow \left\{ i \in I\ \ \middle|\ \ \mathfrak{A}_i \models B\myreplace\right\} \cap \left\{ i \in I\ \ \middle|\ \ \mathfrak{A}_i \models C\myreplace\right\} \in \mathcal{U} & (\text{フィルターの定義} (3))\\
	 & \Leftrightarrow \left\{ i \in I\ \ \middle|\ \ \mathfrak{A}_i \models B\myreplace \text{かつ} \mathfrak{A}_i \models C\myreplace\right\} \in \mathcal{U}\\
	 & \Leftrightarrow \left\{ i \in I\ \ \middle|\ \ \mathfrak{A}_i \models B\myreplace \wedge C\myreplace\right\} \in \mathcal{U} & (\models \text{の定義})\\
	 & \Leftrightarrow \left\{ i \in I\ \ \middle|\ \ \mathfrak{A}_i \models (B \wedge C)\myreplace\right\} \in \mathcal{U}        \end{aligned}$$

(iii) $A \equiv \neg B$ のとき．ここでウルトラフィルターであることが効いてくる．

    $$\begin{aligned}
         \mathfrak{C} \models (\neg B)[\begin{smallmatrix} a_1 & \dots & a_n \\ {[u_1]} & \dots & [u_n] \end{smallmatrix}]&\Leftrightarrow  \mathfrak{C} \models B[\begin{smallmatrix} a_1 & \dots & a_n \\ {[u_1]} & \dots & [u_n] \end{smallmatrix}]\text{でない}\\
         &\Leftrightarrow \left\{ i \in I\ \ \middle|\ \ \mathfrak{A}_i \models B[\begin{smallmatrix} a_1 & \dots & a_n \\ u_1(i) & \dots & u_n(i) \end{smallmatrix}]\right\} \notin \mathcal{U} &\quad (\text{帰納法の仮定})\\
         &\Leftrightarrow I \setminus \left\{ i \in I\ \ \middle|\ \ \mathfrak{A}_i \models B[\begin{smallmatrix} a_1 & \dots & a_n \\ u_1(i) & \dots & u_n(i) \end{smallmatrix}]\right\} \in \mathcal{U} & \quad (\mathcal{U}:\text{ウルトラフィルター})\\
         &\Leftrightarrow \left\{ i \in I\ \ \middle|\ \ \mathfrak{A}_i \not\models B[\begin{smallmatrix} a_1 & \dots & a_n \\ u_1(i) & \dots & u_n(i) \end{smallmatrix}]\right\} \in \mathcal{U}\\
         &\Leftrightarrow \left\{ i \in I\ \ \middle|\ \ \mathfrak{A}_i \models \neg B[\begin{smallmatrix} a_1 & \dots & a_n \\ u_1(i) & \dots & u_n(i) \end{smallmatrix}]\right\} \in \mathcal{U}
        \end{aligned}$$

(iv) $A \equiv B \vee C, B \rightarrow C$ のとき．$B \vee C$ の時は，$\neg (\neg B \wedge \neg C)$
    を考えるか，ウルトラフィルターの性質から
    $X \cup Y \in \mathcal{U} \Rightarrow X \in \mathcal{U} \vee Y \in \mathcal{U}$
    となることを使う．$B \to C$ のときは定義から明らか．

(v) $A \equiv \exists x B$ のとき．(⇒) の方向は明らか．(⇐) の方向について．
    $S \mathrel{:=}\left\{i \in I\ \ \middle| \mathfrak{A}_i \models (\exists x B) [\begin{smallmatrix} a_1 & \dots & a_n \\ u_1(i) & \dots & u_n(i) \end{smallmatrix}]\right\}$
    とおく．$u \in \prod_{i \in I} |\mathfrak{A}_i|$
    を次のように構成する．まず， $i \in S$ に対しては
    $\mathfrak{A}_i \models B\genfrac{[}{]}{0pt}{}{x}{u(i)}[\begin{smallmatrix} a_1 & \dots & a_n \\ u_1(i) & \dots & u_n(i) \end{smallmatrix}]$
    となるように適当な $u(i) \in |\mathfrak{A}_i|$ を取れる．$i\notin S$
    の場合は適当に何でもよいから $|\mathfrak{A}_i|$
    の元を取る．すると，$[u]$ が
    $B[\begin{smallmatrix} a_1 & \dots & a_n \\ u_1(i) & \dots & u_n(i) \end{smallmatrix}][\begin{smallmatrix}x \\ {[u]}\end{smallmatrix}]$
    を満足する．

(vi) $A \equiv \forall x B$ の場合も上と同様．

以上より示された．
</div>

Łośの定理の系として，次が得られる．

<div class="corollary">
$\varphi: L\text{-閉論理式}$ とするとき，

$$\left.{\prod_{i \in I}\mathfrak{A}_i}\middle/{\mathcal{U}}\right. \models \varphi \Leftrightarrow \left\{i \in I\ \ \middle|\ \ \mathfrak{A}_i \models \varphi\right\} \in \mathcal{U}$$

</div>

<div class="corollary">
 特に $\mathfrak{A}_i = \mathfrak{A}$ とする．このように，各
$\mathfrak{A}_i$
が等しい場合の超積を，特に**超冪**（ウルトラパワー；ultrapower）と呼ぶ．このとき，$u \in |\mathfrak{A}|$
について $c_u(i) = u \quad (i \in I)$ により定数函数
$c_u \in \prod_{i \in I} \mathfrak{A}$ を定める．このとき次が成立．
$$\left.{\prod_{i \in I} \mathfrak{A}}\middle/{\mathcal{U}}\right. \models A[\begin{smallmatrix} a_1 & \dots & a_n \\ {[c_{u_1}]} & \dots & [c_{u_n}] \end{smallmatrix}] \Leftrightarrow  \mathfrak{A} \models A[\begin{smallmatrix} a_1 & \dots & a_n \\ u_1 & \dots & u_n \end{smallmatrix}]$$
特に，$\varphi$ が $L$-閉論理式のとき次が成立．
$$\left.{\prod_{i \in I} \mathfrak{A}}\middle/{\mathcal{U}}\right. \models \varphi \Leftrightarrow  \mathfrak{A} \models \varphi$$
</div>

超積によるコンパクト性定理の証明
================================

有限交叉性を使った証明と，フィルターの性質を使った証明の二種類を紹介する．

<div class="theorem" name="コンパクト性定理">
$\mathcal{T}$ を
$L$-閉論理式からなる集合とするとき，次は同値．

1.  $\mathcal{T}$ はモデルを持つ．

2.  $\mathcal{T}$ の任意の有限部分集合がモデルを持つ．
</div>

<div class="proof">
$1 \Rightarrow 2$
は明らか．逆を示す．

$I = \mathfrak{P}_{\mathrm{fin}}(\mathcal{T})$ （$\mathcal{T}$
の有限部分集合全体）と置く．このとき，$2$ より任意の
$S \in I$ に対し，$\mathfrak{A}_S \models S$ となるような $L$-構造
$\mathfrak{A}_S$
が取れる．今，$V_S \mathrel{:=}\left\{X \in I\ \ \middle|\ \ S \subseteq X\right\}$
とおけば，$V_S \neq \emptyset$ であり，
$V_{S_1} \cap V_{S_2} = V_{S_1 \cup S_2}$ となるので，
$$\mathcal{F} \mathrel{:=}\left\{ Y \subseteq I\ \ \middle|\ \ \exists S \in I [V_S \subseteq Y] \right\}$$
とおけば，$\mathcal{F}$
はフィルターとなる．よってウルトラフィルターの補題より
$\mathcal{F} \subseteq \mathcal{U}$ となるようなウルトラフィルター
$\mathcal{U}$
が取れる．$\mathfrak{C} = \left.{\prod_{S \in I} \mathfrak{A}_S}\middle/{\mathcal{U}}\right.$
が $\mathcal{T}$ のモデルとなることを示そう．それには，Łośの補題より
$\varphi \in \mathcal{T}$ に対し，
$\left\{S \in I\ \ \middle|\ \ \mathfrak{A}_S \models \varphi\right\} \in \mathcal{U}$
を示せば十分である．今，$V_{\left\{\varphi\right\}} \in \mathcal{U}$
であり，$S \in V_{\left\{\varphi\right\}}$ とすると $S$
は有限部分集合なのでモデルを持ち，$\varphi \in S$ より
$\mathfrak{A}_S \models \varphi$．よって
$V_{\left\{\varphi\right\}} \subseteq \left\{S \in I\ \ \middle|\ \ \mathfrak{A}_S \models \varphi\right\}$．今，$\mathcal{U}$
はフィルターだったから，結局
$\left\{S \in I\ \ \middle|\ \ \mathfrak{A}_S \models \varphi\right\} \in \mathcal{U}$
となり，命題が示された．
</div>

<div class="proof" name="有限交叉性を使った別証">
$I, \mathfrak{A}_S$ を定めるところまでは上と同様である．次に，$A_\varphi \mathrel{:=}\left\{S \in I\ \ \middle| \mathfrak{A}_S \models \varphi\right\}$
と置く．$2$ より特に
$\mathfrak{A}_{\left\{\varphi, \psi\right\}} \models \varphi, \psi$ なので，
$\mathfrak{A}_{\left\{\varphi, \psi\right\}} \in A_\varphi \cap A_\psi$．よって
$\mathcal{E} \mathrel{:=}\left\{ A_\varphi\ \ \middle|\ \ \varphi \in \mathcal{T}\right\}$
とおけば，$\mathcal{E}$ は有限交叉性を持つ．ウルトラフィルターの補題より
$\mathcal{E}$ はウルトラフィルター $\mathcal{U}$
に拡張出来るので，これによる超積を考えてやると，Łośの定理から直ちに
$1$ が従う．
</div>

自然数の超準モデル
==================

ここでは，超冪を用いて自然数の超準モデルを構成する．**超準モデル**（*non-standard
model*）というのは，通常期待されるような物と異なるモデルでありながら，一階述語論理の範囲内では全く同じ性質を持つようなモデルのことである．

まず，次の $\mathbb{N}$ 上のフィルターを考える．
$$\mathcal{F}_0 \mathrel{:=}\left\{ X \subseteq \mathbb{N}\ \ |\ \ \mathbb{N}\setminus X \text{は有限集合}\right\}$$
これは Fréchet
フィルターと呼ばれるものである．これが実際にフィルターであることは簡単に確かめられる．ウルトラフィルターの補題により，この
$\mathcal{F}_0$ を含むようなウルトラフィルター $\mathcal{U}$
を取ることが出来る．このウルトラフィルターは一意なものではなく，複数のものがありうることを注意しておく．

以下では，順序環の言語 $L = (+, \cdot, \leq)$ を考えて，順序環
$\mathfrak{Z} = (\mathbb{Z}, +, \cdot, \leq)$ の $\mathcal{U}$
による超冪 $\mathfrak{C}$ を考える．
$$\mathfrak{C} = \left.{\prod_{n \in \mathbb{N}} \mathfrak{Z}}\middle/{\mathcal{U}}\right.$$
$\varphi$ を $a_1,\dots, a_n$ 以外に自由変数を持たない
$L$-論理式とすると，Łośの定理の系から次が成立した．
$$\left.{\prod_{n \in \mathbb{N}} \mathfrak{Z}}\middle/{\mathcal{U}}\right. \models \varphi
 \Leftrightarrow \mathfrak{N} \models \varphi$$ 各整数
$n \in \mathbb{Z}$ に対して定数函数 $c_n(i) = n$ を考えてやると，$[c_n]$
は超冪の元である．これらは $\mathbb{Z}$ に元々属している整数 $n$
に対応するものとみることが出来る．また，$\mathbb{N}$ 上の恒等写像 $id$
を考えると，$id \in \prod_{n \in \mathbb{N}} \mathfrak{Z}$ より $[id]$
も上の超冪 $\mathfrak{C}$
の元であることがわかる．ではこれはどんな元だろうか？実は無限大の自然数（超有限自然数）とでも云うべきものになっている．

このことを詳しく見てみよう．Łośの定理より，$\mathfrak{C} \models [c_n] \leq [id]$
は
$\left\{k \in \mathbb{N}\ \ \middle|\ \ \mathfrak{Z} \models c_n(k) \leq id(k)\right\} \in \mathcal{U}$
と同値である．$c_n, id$ の定義からこれは
$\left\{k \in \mathbb{N}\ \ \middle|\ \ \mathfrak{Z} \models n \leq k\right\} \in \mathcal{U}$
と同値である．$\mathcal{U}$ は Fréchet
フィルターを含み，$\left\{k \in \mathbb{N}\ \ \middle|\ \ k < n\right\}$
は有限であるので，$\left\{k \in \mathbb{N}\ \ \middle|\ \ \mathfrak{Z} \models n \leq k\right\} \in \mathcal{U}$
となる．よって，任意の「有限の」整数 $n$ に対し， $[c_n] < [id]$
成立することがわかる．また，任意の整数 $n$ について $n+1 < [id]$ なので
$n < [id] - 1$
となる．つまり，無限大の自然数は無数に存在することになる．また，
$-[id] < -n$ となるので，$\mathfrak{Z}$
は負の無限大の整数も持つことになる．

ところで，今，任意の整数 $n$ について，$n$ か $n-1$
のいずれかは偶数である．即ち，
$$\mathfrak{Z} \models \forall n \exists m (n = 2 \cdot m \vee n - 1 = 2\cdot m)$$
が成立するのであった．すると，Łośの定理の系から
$$\mathfrak{C} = \left.{\prod_{n \in \mathbb{N}} \mathbb{Z}}\middle/{\mathcal{U}}\right. \models \forall n \exists m (n = 2 \cdot m \vee n - 1 = 2\cdot m)$$
である．よって $[id]$ か $[id]-1$ のいずれかが $2$
で割れることになる[^2]．割れる方を $2$
で割ってやると，これも超有限の自然数になっている．この手続を繰り返して，$\mathfrak{C}$
の元の狭義減少列 $\alpha_0 > \alpha_1 > \dots > \alpha_n > \cdots$
が得られる．これらはいずれも $0$
より大きい．よって自然数の狭義減少列が得られたことになる．

しかし，自然数の整列性から狭義減少列は存在しない筈だ．どういうことか？今我々が考えているのは，「一階述語論理」で書ける範囲の理論であった．つまり，「狭義減少列が存在しない」とか「自然数は整列する」といった概念は，一階述語論理では書けないと云うことが，この事実の伝える事なのである．自然数の整列性は，「任意の自然数からなる部分集合に最小値が存在する」という形で述べられるが，この「部分集合」に対する量化が一階述語論理では行えないのである[^3]．このことは，「自然数の部分集合」全体は非可算無限個存在するが，自然数の理論自体は可算言語で記述されるため，高々可算個の部分集合しか扱えない，ということを考えるとちょっと分かり易いのではないだろうか．

おわりに
========

駆け足で超積とその応用を説明した．他にも色々な使いでがあって，Löwenheim-Skolem
の定理の証明や，超準解析の公理を満たすモデルを構成するのにも用いる事ができる．Löwenheim-Skolem
については江田<sup><a href="bib-Eda">[4]</a></sup> や田中・坪井・野本<sup><a href="bib-Tanaka">[3]</a></sup> が，超準解析については江田<sup><a href="bib-Eda">[4]</a></sup> や Keisler<sup><a href="bib-Keisler">[2]</a></sup>が参考になるだろう．

参考文献
=======

1. <a id="bib-Awodey"></a>Steve Awodey. Category Theory, Vol. 52 of Oxford Logic Guides. Oxford University Press, 2010. 
2. <a id="bib-Keisler"></a>H. Jerome Keisler. [Foundations of infinitesimal caculus](http://www.math.wisc.edu/~keisler/foundations.html), 2007. 
3. <a id="bib-Tanaka"></a>田中一之, 坪井明人, 野本和幸. ゲーデルと 20 世紀の論理学(ロジック)2 完全性定理とモデル理論, ゲーデルと 20 世紀の論理学, 第 2 巻. 東京大学出版会, 2011. 
4. <a id="bib-Eda"></a>江田勝哉. 数理論理学 ──使い方と考え方:超準解析の入口まで. 内田老鶴圃, 2010.
5. <a id="bib-Arai"></a>新井敏康. 数学基礎論. 岩波書店, 2011.
6. <a id="bib-Tsuboi"></a>坪井明人. モデルの理論. 河合出版, 1997.

[^1]: Łは “w” と “l”
    の中間音らしい．カタカナでは「ウォッシュの定理」という表記が一般的なようだ．.

[^2]: どちらが割れるかは，ウルトラフィルターの取り方によってかわってくる.

[^3]: そうはいっても，「集合と位相」などの講義でそういったことを証明したぞ，と思われるかもしれない．あれが上手くいくのは，集合論の中で自然数や数列といった対象を扱っているからである．.
