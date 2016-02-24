---
title: On Regularity Properties of Sets of Reals and Inaccessible Cardinals (実数の集合の正則性と到達不能基数)
date: 2016/02/01 18:02:38 JST
author: 石井大海
description: 修士論文。Solovayによる「ZF+弱い選択公理+"任意の実数の集合が可測"」という体系の無矛盾性証明と、およびKhomskiiによる一般化、そしてSolovayの逆向きの結果であるShelahの結果についてのサーヴェイ論文。また、上述の体系における代替的な解析学の結果も簡単に紹介。
tag: ZFC,set theory,公理的集合論,測度論,Solovay,Shelah,Khomskii
---

# 概要 #
[[English abstract](#english-abstract)]

選択公理の下ではLebesgue非可測であったり、Baireの性質を持たない実数の集合が存在する事が知られています。
一方で、測度論や解析学の基礎理論の展開には、**従属選択公理 (DC)**や**可算選択公理 (CC)**と呼ばれる弱い形の選択公理があれば十分な事も知られています。

本論文では、SolovayとShelahによる次の結果を取り扱います：

<div class="theorem" id="1">
次の三つの体系は、無矛盾性の意味において同値：

1. $\ZF$+$\DC$+「任意の実数の集合が可測」
2. $\ZF$+$\mathrm{CC}$+「任意の実数の集合が可測」
3. $\ZFC$ +「到達不能基数の存在 ($\mathrm{IC}$)」
</div>

ここで、**到達不能基数**とは、その存在からZFCの無矛盾性が証明されるため、$\ZFC$単体からは存在を証明出来ない**巨大基数**の一種で、その中でも最も大人しいものです。

(3)「到達不能基数の存在」から (1)「ZF+DC+任意の実数の集合が可測」 を導いたのがSolovayで、当初到達不能基数の仮定は落とせると予想していましたが、後にShelahが (1)または(2)から(3)を示し、必要十分であることが明らかにされました[^1]。

更に、Solovayの結果については、近年KhomskiiによりLebesgue可測性やBaireの性質を持つより一般の**$I$-正則性**に拡張されています。
本稿では、この内容拡張された形のSolovayの定理を扱っています。

また、Solovayの体系は通常のフルパワーの選択公理を仮定した解析学の体系とは異なる理想的な性質が多く成り立つため、既存の解析学の基礎に対する代替的な体系たり得ます。
本稿では、その体系についても非常に簡単な分析を行います。

[^1]: また、Shelahの論文では「任意の実数の集合がBaireの性質を持つ」には到達不能基数が不要な事を示しましたが、本稿では扱いません。

## Abstract {#english-abstract}

It is widely known that, under the full Axiom of Choice, there exists sets of reals without Baire property and which is not Lebesgue measurable.
On th other hand, it is also known that it is enough, to develop the basic theory of analysis and measure theory, to assume the Axiom of Dependent Choice (DC) or the Axiom of Countable Choice (CC) which are weakened form of AC.

In this thesis, we will treat the following theorems due to Solovay and Shelah:

<div class="theorem" id="2">
The following three systems are equiconsitent (i.e. have the same consistency strength):

1. $\ZF$ + $\DC$ + "Every set of reals is Lebesgue measurable"
2. $\ZF$ + $\mathrm{CC}$ + "Every set of reals is Lebesgue measurable"
3. $\ZFC$ +"The exisntence of an inaccessible cardinal ($\mathrm{IC}$)"
</div>

First, Solovay showed the direction (3) to (1) and he conjectured the use of inaccessibles can be dropped.
But, later, Shelah showed the direction (1) or (2) to (3), and they turned out to be equiconsitent [^2].

Furthermore, Khomskii recently generalized the result of Solovay to general notion of $I$-genericity, which subsumes the Baire Property and Lebesgue measurability.
Actually, we will treat this generalized form of Solovay Theorem.

In addition, the system suggested by Solovay can be regarded as an alternative foundation to develop analysis.
We also give really brief survey on this topic.

[^2]: Shelah also showed that "Every set of reals has Baire Property" doesn't require any inaccessibles, but we don't step in this direction in this thesis.

# Contents

* [English Version (External Link)](https://tsukuba.repo.nii.ac.jp/?action=pages_view_main&active_action=repository_view_main_item_detail&item_id=37187&item_no=1&page_id=13&block_id=83)
* Japanese Version (hopefully accessible in future?)

# Reference Information

```bibtex
@mastersthesis{ISHII:2016sf,
	Author = {Hiromi ISHII},
	Institution = {Tsukuba University},
	Title = {On Regularity Properties of Set of Reals and Inaccessible Cardinals},
	School = {Graduate School of Pure and Applied Sciencies},
	Month = 2,
	Year = {2016}}
```

## History
* 2016/02/16 Grammatical Change in Title
* 2016/02/01 Silently Published
