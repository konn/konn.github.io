---
title: F4, F5 アルゴリズムに関するサーベイ（A survey on F_4 and F_5 algorithms）
author: 石井大海
description: Gröbner基底の効率的な計算法である F4, F5 アルゴリズムに関するサーベイ、メモ（A note on F4 and F5 algorithms to efficiently compute Gröbner bases）。
tag: 数学, 計算機代数, GSoC 2014, Gröbner基底
---

F_5 algorithm
=============

Plan
----
1. Because [the original paper][Orig] includes many errors and lacks strictness,
   read [Steger's thesis][Rev] instead first.
2. Then read [F5C][F5C] paper to improve it.
3. Read [the paper on termination][Term] and apply its argument to F5C.

Current progress
----------------
* Reading [Steger's thesis][Rev]. (2014/04/29)

Papers
------
[A new efﬁcient algorithm for computing Gröbner bases without reduction to zero (F5)](http://www.risc.jku.at/Groebner-Bases-Bibliography/gbbib_files/publication_502.pdf)
:    Original paper. This includes some errors and have an issue on the proof of termination.
[Faugère's F5 Algorithm Revisited][Rev]
:    More accurate and comprehensive description on F_5 algorithm. This paper notes on termination, but doesn't guarantee termination for general inputs.
[F5C: a variant of Faugère’s F5 algorithm with reduced Gröbner bases][F5C]
:    Improved version of F5 algorithm, which computes reduced bases in internal computation. This algorithm is not proven to be terminate for general input cases.
[Faugère's F5 algorithm: variants and termination issues](http://www.mathematik.uni-kl.de/~ederc/download/cased_talk.pdf)
:    A short slide on F5 algorithm. This discusses on improvements and how to make F5 algorithm to terminate for general cases.
[Modifying Faugère's F5 Algorithm to Ensure Termination][Term]
:    This paper discusses several ways to make F5 algorithm terminate for not only regular sequences but also general cases.

[Rev]: https://eprint.iacr.org/2006/404.pdf

[F5C]: http://arxiv.org/pdf/0906.2967.pdf

[Term]: http://arxiv.org/pdf/1006.0318v4.pdf

[Orig]: http://www.risc.jku.at/Groebner-Bases-Bibliography/gbbib_files/publication_502.pdf
