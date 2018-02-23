---
title: 「依存関係のある値のうちの一部をrewriteしようとしたときの問題」を Idris で
published: false
author: 石井大海
description: よしひろさんの「依存関係のある値のうちの一部をrewriteしようとしたときの問題」を Idris で解いてみた記録
tag: idris, theorem prover, 定理証明系
---

```idris
module rewriting

parameters (f : (n : Nat) -> GT n 0 -> Nat,
            h : (n : Nat) -> (H : GT (2 * n) 0) -> (f (2 * n) H = n),
            x : Nat, H : GT x 0)
  goal : (x = S (S Z) * (S Z)) -> (f x H = S Z)
  goal e with (x)
    goal refl | (S (S Z)) = ?refl_rhs_1

  goal' : (x = S (S Z) * S Z) -> (f x H = S Z)
  goal' e with (x)
    goal' refl | (S (S Z)) = h 1 H
  

---------- Proofs ----------

rewriting.refl_rhs_1 = proof
  compute
  intros
  refine h
  exact 1
  compute
  trivial
```
