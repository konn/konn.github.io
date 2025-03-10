---
title: 古典的実現可能性モデルノート
author: 石井大海
description: |
  古典的実現可能性モデル（classical realisability）の手法は，Curry--Howard対応を拡張する形で$\mathrm{ZF}$のモデルを与える方法であり，Krivineによって導入された．
  強制法を特別な場合として含むが，$\mathrm{ZFC}$から開始しても$\mathrm{ZF}+\neg \mathrm{AC}$のモデルが得られるという点で強制法を真に一般化するものになっている．
tag: 集合論,選択公理,ロジック,数理論理学,数学基礎論,実現可能性モデル,古典的実現可能性モデル,継続,call/cc
date: 2022/09/16 21:00:00 JST
---

# 概要
古典的実現可能性モデル（classical realisability）の手法は，Curry--Howard対応を拡張する形で$\mathrm{ZF}$のモデルを与える方法であり，Krivineによって導入された．
強制法を特別な場合として含むが，$\mathrm{ZFC}$から開始しても$\mathrm{ZF}+\neg \mathrm{AC}$のモデルが得られるという点で強制法を真に一般化するものになっている．

Krivineの着想は，旧来の直観主義・構成的数学の実現可能性モデルに，Peirce's Law と対応することが広く知られている `call/cc` プリミティヴを加えれば古典論理上の集合論のモデルが得られる筈である，というものである．
この達成のため，Krivine はBCKW論理に call/cc を加えた体系を，Krivine機械と呼ばれるある種のスタックマシンを用いて解釈するという方法を採っている．

本稿は，元々は2017年に早稲田大学で行われた集合論セミナーでの筆者（石井）の発表の資料である．
強制法との関係をなるべく明らかにすることを目的に書き始められ，Krivineが原論文で採用したものよりも現代的な強制法の定式化に近い形で再定式化を与えたが，現時点では完全に目的を達するに至ってはいない．誰かがその道に至る道標になれば幸いである．

# 本文リンク
* [PDFはこちら](./realisability.pdf)
* [LaTeX Source](https://github.com/konn/realisability-note)

元となったレジュメは6部に及ぶかなり膨大な TeX であり、TikZ をふんだんに用いているため現時点ではHTML化が出来ていない。
従って、当面は PDF のみの提供とさせて頂く。
