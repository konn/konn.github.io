---
title: VSCode で YaTeX のイメージ補完やユニコード数式記号を入力する為の拡張をつくった
date: 2018/11/17 03:54:00 JST
author: 石井大海
description: |
  VSCode で YaTeX のイメージ補完相当の機能を実現したり、Agda や Lean などの定理証明系で必要となるユニコード数式記号の入力を補助するための機能拡張を作った話しです。
tag: vscode,YaTeX,LaTeX,定理証明系
---

**<span style="color: red;">LaTeX機能は独立させて[CaTeX] として新たにリリースしました。Generic Input Method を入れてもイメージ補完等は使えなくなったので、CaTeX の方を入れてください。[詳細なアナウンス](../articles/2018-11-26-happy-latex-with-catex.html)。</span>**

[][CaTeX]

# はじめに

[以前の記事][previous]にも書いたように、最近は作業環境を Emacs から VSCode に移行しています。

一方、どう考えても、Emacs の方が機能拡張（パッケージ）は沢山あって、どうしても VSCode には足りない部分も多くあります。
まあ Emacs はその沢山あるパッケージを管理するシステムが壊滅に腐っていたから地獄だったわけで、それにくらべれば、VSCode はしっかり考えて設計されており、今のところ大変快適です。

という訳で、仕方がないので LaTeX 向けの入力支援システムを作ってみました。
これがその `generic-input-method` です：

[](https://marketplace.visualstudio.com/items?itemname=mr-konn.generic-input-method)

名前に LaTeX って入っていないのは、概要に書いたように LaTeX の入力支援だけではなく、ユニコード文字を多用する定理証明系の入力支援にも使えるからです。

## DEMO

こんな感じで、YaTeX のイメージ補完とギリシア文字補完っぽいのができます：

![Greek and Image](https://github.com/konn/vscode-generic-input-method/raw/master/images/image-and-greek.gif)

選択範囲を置き換えたり、包んだりするのに、別途コマンドパレットで呼び出すこともできます：

![Invoking](https://github.com/konn/vscode-generic-input-method/raw/master/images/image-invoked.gif)

ユニコード文字の入力はこんな感じ：

![Invoking](https://github.com/konn/vscode-generic-input-method/raw/master/images/unicode-input.gif)

詳しくは README とか設定ファイルを読んでほしいですが、自分で新しい変換辞書を定義したり、新しい記号を足したり、ということもできます。

また、API を露出してあるので、他の機能拡張から呼び出して使うこともできます。
一例として、この仕組みを使って実装した、定理証明系 [redtt][redtt] の入力支援パッケージが以下です：

[](https://marketplace.visualstudio.com/items?itemname=mr-konn.redtt-diagnostics)

## LaTeX 環境の設定

オマケ的に VSCode 上での LaTeX 環境の整備について言うと、私のこれと、

- [LaTeX Workshop][latex workshop]
- [LaTeX Preview]

あたりがあれば大変快適に TeX 打ちが出来るようになった。
というか、書いている途中で気付いたんだけど、LaTeX Workshop も、イメージ補完とギリシア文字補が合体したような機能があって、<kbd>@</kbd>を押すと有効化されるようだ。
とはいえ、この補完アイテムを自分で後から追加設定したりすることは出来ないようだし、多言語から呼ぶことも出来なさそうなので、こうして新しく作った甲斐はあったと思う。

他に似た用途の機能拡張だと、[latex-input]や[Unicode LaTeX]というのがあるようだけど、辞書が一つしか持てなかったり、そもそも辞書をカスタマイズ出来るようになっていなかったり、API を露出していなかったりする。

あと、多分変換を確定させる方法は <kbd>Enter</kbd> しかどれも提供していないが、こちらでは「補完シーケンス中に出て来ない任意の文字」で確定するようにしてある。
とはいえ、つかってみてちょっと不便な場合もあるので、今後この辺りももっと柔軟に設定出来るようにする予定。
乞うご期待。

[previous]: ./migrating-emacs-to-vscode.html
[latex workshop]: https://marketplace.visualstudio.com/items?itemName=James-Yu.latex-workshop
[latex preview]: https://marketplace.visualstudio.com/items?itemName=ajshort.latex-preview
[latex-input]: https://marketplace.visualstudio.com/items?itemName=yellpika.latex-input
[unicode latex]: https://marketplace.visualstudio.com/items?itemName=oijaz.unicode-latex
[redtt]: https://github.com/RedPRL/redtt
[CaTeX]: https://marketplace.visualstudio.com/items?itemname=mr-konn.catex