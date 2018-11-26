---
title: CaTeX（軽鳥／怪鳥）で快適 LaTeX ライフ in VSCode
date: 2018/11/26 23:10:00 JST
author: 石井大海
description: |
  [YaTeX](http://yatex.org) の補完機能を [Visual Studio Code](https://code.visualstudio.com) で実現する [CaTeX](https://marketplace.visualstudio.com/items?itemName=mr-konn.catex) 機能拡張のご紹介。
tag: vscode,LaTeX,YaTeX,CaTeX
---

# CaTeX のご紹介

Visual Studio Code に乗り換えたというのは[前の記事]でも書きましたが、その際には LaTeX を編集するのに [YaTeX] の代替がないのが不満、という話をしていました。

そこで、僕がVSCodeに足りないと思っている [YaTeX] 機能を実現する機能拡張 [CaTeX] をリリースしました！

[][CaTeX]

[][CaTeX-repo]

有り難いことに、[YaTeX 公式でもご紹介](http://yatex.org/#catex)頂いています。

先日の研究集会のスライドは CaTeX で書き進めて、今は一段落しています。
とはいえ、これからD論を書くので、まだドッグフーディングは続いていくと思います。
というわけで、今回は、CaTeXのアレコレについて説明させて頂こうかなと思います。

## 名称

YaTeX ファミリーは $\TeX$ が日本語で「てふ」と呼ばれがちなことを逆手にとって、和名として「○○てふ」→「○○鳥」を名乗ることが多いです。
今回は Visual Studio Code の Code から[^1] C を採って *CaTeX* と命名し、ローマ字というより英語的に発音して「けいてふ→けいちょう→軽鳥」、あるいは約めて「けてふ→けちょう→怪鳥」と読むことにしました。

[^1]: 正直な話、Visual Studio Code のCLIが `code` って名前なのはどうなのよと思わなくはないんですが、そこはそれ。

## 設計思想と機能

[Marketplace][CaTeX]でも紹介していますが、CaTeX は主に James Yu 氏の **[LaTeX Workshop] と共に使うことを想定**しています。

[][LaTeX Workshop]

LaTeX Workshop は本当に良く出来た拡張で、SyncTeX の機能（PDF と TeX 本文の往き来）や参考文献ブラウザ、自動タイプセットなど本当に豊富な機能があります。
スニペットを使ったコマンドの補完も実は既に LaTeX Workshop にあるんですが、これは余り拡張しやすい感じにはなっていません。
また、個人的に YaTeX のキラー機能だと思っているイメージ補完やセクション補完機能もありません。
CaTeX は、この**YaTeX と LaTeX Workshop のギャップを埋める**というのが大きな動機の一つになっています。
ですので、上に挙げたような、LaTeX Workshop で既に実現されている機能については、当面のところ CaTeX で敢えてサポートする予定はありません。
LaTeX Workshop には YaTeX の <kbd>C-c C-k</kbd> （Smart Kill）や <kbd>C-c C-c</kbd> に相当するそのものズバリの機能はありませんが、LaTeX Workshop の "*Add a multicursor to the current environment name*" や "*Navigate to matching begin/end*" あるいは "*Select current environment name*" あたりを使えばなんとかなるので、まだ手を着けてません。

また、YaTeX は Emacs が前提となる UI の設計をしていますが、郷に入りては郷に、ローマにてはローマに、VSCode では Microsoft に従え、ということで、**YaTeX の機能をより VSCode らしい方法で実現**することを念頭に置いています。
なので、補完機能に関しても、VSCode の **InteliSence やコードスニペットの機能**を使って実装して、他の機能拡張とシームレスに連携出来ような物を心掛けています。

では、以下各機能を順に見ていきましょう。

### ギリシア文字補完、イメージ補完、フォント補完

最初に書いておくと、実はまだ、VSCode では他の機能拡張で提供されている構文定義から、構文上のスコープを得る方法が提供されていません。
これはつまり、手軽に「今カーソルのある場所が数式の内部なのか手軽に判定できない」という意味です。
という訳で、下記のギリシア**文字補完、イメージ補完等は数式環境外でも呼び出せてしまう**状態です。
まあ、TikZ 環境の中で `\matrix[math nodes] { ... }`{.tex} と書かれててもイメージ補完が使えなかったりすることはよくあり、そういう点では望ましいかもしれませんが、普通の本文中（テキストモード）でも `;` や `:` を入力しようとしたら補完を解除しないといけないのは、ちょっと不便かもしれません。
その内なんとかなる、といいな……。

デモは以下の通り。
まず、<kbd>;</kbd> で数式補完が、<kbd>:</kbd> でギリシア文字補完が走ります。

![イメージとギリシア文字補完](https://github.com/konn/catex/raw/master/images/image-and-greek.gif)

補完ではなく単に `:` や `;` などのトリガー自身を入力したい時は、<kbd>Esc</kbd>キーでも押して補完をキャンセルすれば大丈夫です。
本家に合わせて二回押しとかにしても良いかもしれないですが、取り敢えず現状はこうなってます。

あと本家 YaTeX にはデフォルトでは付いていませんが、<kbd>@</kbd>キーで数式フォント補完が走ります。
ただ、<kbd>@</kbd> は LaTeX Workshop が提供している補完のプレフィックスと被っていて、使いづらいです。

また、キーボードだけではなくて、直接コマンドを呼び出すことでも補完を実行出来ます：

![イメージ、直接呼出](https://github.com/konn/catex/raw/master/images/image-invoked.gif)

補完のトリガーキーや辞書は `catex.image-completion` を編集することでカスタマイズ出来ます。
例えば、デフォルトでは以下のような設定になっています：

```javascript
{
  "name": "CaTeX Image Completion",
  "languages": [
    "latex"
  ],
  "triggers": [
    ";"
  ],
  "dictionary": "defaults/images.json",
  "renderMode": "latex"
}
```

`renderMode` の部分は必須で、`triggers` を空配列にすればホットキーは無効になり、直接呼出以外は出来なくなります。
`languages` の欄については、なんか生 TeX とか expl3 とかでも使いたくなったら入れてください。
`dictionary` の部分がファイルになっていますが、これは **CaTeX に標準で同梱されているデフォルトの辞書** への参照になっていて、例えば、以下のように書き換えることができます：

```javascript
{ ...
  "dictionary": [
      "defaults/images.json",

      // デフォルトでは \int のような \maketitle 型の補完になる。
      { "label": "{/", "body": "\\notin", "description": "∉" },

      {
        "label": "wcheck",
        "body": "widecheck", "description": "✓ (wide accent)",
        "type": "section" // "environment" などもある
      }
  ]
  ...
}
```

この補完機能は拙作の[Generic Input Method]機能拡張を使って実現されています。
なので、他の補完機能を作りたくなったら、同じ要領で Input Method の定義を書いて、`generic-input-methods.input-methods` にでも追加しておけば大丈夫な筈です。

### セクション、環境、`\maketitle` そして `{\Large }` 補完

詳しくは[Marketplace][CaTeX]や[GitHub][CaTeX-repo]に上がっているGIFを見て欲しいんですが、<kbd>C-c *</kbd> 系の補完も用意してあります。
特に分ける意味もないので、例えばセクション補完では <kbd>C-c s</kbd> を押すと、選択範囲があれば囲む形で、そうでなければ新しくスニペットを挿入する形になっています：

![Section Completion](https://github.com/konn/catex/raw/master/images/section-1.gif)

また、YaTeXの補完機能 （や全く関係ないけど AquaSKK）を偉大たらしめている**その場での辞書登録機能**も CaTeX は提供しています。

![Section Completion in-place registration](https://github.com/konn/catex/raw/master/images/section-2.gif)

折角 VSCode のスニペット機能を使っているので、**引数の仕様まで含めて登録**することが出来るようにしました。
例えば二番目に `[]{}[]` とか入力すると、オプション引数、固定引数、オプション引数の順だという定義になります。
オプション引数はまず括弧全体が選択されるので、不要なら削除することができます。
上図の通り、デフォルトのプレースホルダ文字列も指定出来て、`{foo}{}[bar]`とかすると、`\mycmd{foo}{}[bar]`という形で挿入されるので、デフォルトの内容が決まってる時や、引数の説明などを入れると良いでしょう。

選択範囲を囲む機能については、`\begin/\end`-補完では選択範囲を環境の本体として扱いますが、それ以外の補完については、

1. プレースホルダが `!` で始まっていればそこに挿入
2. そうでなければ一番最初の固定引数の所に挿入

という仕様です。上のデモで `!here` と書かれている場所がそうです。
上記 1 の場合の引数については、選択範囲が空の場合はそのまま `here` 部分がプレースホルダとして使用され、タブで往き来出来るようになります。
選択範囲が空でない場合は引数の内容として用いられ、その部分はスニペット展開では地の文として扱われタブでは無視されます。

以上のような機能は、`\begin{}...\end{}`-補完、`{\Large }`-補完、`\maketitle`-補完でも使うことが出来ます。
ただ、VSCode は2ストロークまでしかショートカットを許可していないので、YaTeX でいう <kbd>C-c b a</kbd>で `align` 環境を補完したり、<kbd>C-c b [SPACE]</kbd>で環境名一覧を出したり、といった機能は実装できていません。
まあとはいえ、曖昧検索をしてくれるので、今のところそんなに不便を感じていません。

自動登録をしてもいいですが、辞書を手で編集することも出来ます。
例えば以下のような感じ：

```javascript
  "catex.environment.dictionary": [
    {
      "include": "defaults/environments.json"
    },
    {
      "name": "fact",
      "args": [
        {
          "kind": "optional",
          "placeholder": "title"
        }
      ]
    },
    {
      "name": "remark",
      "args": [
        {
          "kind": "optional",
          "placeholder": "title"
        }
      ]
    }
  ],
```

まあこれも、[GitHubで `defaults/*.json`](https://github.com/konn/catex/tree/master/defaults)を見ればだいたい書き方がわかると思います。

## 既知の問題

### VSCode における日本語入力

これは CaTeX に限らない既知の問題ですが、VSCode では日本語入力が乱れる、特に AquaSKK との食い合わせが悪い、という事が知られています。
完全に解決は出来ませんが、余分な拡張をオフにすると若干改善します。特に、私の環境では単語ベースの InteliSense 補完と Spell Right 機能拡張（自動スペルチェッカ）をオフにした所、完全にではないものの、許容出来る範囲まで文字列の乱れを軽減出来ました。

また、Emacs の要領で <kbd>C-g</kbd> を連発していたりすると制御文字が溜まるので、[Remove backpace control character][backspace]機能拡張を使うと良いでしょう。
とはいえ、たぶんこれエディタのテキストを `EditorTextEdit` ではなく `text` を置き換える形でやっていてあまり行儀がよくないのがちょっと不満です。

[前の記事]: ../prog/migrating-emacs-to-vscode.html
[CaTeX]: https://marketplace.visualstudio.com/items?itemName=mr-konn.catex
[vscode]: https://code.visualstudio.com
[YaTeX]: http://yatex.org
[CaTeX-repo]: https://github.com/konn/catex
[LaTeX Workshop]: https://marketplace.visualstudio.com/items?itemName=James-Yu.latex-workshop
[backspace]: https://marketplace.visualstudio.com/items?itemName=satokaz.vscode-bs-ctrlchar-remover