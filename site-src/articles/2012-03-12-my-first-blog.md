------
title: 初めてのブログ
author: 石井大海
date: 2012/03/12 15:00:59 JST
description: ブログを作った報告。技術的な詳細解説。
tags: Yesod,Haskell
------
## What is this?
なんかはてなブックマークが炎上しているのと、はてなブログが思ったより使い勝手がよくないのとで、自分で自由に弄って遊べるようなブログを設置したいなあ、と思ったので、勉強も兼ねて自分で実装してみたのがこれである。

下のフッタを見て貰えれば判るとおり、まず第一にこれは [Yesod](http://yesodweb.com) アプリで、つまり [Haskell](http://www.haskell.org) で書かれている。サブドメインで動かすために、kazu-yamamoto さんの[Mighttpd2](http://mew.org/~kazu/proj/mighttpd/en/) のリバースプロクシ機能を使っています。デザインの才能がないので、その辺りは一括して[Twitter Bootstrap](http://twitter.github.com/bootstrap/)と[jQuery](http://jquery.com/)のお世話になっている。JavaScript むずい[^1]。 今のところ記事は私しか書けないようにしているけど、はてなみたいにタブを切り替えて編集しながら内容の確認も出来るようになっている。

記事の整形は[Pandoc](http://hackage.haskell.org/package/pandoc)を使った。基本的に markdown 決め打ち。シンタックス・カラーリングも [hightlighting-kate](http://hackage.haskell.org/package/highlighting-kate) と連携して綺麗にやってくれる。うれしい。こんな感じに。

```haskell
module Handler.Root where
import qualified Data.Text as T
import Control.Monad
import Import

getRootR :: Handler RepHtml
getRootR = do
  articles <- runDB $ do
    as <- map entityVal <$> selectList [] [Asc ArticleCreatedDate, Asc ArticleCreatedTime, LimitTo 5 :: SelectOpt Article]
    zip as <$> mapM (get404 . articleAuthor) as
  let render = writeHtml defaultWriterOptions . readMarkdown defaultParserState
  defaultLayout $ do
    h2id <- lift newIdent
    setTitle "Yablog homepage"
    $(widgetFile "homepage")
```

また、[MathJax](http://www.mathjax.org/)を使った数式のレンダリングにも対応している。$\sqrt{2}=\frac{1}{\frac{1}{1 + \frac{1}{\vdots}}}$ みたいにインラインで数式を書くこともできるし、

$$
\int_{-\infty}^{\infty} e^{x^2} dx = \sqrt{\pi}
$$

みたいに別行立ての数式も書ける。これで可換図式が書ければ云うことないんだけどなー。

ブログのデータなどは、VPSで立ちっぱなしにしてある [MongoDB](http://www.mongodb.org/) で今風に管理している。おっしゃれー。

あと、後々コマンドラインクライアントなどを書くことも考えて、RESTful なAPIも用意されている。問題としては、いちど Google か BrowserID で認証する必要があるのでそこが面倒なことか。OAuth Provider 書ければそっち経由でやるのが楽ですがはて。

## ソースコード
ソースコードは [Gitolist で管理](http://gitweb.konn-san.com/repo/Yablog/tree/master)しています。Gitolist じたい、ぼくが Yesod で書いた Gitolite のウェブフロントエンドです。こいつはあと commit 内容/diffの確認とかを付ければ一段落する予定。

###TODO
* コメント・トラックバック機能。
* リファラ統計などアクセス解析機能。
* RSS 配信機能
* Twitter との連携？
* UI の改善
* タグのUIの改善

[^1]: iframe の中身を弄るのにかなり時間を割いた。あとtextareaの中身を取得するのに .text を使っていておっかしいなー、と思ったら .val() を使わなくてはいけないらしかった。知るか！

