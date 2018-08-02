---
title: |
  zsh hooks を使って ssh / mosh の接続先によって Terminal.app のテーマを変える
date: 2015/08/04 00:00:00 JST
author: 石井大海
description: |
  **この記事はQiitaからの移行記事です**。情報は古いので、記録以上の価値はありません。
tag: zsh,OSX,macOS,Terminal,Terminal.app,mosh,ssh
---

前置き
=====

最近リモートの VPS とテモートの VM で同時に作業していて、どっちがどっちのプロンプトかわからなくなるということがよくあります。

そこで、Terminal.app の背景を接続先に応じて変化させられたら便利です。
この解決法については、たとえば次の記事にあります：

* [OSXからSSHでリモートサーバーにログインしてる時は背景色を変更する](http://qiita.com/ironsand/items/b04581bf668f886e3d20)
* [osx - How do I make the apple terminal window auto change colour scheme when I ssh to a specific server - Stack Overflow](http://stackoverflow.com/questions/157959/how-do-i-make-the-apple-terminal-window-auto-change-colour-scheme-when-i-ssh-to)

問題点
-------

とはいえ、上のやり方には幾つか問題点や不満があります。

<dl>
<dt>タブが最前決め打ち</dt>
<dd><code>tab 1 of window 1</code> 決め打ちなので、ウインドウを切り替えたタイミングによっては違うタブの色が変わってしまう。</dd>
<dt>ダミーのコマンドを用意する必要がある</dt>
<dd>最優先のパスの所に同名のコマンドを作って置くわけで、他のプロセスが <code>ssh</code> や <code>mosh</code> を呼ぶときに思わぬ問題が発生する可能性があるし、気付きづらい。</dd>
<dt>パスの設定によっては機能しない
<dd>上の問題と関連して、パスが適切に通っている場所に配置しないと、優先順位の問題などで機能しなくなる可能性がある。</dd>
</dl>

これらを解決しましょう。

### タブの決め打ちを何とかする
まず最初の問題については、AppleScript レベルで `window` には ID が振られているので、呼び出された瞬間にその `id` を取得すれば良い。ただ、 `tab` には ID が割り振られていないようなので、ウィンドウの中で左から何番目かを覚えておく他に解決法はないみたい。なので、途中で同じウインドウの前のタブを閉じたりして順番が変わっていると、結局変更を辿ることができない。

### 上書きパスの問題
ぼくは zsh を使っているので、下記を参考に zsh hooks を使うことにしました。

* [zshでhook関数を登録する - Qiita](http://qiita.com/mollifier/items/558712f1a93ee07e22e2)
* [zsh でウィンドウ名に ホスト名とコマンド名を表示する - 俺の日記](http://izawa.hatenablog.jp/entry/2012/09/18/220106)

上の情報を使って、`preexec` フックで ssh や mosh が呼び出されていたらサーバ名によってテーマを切り替えるようにすればよい。その際にシェル変数として、ウィンドウのIDとタブの位置、それから元々のテーマを覚えておいて、その変数が定義されている場合に限り `precmd` フックで覚えておいたタブを元のテーマに復帰させればよい。

出来上がったもの
---------------

以下の内容を `~/usr/share/ssh-mosh-bgch-hook.sh` とか適当な名前で保存し、 `~/.zshrc` の適当な位置に `source ~/usr/share/ssh-mosh-bgch-hook.sh` という行を追加しとけばいい：

```zsh:ssh-mosh-bgch-hook.sh
autoload -Uz add-zsh-hook;

unset __WINDOWLOC;
unset __PREVSETID;

function set_bg () {
  osascript -e "tell app \"Terminal\" to set the current settings of the tab ${__WINDOWLOC[1]} of window id ${__WINDOWLOC[2]} to settings set \"$1\""
}

function ssh-mosh-observe() {
    mycmd=(${(s: :)${1}});
    case ${mycmd[1]} in
        ssh|mosh)
            HOSTNAME=${mycmd[-1]};
            __WINDOWLOC=(`osascript -e "tell app \"Terminal\" to selected tab of the window 1" | cut -d' ' -f 2,6`);
            __PREVSETID=`osascript -e "tell app \"Terminal\" to name of current settings of tab ${__WINDOWLOC[1]} of the window id ${__WINDOWLOC[2]}"`;
            case $HOSTNAME in # host name
                sakura-vps) set_bg "Sakura";;
                ubuntu-vm)  set_bg "VM";;
                *)          set_bg "Pro";;
            esac;;
    esac
}

add-zsh-hook preexec ssh-mosh-observe

function ssh-mosh-post() {
    if [ -n "${__WINDOWLOC-}" ]; then
        set_bg ${__PREVSETID};
        unset __WINDOWLOC;
        unset __PREVSETID;
    fi
}

add-zsh-hook precmd ssh-mosh-post
```

ここで、`Sakura` とか `Pro` とか `VM` とかってのは、ターミナル環境設定の「プロファイル」一覧に上がっているテーマの名前。サーバ毎に適当に色とか背景画像とかを作れば良いだけの話。

### デモ

以下のように、非常にシームレスにテーマが入れ替わるのがわかると思う：

[![デモを御覧頂けないのが残念です](http://i.gyazo.com/13a3c43d6bee5cfcb27770f9b76ae43f.gif)](http://gyazo.com/13a3c43d6bee5cfcb27770f9b76ae43f)

また、次のように ssh が終了した時に背面にあっても、（タブさえ動いていなければ）元のテーマに復帰するようになっている：

[![Gyazo](http://i.gyazo.com/2b7eb83278afb22d029c703bc1b43e66.gif)](http://gyazo.com/2b7eb83278afb22d029c703bc1b43e66)

### 不満点

* タブの位置が決め打ち。こればかりは `tab` に `id` みたいのがないから仕方ない。
* サーバ名が ssh / mosh コマンドのラストに来ると決め打ちしている。これは真面目にパーザ書けばいいだろうけど、ダルいしまあ実用上はそんなに問題ないだろう。
