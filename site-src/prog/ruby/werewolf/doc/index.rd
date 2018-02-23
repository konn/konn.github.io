=begin
= 汎用人狼エンジン WereWolf
== 目次
* ((<WereWolf::Village|URL:village.html#label-1>))
* ((<WereWolf::StandardProfs|URL:profs.html#label-1>))
* ((<WereWolf::PossessionProfs|URL:profs.html#label-10>))
* ((<WereWolf::HamsterProfs|URL:profs.html#label-12>))

== 簡単な説明
=== 普通の役職構成
(1) ((|Village.new(StandardProfs)|))で初期化
(2) ((|Village#entry(user, char, prof)|))で村民をエントリー
(3) ((|Village#start|))で村開始
(4) ((|Village#update|))で更新
    *返値はHash
        ::votes
          投票情報
        ::hanged
          処刑
        ::medium
          霊能結果
        ::fortune
          占い結果のHash
          
          Keyは占い師、Valueは[占い先, 占い結果]
    *生存者は((|Village#alivers|))、日数は((|Village#progress|))で確認できます

(5) ((|Village#finished?|))が((|true|))を返せば終了
=== その他
ハムスターを入れたければ、1. の((|StandardProfs|))を((|HamsterProfs|))に、憑依を有りにしたければ((|PossessionProfs|))に変更すればOK。

=end