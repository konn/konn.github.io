=begin
= profs.rb
== WereWolf::StandardProfs
役職を管理する基本クラスです。
=== 役職の追加の仕方
(1) ((|WereWolf::StandardProfs|))を継承したサブクラスを作ります。
(2) ((|setup|))で初期化後の処理を指定します。
(3) ((|power_use|))を上書きして、能力の実行処理を指定します。((|power|))には能力を表す文字列が入ります。
(4) ((|team_win?|))を上書きして、能力の処理判定を指定します。((|team|))には能力を表す文字列が入ります。

=== クラスメソッド
--- new
    標準構成の役職を管理するクラスを生成します。
=== インスタンスメソッド
--- setup
    ((<new>))の直後に呼ばれます。メソッドはサブクラスでオーバーライドされます。
--- set_prof(from, to)
    ((|from|))の能力使用先を((|to|))にセットします。

--- use_prof(binding)
    能力を発動します。((|Binding|))オブジェクトを渡します。
    ((|@prof_prio|))で指定された順に((|power_use|))を呼び出していきます。powerの部分には能力者のラベルが入ります。

--- winner(binding)
    勝利判定をします。((|Binding|))オブジェクトを渡します。
    ((|@win_check|))で指定された順に((|team_win?|))を呼び出します。teamの部分には能力者のラベルが入ります。

== WereWolf::PossessionProfs
憑依有りの役職管理クラスです。
=== スーパークラス
((<WereWolf::StandardProfs>))

== WereWolf::HamsterProfs
ハムスター有りの役職管理クラスです。
=== スーパークラス
((<WereWolf::StandardProfs>))

=end