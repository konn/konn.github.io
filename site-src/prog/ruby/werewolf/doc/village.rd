=begin
= village.rb
== WereWolf::Village
人狼村のクラスです。
=== クラスメソッド
--- new(profs = StandardProfs)
    役職を管理するクラスを渡します。デフォルトは((<WereWolf::StandardProfs|URL:profs.html#label-0>))です。
=== インスタンスメソッド
--- start
    村を開始します。

--- update
    村を更新します。

--- set_vote(from, to)
    ((|from|))の投票先を((|to|))に設定します。成功すればtrueを返します。

--- do_vote(from, to)
    投票→処刑処理を実行します。処刑された人物の((<Person|URL:person.html>))オブジェクトを返します。

--- set_prof(from, to)
    ((|from|))の能力使用先を((|to|))に設定します。成功すればtrueを返します。

--- entry(user, char, prof)
    指定したユーザー、キャラクター、役職の((<Person|URL:person.html>))オブジェクトを生成し、村民登録します。成功すればtrueを返します。

--- person_by_user(user)
    ユーザー名から((<Person|URL:person.html>))オブジェクトを返します。

--- person_by_char(char)
    キャラクターから((<Person|URL:person.html>))オブジェクトを返します。

--- alivers
    生存者の配列を返します。

--- started?
    村が開始していればtrueを返します。

--- started?
    村が終了していればtrueを返します。

--- progress
    村の状態を返します。
=end