------
title: Haskell で LK の定理証明系を書いた
author: 石井大海
date: 2012/05/30 17:54:18 JST
description: Haskellの型機能をふんだんに使って、古典命題論理の体系であるLKの定理証明系を書いた話ぞ。
tags: Haskell,論理学,定理証明
------

<p>数学基礎論の講義で <a href="http://ja.wikipedia.org/wiki/%E3%82%B7%E3%83%BC%E3%82%AF%E3%82%A8%E3%83%B3%E3%83%88%E8%A8%88%E7%AE%97">Gentzen の LK</a> を習ったので、ここ数日間ずっと定理証明支援系を書いてました。 単に書いただけなら普通な気がするんですが、興が乗って Template Haskell、準クォート、GADTs、型族、DataKinds などをフル活用した無駄にリッチなものが出来てしまったので紹介します。DataKinds や Supeclass equality を使っている関係上 GHC 7.4.* でないと動かないです。</p>
<p><a href="http://gitweb.konn-san.com/repo/logic/tree/master">ソースコード一式</a>はこちら。</p>
<h2 id="簡単な紹介">簡単な紹介</h2>
<p>LKの推論規則を定義しているのが <a href="http://gitweb.konn-san.com/repo/logic/blob/master/LKRules.hs">LKRules.hs</a> です。以下そこからの一部抜粋。</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span class="ot">{-# LANGUAGE QuasiQuotes, UnicodeSyntax #-}</span>
<span class="kw">module</span> <span class="dt">LKRules</span> <span class="kw">where</span>
<span class="kw">import</span> <span class="dt">SequentTypes</span>
<span class="kw">import</span> <span class="dt">SequentMacros</span>

<span class="ot">isInitial ::</span> <span class="dt">Sequent</span> <span class="ot">-&gt;</span> <span class="dt">Bool</span>
isInitial [lkseq<span class="fu">|</span> a <span class="fu">|-</span> b <span class="fu">|</span>] <span class="fu">=</span> a <span class="fu">==</span> b

[rule<span class="fu">|</span>
<span class="fu">#</span> <span class="dt">Cut</span>
  Γ <span class="fu">|-</span> Δ, <span class="dt">A</span>   <span class="dt">A</span>, Σ <span class="fu">|-</span> Π
<span class="fu">---------------------------</span> cut
       Γ, Σ <span class="fu">|-</span> Δ, Π

<span class="fu">#</span> <span class="dt">Contraction</span>
 Γ <span class="fu">|-</span> Δ, <span class="dt">A</span>, <span class="dt">A</span>
<span class="fu">---------------</span> contractR
 Γ <span class="fu">|-</span> Δ, <span class="dt">A</span>

<span class="fu">#</span> <span class="dt">Rules</span> for <span class="dt">AND</span>
 Γ ├ Δ, <span class="dt">A</span>   Σ ├ Π, <span class="dt">B</span>
<span class="fu">--------------------------</span> andRight
  Γ, Σ ├ Δ, Π, <span class="dt">A</span> ∧ <span class="dt">B</span>

<span class="fu">#</span> <span class="dt">Rules</span> for <span class="dt">OR</span>
 <span class="dt">A</span>, Γ ├ Δ   <span class="dt">B</span>, Σ <span class="fu">|-</span> Π
<span class="fu">--------------------------</span> orLeft
  <span class="dt">A</span> ∨ <span class="dt">B</span>, Γ, Σ <span class="fu">|-</span> Δ, Π

 Γ ├ Δ, <span class="dt">A</span>
<span class="fu">----------------</span> orRightR
 Γ ├ Δ, <span class="dt">A</span> ∨ <span class="dt">B</span>

<span class="fu">#</span> <span class="dt">Rules</span> for <span class="dt">Implies</span>
  Γ ├ Δ, <span class="dt">A</span>   <span class="dt">B</span>, Σ ├ Π
<span class="fu">---------------------------</span> implLeft
  <span class="dt">A</span> <span class="ot">→</span> <span class="dt">B</span> , Γ, Σ ├ Δ, Π

<span class="fu">#</span> <span class="dt">Rules</span> for <span class="dt">Not</span>
      Γ ├ Δ, <span class="dt">A</span>
<span class="fu">------------------</span> notLeft
 ¬ <span class="dt">A</span>, Γ ├ Δ
<span class="fu">|</span>]</code></pre>
<p>はい。ご覧の通り、<em>規則の定義をそのまま書き下す</em> だけで、各規則をシーケントに適用/逆適用する函数を <em>自動生成</em> してくれます。これぞ準クォートの威力！スバラシイ。推論規則の図から実際のコードを生成しているのが <a href="http://gitweb.konn-san.com/repo/logic/blob/master/SequentMacros.hs">SequentMacros.hs</a> です。</p>
<p>では、これを読み込んで推論規則の型を見てみましょう。</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell">ghci<span class="fu">&gt;</span> <span class="fu">:</span>l LKRules.hs
<span class="dt">Ok</span>, modules loaded<span class="fu">:</span> <span class="dt">LKRules</span>, <span class="dt">SequentTypes</span>, <span class="dt">SequentMacros</span><span class="fu">.</span>

ghci<span class="fu">&gt;</span> <span class="fu">:</span>t cut
cut
<span class="ot">  ::</span> <span class="dt">Rule</span>
       ([] <span class="fu">*</span>)
       ((<span class="fu">:</span>)
          <span class="fu">*</span> <span class="dt">Formula</span> ((<span class="fu">:</span>) <span class="fu">*</span> (<span class="dt">Index</span> <span class="ch">&#39;Z &#39;</span><span class="dt">LHS</span>) ((<span class="fu">:</span>) <span class="fu">*</span> (<span class="dt">Index</span> <span class="ch">&#39;Z &#39;</span><span class="dt">RHS</span>) ([] <span class="fu">*</span>))))</code></pre>
<p>うへえ、混み合ってますね。これを読み易く整理するとこうなります。</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell">ghci<span class="fu">&gt;</span> <span class="fu">:</span>t cut
cut
<span class="ot">  ::</span> <span class="dt">Rule</span> [] [<span class="dt">Formula</span>, <span class="dt">Index</span> <span class="dt">Z</span> <span class="dt">LHS</span>, <span class="dt">Index</span> <span class="dt">Z</span> <span class="dt">RHS</span>]</code></pre>
<p>これは、「正順に適用する場合は何も引数を取らず、逆適用する場合は論理式と、最初の式の左辺から取り出すの論理式の長さ、右辺から取り出す論理式の長さを引数にとる推論規則」と読みます。 どういうこと？ Cut の推論図を引用しましょう。</p>
<pre><code>  Γ |- Δ, A   A, Σ |- Π
--------------------------- cut
       Γ, Σ |- Δ, Π</code></pre>
<p>要は三段論法<sup><a href="#fn1" class="footnoteRef" id="fnref1">1</a></sup>です。上から下を推論する場合は曖昧性の問題はないですが、下から上を導出する際には、</p>
<ul>
<li>間に挟まれている <code>A</code> に何を使うか？</li>
<li><code>Γ</code>と<code>Σ</code>、<code>Δ</code>と<code>Π</code>の境界はどこか？</li>
</ul>
<p>といったことを指定してやる必要性があります。そこで、逆適用の際には<code>A</code>に当る論理式、<code>Γ</code>と<code>Π</code>の長さを引数として取るので、そのことが型に残されています。</p>
<p>因みに、<code>Rule</code> の定義は以下の通りです。</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span class="kw">type</span> family   (<span class="fu">:~&gt;</span>) (<span class="ot">a ::</span> [<span class="fu">*</span>]) (<span class="ot">f ::</span> <span class="fu">*</span>)
<span class="kw">type</span> <span class="kw">instance</span> <span class="ch">&#39;[] :~&gt; f = f</span>
<span class="kw">type</span> <span class="kw">instance</span> (a <span class="ch">&#39;: b) :~&gt; f = a -&gt; b :~&gt; f</span>

<span class="kw">data</span> <span class="dt">Rule</span><span class="ot"> ::</span> [<span class="fu">*</span>] <span class="ot">-&gt;</span> [<span class="fu">*</span>] <span class="ot">-&gt;</span> <span class="fu">*</span> <span class="kw">where</span>
  <span class="dt">Rule</span><span class="ot"> ::</span> {<span class="ot"> ruleName ::</span> <span class="dt">String</span>
          ,<span class="ot"> apply   ::</span> <span class="kw">as</span> <span class="fu">:~&gt;</span> ([<span class="dt">Sequent</span>] <span class="ot">-&gt;</span> [<span class="dt">Sequent</span>])
          ,<span class="ot"> unapply  ::</span> bs <span class="fu">:~&gt;</span> ([<span class="dt">Sequent</span>] <span class="ot">-&gt;</span> [<span class="dt">Sequent</span>])}
       <span class="ot">-&gt;</span> <span class="dt">Rule</span> <span class="kw">as</span> bs</code></pre>
<p>型のリストから実際に適用/逆適用を行う函数の型を生成しているわけです。</p>
<p>他の推論規則の型を見てみるとこうなります。</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell">ghci<span class="fu">&gt;</span> <span class="fu">:</span>t implLeft
<span class="ot">implLeft ::</span> <span class="dt">Rule</span> [] [<span class="dt">Index</span> <span class="ch">&#39;Z &#39;</span><span class="dt">LHS</span>, <span class="dt">Index</span> <span class="ch">&#39;Z &#39;</span><span class="dt">RHS</span>]

ghci<span class="fu">&gt;</span> <span class="fu">:</span>t andLeftR
<span class="ot">andLeftR ::</span> <span class="dt">Rule</span> [<span class="dt">Formula</span>] []

ghci<span class="fu">&gt;</span> <span class="fu">:</span>t notRight
<span class="ot">notRight ::</span> <span class="dt">Rule</span> [] []</code></pre>
<p>いい感じですね！</p>
<h2 id="簡単な証明支援系">簡単な証明支援系</h2>
<p>上のライブラリを使って書いた証明支援系が、<a href="http://gitweb.konn-san.com/repo/logic/blob/master/assistant.hs"><code>assistant.hs</code></a>です。まだひたすら進んでいくだけの簡単な機能しかなくて、例えば適用の取消をしたりすることは出来ませんが、一通り動きます。</p>
<p>例えば、Peirce の法則 $((P \to Q) \to P) \to P$ の証明は次のように行います。</p>
<pre><code>$$ ./assistant
|- ((P-&gt;Q)-&gt;P)-&gt;P
----------------------
Goal:  |- ((P → Q) → P) → P
&gt; right
----------------------
Goal: (P → Q) → P |- P
&gt; contractR
----------------------
Goal: (P → Q) → P |- P, P
&gt; left () (P)
----------------------
Goal:  |- P, P → Q
&gt; right
----------------------
Goal: P |- P, Q
&gt; weakenR
complete: P |- P
complete: P |- P
[&quot;right&quot;,&quot;contractR&quot;,&quot;left () (P)&quot;,&quot;right&quot;,&quot;weakenR&quot;]</code></pre>
<p>最後のゴールから逆算して逆適用していくわけです。曖昧性のないときは、適用規則の名前を略すことが出来ます。例えば、最初の <code>right</code> は <code>implRight</code> の、二番目の <code>left</code> は <code>implLeft</code> のことです。</p>
<p>こうして全部のシークエントが始式 $A \vdash A$ まで還元されたら証明終了となって、最後に左下から順に適用した規則の列が出力されます。</p>
<h2 id="めくるめく型の応酬">めくるめく型の応酬</h2>
<p>ところで、先程の <code>left</code> つまり <code>implLeft</code>の引数を見てみると、長さを取る筈なのに <code>left () (P)</code> と式の列を取っていますね？ こうしたことを可能にしているのが <code>RuleArgument</code> 型クラスの <code>toArg</code> 函数です（<a href="http://gitweb.konn-san.com/repo/logic/blob/master/SequentTypes.hs">SequentTypes.hs</a> にあります）。Human readable なように左端から論理式列をマッチさせて、その長さを返すような処理をしています。データ表現としては左辺の式は正順のリストですが、右辺式はリストを反転して右側がリストの先頭になるようにしていて左辺と右辺とでする処理がちがうので、型に<code>LHS</code>や<code>RHS</code>でタグ付けをしている訳です。</p>
<p>今のところ論理式の列は規則中に高々二つしか出て来ないので、全部左端から一致する形で同一化しています。なので、新たに$\Delta, \Gamma, \Sigma \vdash A$ のような形のシーケントを扱う規則を追加した場合、一々同じ部分を書く必要性があります。例としては、</p>
<p>$$
A, B, C, D, E \vdash F
$$</p>
<p>みたいなシーケントが与えられたとき、 $\Delta = (A, B), \Gamma = C, \Sigma = (D, E)$ として推論規則 rule を適用したい場合は</p>
<pre><code>rule (A,B) (A,B,C)</code></pre>
<p>などと重複して指定してやる必要性があります。多分。<sup><a href="#fn2" class="footnoteRef" id="fnref2">2</a></sup></p>
<p>SequentTypes.hs では、 assistant を書く時に便利なように</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span class="ot">unapplyList ::</span> (<span class="dt">ListToVector</span> (<span class="dt">Length</span> b), <span class="dt">ApplyVec</span> (<span class="dt">Length</span> b) b)
            <span class="ot">=&gt;</span> <span class="dt">Rule</span> a b <span class="ot">-&gt;</span> [<span class="dt">String</span>] <span class="ot">-&gt;</span> [<span class="dt">Sequent</span>] <span class="ot">-&gt;</span> <span class="dt">Maybe</span> ([<span class="dt">Sequent</span>] <span class="ot">-&gt;</span> [<span class="dt">Sequent</span>])
<span class="ot">applyList ::</span> (<span class="dt">ListToVector</span> (<span class="dt">Length</span> <span class="kw">as</span>), <span class="dt">ApplyVec</span> (<span class="dt">Length</span> <span class="kw">as</span>) <span class="kw">as</span>)
          <span class="ot">=&gt;</span> <span class="dt">Rule</span> <span class="kw">as</span> b <span class="ot">-&gt;</span> [<span class="dt">String</span>] <span class="ot">-&gt;</span> [<span class="dt">Sequent</span>] <span class="ot">-&gt;</span> <span class="dt">Maybe</span> ([<span class="dt">Sequent</span>] <span class="ot">-&gt;</span> [<span class="dt">Sequent</span>])</code></pre>
<p>と云う二つのヘルパ函数を定義しています。型からもわかるとおり、これは文字列（=引数）のリストをルールに適用ないし逆適用する函数です。途中で長さ付きベクトルに変換して、それから適用しています。</p>
<p>この函数を使って<a href="http://gitweb.konn-san.com/repo/logic/blob/master/assistant.hs">assistant.hs</a> ルールを一括適用しているところを見てみましょう。</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span class="kw">case</span> <span class="fu">words</span> cmd <span class="kw">of</span>
  (<span class="st">&quot;cut&quot;</span><span class="fu">:</span>args) <span class="ot">-&gt;</span> unapplyList cut args [fm]
  (<span class="st">&quot;permutationL&quot;</span><span class="fu">:</span>args) <span class="ot">-&gt;</span> unapplyList permutationL args [fm]
  (<span class="st">&quot;permutationR&quot;</span><span class="fu">:</span>rest) <span class="ot">-&gt;</span> unapplyList permutationR rest [fm]
  (<span class="st">&quot;contractL&quot;</span><span class="fu">:</span>rest) <span class="ot">-&gt;</span> unapplyList contractL rest [fm]
  (<span class="st">&quot;contractR&quot;</span><span class="fu">:</span>_) <span class="ot">-&gt;</span> <span class="kw">Just</span> <span class="fu">$$</span> unapply contractR
  (<span class="st">&quot;weakenL&quot;</span><span class="fu">:</span>_) <span class="ot">-&gt;</span> <span class="kw">Just</span> <span class="fu">$$</span> unapply weakenL
  (<span class="st">&quot;weakenR&quot;</span><span class="fu">:</span>_) <span class="ot">-&gt;</span> <span class="kw">Just</span> <span class="fu">$$</span> unapply weakenR
  (<span class="st">&quot;andRight&quot;</span><span class="fu">:</span>args)
    <span class="fu">|</span> [lkseq<span class="fu">|</span> <span class="kw">as</span> <span class="fu">|-</span> gs, a ∧ b <span class="fu">|</span>] <span class="ot">&lt;-</span> fm <span class="ot">-&gt;</span> unapplyList andRight args [seqs<span class="fu">|</span> <span class="kw">as</span> <span class="fu">|-</span> gs <span class="fu">|</span>]
  (<span class="st">&quot;andLeftR&quot;</span><span class="fu">:</span>_) <span class="ot">-&gt;</span> <span class="kw">Just</span> <span class="fu">$$</span> unapply andLeftR
<span class="fu">...</span> many other <span class="fu">lines...</span></code></pre>
<p>実は、ちょっとこれは不正をしています、妙ちきりんな型エラーが出て仕舞うので、ベクトルを <code>[*] :~&gt; fs</code> に適用する為の型クラス <code>ApplyVec</code> を見てみましょう。</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span class="kw">class</span> (<span class="dt">List</span> len <span class="fu">~</span> xs) <span class="ot">=&gt;</span> <span class="dt">ApplyVec</span> len xs <span class="kw">where</span>
  <span class="kw">type</span> <span class="dt">List</span> (<span class="ot">len ::</span> <span class="dt">Nat</span>)<span class="ot"> ::</span> [<span class="fu">*</span>]
<span class="ot">  applyVec ::</span> (xs <span class="fu">:~&gt;</span> a) <span class="ot">-&gt;</span> [<span class="dt">Sequent</span>] <span class="ot">-&gt;</span> <span class="dt">Vector</span> <span class="dt">String</span> len <span class="ot">-&gt;</span> <span class="dt">Maybe</span> a

<span class="kw">instance</span> <span class="dt">ApplyVec</span> <span class="dt">Z</span> <span class="ch">&#39;[] where</span>
  <span class="kw">type</span> <span class="dt">List</span> <span class="dt">Z</span> <span class="fu">=</span> <span class="ch">&#39;[]</span>
  applyVec f _ <span class="dt">VNil</span> <span class="fu">=</span> <span class="kw">Just</span> f

<span class="kw">instance</span> (<span class="dt">RuleArgument</span> x, <span class="dt">ApplyVec</span> len xs) <span class="ot">=&gt;</span> <span class="dt">ApplyVec</span> (<span class="dt">S</span> len) (x <span class="ch">&#39;: xs) where</span>
  <span class="kw">type</span> <span class="dt">List</span> (<span class="dt">S</span> len) <span class="fu">=</span> <span class="dt">List</span> (<span class="dt">S</span> len)
  applyVec f s (<span class="dt">VCons</span> x xs) <span class="fu">=</span>
      <span class="kw">case</span> f <span class="fu">&lt;$$&gt;</span> toArg s x <span class="kw">of</span>
        <span class="kw">Just</span> f&#39; <span class="ot">-&gt;</span> applyVec f&#39; s xs
        <span class="kw">Nothing</span> <span class="ot">-&gt;</span> <span class="kw">Nothing</span></code></pre>
<p>スーパークラスで型同値判定を使っているので、これは関数従属性とほぼ同値です。関数従属性で定義しなおすと、<code>ApplyVec</code>の頭部は</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span class="kw">class</span> <span class="dt">ApplyVec</span> len xs <span class="fu">|</span> len <span class="ot">-&gt;</span> xs <span class="kw">where</span></code></pre>
<p>と書き換えることが出来ます。しかし、これでは具合が悪いです。何故ならこれでは「型リストの長さによって内容が一意に決定される」と云う意味になってしまって、実際 GHC 7.0.* 系では<code>LKRules.hs</code>のコンパイルが通らなくなります<sup><a href="#fn3" class="footnoteRef" id="fnref3">3</a></sup>。 これは、<code>permutationL</code> と <code>permutationR</code> の型を見てみると、</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell">ghci<span class="fu">&gt;</span> <span class="fu">:</span>t permutationL
<span class="ot">permutationL ::</span> <span class="dt">Rule</span> [<span class="dt">Index</span> <span class="ch">&#39;Z &#39;</span><span class="dt">LHS</span>] [<span class="dt">Index</span> <span class="ch">&#39;Z &#39;</span><span class="dt">LHS</span>]

ghci<span class="fu">&gt;</span> <span class="fu">:</span>t permutationR
<span class="ot">permutationR ::</span> <span class="dt">Rule</span> [<span class="dt">Index</span> <span class="ch">&#39;Z &#39;</span><span class="dt">RHS</span>] [<span class="dt">Index</span> <span class="ch">&#39;Z &#39;</span><span class="dt">RHS</span>]</code></pre>
<p>となって、長さ1のリストなのに <code>LHS</code> と <code>RHS</code> で要素となる型が食い違ってしまって一意性を破ってしまうからです。</p>
<p>なので、この版では関連型の <code>List len</code> を循環定義にして、かつそれをどこでも陽には呼ばないことで誤魔化しています。いずれの場合にせよ、型推論の停止性を脅かす虞れがあるので、UndecidableInstances 拡張を有効にする必要があります。UndecidableInstances を使わないで実現する方法があるぞ！と云う方は是非教えて頂けると嬉しいです。</p>
<p>……とまで書いて、この版を GHC 7.4.1 に喰わせてみたら何故か関数従属性版もコンパイル通っちゃいました orz これは何でだろう……？型族+スーパークラス文脈型同値性に desugar されるからとかかなあ。うーむ。</p>
<h2 id="今後の展望">今後の展望</h2>
<p>今後改良すべきところがあるとすれば以下の感じでしょうか。</p>
<ul>
<li>述語論理に対応する
<ul>
<li>置換や変数の出現を陽に扱う必要性があるので、自動生成は難しそう……。</li>
</ul></li>
<li>命題論理の範囲での自動定理証明
<ul>
<li>これは出来ることが証明されていますし、アルゴリズムも非常に簡明なので実装するだけです。推論規則だけ与えてそこから自動証明系を生成するのは無理ですが……。 ただ、全部複製しないで必要なものだけ複製するようにするとか、左辺の$\wedge$や右辺の$\vee$を単なるコンマにバラすのを一々処理として書き下そうとか厳密にルールだけで記述しようと思うと面倒そうです。</li>
</ul></li>
<li>assistant に undo 機能を付ける</li>
<li>推論規則のヘルプ機能</li>
<li>論理式だけでなくて、簡単なL-論理式を扱えるようにしたい
<ul>
<li>これは大変そう……。</li>
</ul></li>
</ul>
<p>まあ自動定理証明が一番今からやろうとうするには簡単そうですね。この場合、上で作り上げてきた機能は余り使わなそうですが……。</p>
<p>命題論理の自動定理証明のアルゴリズムは、風呂入りながら「交換規則と弱化規則と縮約規則を駆使して全式を引き回して、左右辺の$\wedge$や$\vee$をバラすのを自動で挿入するようにすればいいな」とすぐに思い浮かびました。思い浮かんだ後で「あれ？これどっかで見たことあるぞ……？」と思ったら次の本でした。</p>

<p><iframe src="http://rcm-fe.amazon-adsystem.com/e/cm?lt1=_blank&bc1=000000&IS2=1&bg1=FFFFFF&fc1=000000&lc1=0000FF&t=konn06-22&o=9&p=8&l=as4&m=amazon&f=ifr&ref=ss_til&asins=4795268878" style="width:120px;height:240px;" scrolling="no" marginwidth="0" marginheight="0" frameborder="0"></iframe></p>

<p>これは、中高生〜大学一年向けの数学読み物シリーズの『アウト・オブ・コース』シリーズの一つで、僕も小六か中学時代に読んだんですが、これに出て来る「○×ゲーム」と云うものが、本質的には LK を使った命題論理の定理自動証明と等価なものです。</p>
<p><a href="http://www2.odn.ne.jp/yuseisha/out.htm">公式ページ</a>によると</p>
<blockquote>
<p>高校生・大学生に贈る数学入門シリーズ やさしい！　面白い！　ためになる！ 三拍子そろった課外読み物。最近の数学をもう一度 学び直したいという社会人にも好適！　四六判並製</p>
</blockquote>
<p>とのことなので、中高生に限らず興味のある方は是非読まれては如何でしょうか。豊富なパズル（何と犯人当てみたいなものもある！）の例や平易な説明を通して、記号論理学に親しめるインフォーマルな入門書として最適な一冊だと思います。興味を持たれた方は是非。</p>
<section class="footnotes">
<hr>
<ol>
<li id="fn1"><p>厳密には三段論法ではないので、これを三段論法と呼ぶと（数理でない）論理学者に怒られるらしい<a href="#fnref1">↩</a></p></li>
<li id="fn2"><p>多分、と云うのは実際にそういうルールで試してない、と云う意味です。<a href="#fnref2">↩</a></p></li>
<li id="fn3"><p>また、GHC 7.0.* には DataKinds がないので、その場合は EmptyDataDecls などを使って頑張る必要があります。<a href="#fnref3">↩</a></p></li>
</ol>
