------
title: GHC 7.4.1 の型レベル新機能を使い倒す 〜GADTs、型族 と DataKinds、ConstraintKinds の円環〜
author: 石井大海
date: 2012/06/07 00:48:45 JST
description: GADTsや型族が入って、GHCの型システムがリッチになりはじめた頃の解説記事。
tags: Haskell,型理論,形式手法
------
<h2 id="型であそぼう">型であそぼう！</h2>
<p>みなさん今日は！遂に<a href="http://hackage.haskell.org/platform/">Haskell Platform 2012.2.0.0 がリリース</a>されましたね！</p>
<p>そこでこの記事では、シンプルな例を通して、<strong>GADTs</strong> や<strong>型族</strong>、更には GHC 7.4.1 の新機能である<strong>データ型の昇格</strong>と<strong>制約カインド</strong>、などといった型レベルプログラミングの機能を紹介していきたいと思います。こちらが<a href="https://gist.github.com/2882812">題材となるソースコード</a>です。</p>
<h2 id="問題リストの要素を関数に渡したい">問題：リストの要素を関数に渡したい</h2>
<p>Haskell を学びたての頃によくしがちな間違いとして、<strong>リストの要素を関数に適用したくなる</strong>と云うことがあるんじゃないでしょうか。どういうことかと云うと、例えば下のような関数があるとしましょー。</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span class="ot">catThreeFiles ::</span> <span class="fu">FilePath</span> <span class="ot">-&gt;</span> <span class="fu">FilePath</span> <span class="ot">-&gt;</span> <span class="fu">FilePath</span> <span class="ot">-&gt;</span> <span class="dt">IO</span> <span class="dt">String</span></code></pre>
<p>つまり三つのファイルを結合する関数ですね。 こいつを使って、コマンドライン引数を三つ取って、そのパスの内容を読んで連結するプログラムを書きたかったとします。引数の数は必ず三つ渡されると仮定すると、普通なら</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span class="ot">main ::</span> <span class="dt">IO</span> ()
main <span class="fu">=</span> <span class="kw">do</span>
  [f1, f2, f3] <span class="ot">&lt;-</span> getArgs
  <span class="fu">putStrLn</span> <span class="fu">=&lt;&lt;</span> catThreeFiles f1 f2 f3</code></pre>
<p>などと書くと思いますが、<code>foldl</code> などを覚えたてだと、「<code>catThreeFiles</code> に <code>f1</code>, <code>f2</code>, <code>f3</code> を順繰りに適用していけばいいんだから、こう書けるんじゃないか……？」と、</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span class="fu">foldl</span> (<span class="fu">$$</span>) catThreeFiles <span class="fu">=&lt;&lt;</span> getArgs</code></pre>
<p>などと書いて「型が合わねーよ！」と叱られる訳です。</p>
<p><code>foldl</code>を使うことは出来ませんが、この「リストの要素を順次関数に適用する」と云うのを、GHC 7.4.1 の最新機能を使って実現してみたいと思います。</p>
<h2 id="まずおまじない">まずおまじない</h2>
<p>では早速書き始めましょう。以下で使う言語拡張とモジュール群は以下の通りです。</p>
<pre class="sourceCode literate haskell"><code class="sourceCode haskell"><span class="ot">{-# LANGUAGE GADTs, TypeFamilies, DataKinds, ConstraintKinds #-}</span>
<span class="ot">{-# LANGUAGE TypeOperators, ScopedTypeVariables #-}</span>
<span class="kw">module</span> <span class="dt">Main</span> <span class="kw">where</span>
<span class="kw">import</span> <span class="dt">GHC.Exts</span> (<span class="dt">Constraint</span>)
<span class="kw">import</span> <span class="dt">Control.Applicative</span></code></pre>
<h2 id="方針">方針</h2>
<p>さて、関数にリスト要素を喰わせるといっても、幾つか問題があります。それは、</p>
<ol type="1">
<li>どこまでを引数、どこからを返り値とするか？</li>
<li>リストは同一型しか含まないが、関数の引数は異なる型を取り得る</li>
</ol>
<p>と云うことです。一番目の問題はどういうことかと云うと、Haskell はカリー化があるので、</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span class="ot">withFile ::</span> <span class="fu">FilePath</span> <span class="ot">-&gt;</span> <span class="dt">IOMode</span> <span class="ot">-&gt;</span> (<span class="dt">Handle</span> <span class="ot">-&gt;</span> <span class="dt">IO</span> r) <span class="ot">-&gt;</span> <span class="dt">IO</span> r</code></pre>
<p>みたいな関数があったときに、<code>(Handle -&gt; IO r)</code>までが引数で返り値が<code>IO r</code> である関数と見ることも出来ますし、それとも<code>FilePath</code>だけが引数で、<code>IOMode -&gt; (Handle -&gt; IO r) -&gt; IO r</code> が返り値だと見ることも出来ます。</p>
<p>一般に、どちらの場合を採用したいかは用途によって違うので、ここでは引数一覧と返り値を明示したようなデータ型 <code>NAry</code> を定義してみましょう。完全適用の形になって返り値だけになっている状態と、まだ引数を必要としている状態が区別出来ていればよさそうです。</p>
<p>引数を受け付けるような場合はどうあらわせばいいでしょうか？型引数として、関数の引数の型一覧を持たせることが出来るとよいのですが……そんな時に便利なのが、GHC 7.4.1 の新機能、<strong>データ型の昇格</strong>機能（<code>DataKinds</code> 言語拡張）です。</p>
<h2 id="実装の前にカインドの説明">実装の前に：カインドの説明</h2>
<p>データ型の昇格、と云うのは、通常のデータ型を自動的にカインドに持ち上げてくれる機能です。……カインドって何？と云うことで、実装に入る前にカインドについての簡単な説明をしておこうと思います（知っている、と云うひと は飛ばしてしまって構いません）。</p>
<h3 id="ghc-7.4-以前のカインドの世界">GHC 7.4 以前のカインドの世界</h3>
<p>カインドと云うのは、型の型のようなものです。なので、型の類推で考えると分かり易いです。</p>
<p>例えば、<code>&quot;こんにちわーるど！&quot;</code> や <code>Just True</code> のように関数ではない、<em>完全に適用された値</em> は、それぞれ <code>String</code>型、 <code>Maybe Bool</code>型を持ちます。 これと同様に、<code>Int</code> や <code>Maybe Bool</code> のように完全適用の形になっている、カインド <code>*</code> を持つ、とされます。つまり、<code>Int :: *</code>、<code>Maybe Bool :: *</code>です。完全適用の形になっている、と云うのは、直感的には「その型を持つ値が存在する」と云う意味だと思っておけばよいです <sup><a href="#fn1" class="footnoteRef" id="fnref1">1</a></sup>。</p>
<p>一方、値の世界には、<code>odd :: Int -&gt; Bool</code> や <code>(++) :: [a] -&gt; [a] -&gt; [a]</code> のように、何らかの引数を取って値を返すような関数があり、これらは引数の型と返り値を <code>-&gt;</code>（アロー）で区切って型が与えられます。 これと似たように、何らかの型を取って違う型を返すような型のカインド（ややこしいですね！）も、アローを使って表現されます。例えば、<code>Maybe</code> 型はそれ自身では値を持たず、他の <code>*</code> カインドの型（例えば <code>Int</code>）を取って始めて値を持ちます。そこで <code>Maybe</code> のカインドは <code>* -&gt; *</code> となります。同様に、 <code>[a] == [] a</code> に注意すれば、<code>[] :: * -&gt; *</code> ですし、<code>Either</code> は <code>Either Bool Int</code> のように <code>*</code> カインドの型を二つ取って値を持つ型を返すので、<code>Either :: * -&gt; * -&gt; *</code> です。</p>
<p>注意が必要なのは、<em>型</em> <code>Int -&gt; Bool</code> の <em>カインド</em> は <code>*</code> である、と云うことです。一瞬 <code>* -&gt; *</code> のようなカインドを持つように思えるかもしれませんが、<code>Int -&gt; Bool</code> は例えば <code>odd</code> 関数のように <em>値を持つ</em> のでカインド <code>*</code> を持つことになる訳です。</p>
<p>すこしややこしいですね……。まあ、迷ったら、GHCi を開いて <code>:kind</code> コマンドで型のカインドが調べられるので、色々な型のカインドを見てみるとよいでしょう。 とまれ、GHC 7.4.1 以前の世界には <code>*</code> と <code>-&gt;</code> を使って作られるカインドしかなかったと思っておけば問題ないです<sup><a href="#fn2" class="footnoteRef" id="fnref2">2</a></sup>。</p>
<h3 id="データ型の昇格とは何者なのか">データ型の昇格とは何者なのか？</h3>
<p>さて、GHC 7.4.1 からは、上の基本的なカインドに加えて、<code>DataKinds</code>機能拡張を有効にすることで自前のカインドを定義出来るようになりました。</p>
<p>定義出来る、と云っても無節操に定義出来る訳ではなく、<strong>既存のデータ型をカインドに昇格させる</strong>ことが出来るようになった、と云ったほうが正確です。既存の<strong>型</strong>を<strong>カインド</strong>に、<strong>データコンストラクタ</strong>を<strong>型</strong>に持ち上げることが出来るようになったのです。</p>
<p>エッ、どういうこと……？ 掻い摘んで説明しましょう。例えば以下のような自然数を表すデータ宣言があったとします。</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span class="kw">data</span> <span class="dt">Nat</span> <span class="fu">=</span> <span class="dt">Zero</span> <span class="fu">|</span> <span class="dt">Succ</span> <span class="dt">Nat</span></code></pre>
<p>この時、上で定義されている <code>Nat</code> <em>型</em> と <em>データコンストラクタ</em> <code>Zero :: Nat</code>, <code>Succ :: Nat -&gt; Nat</code> に加えて、それぞれ <code>Zero :: Nat</code>, <code>Succ :: Nat -&gt; Nat</code> と云う <em>カインド</em> を持つような <em>型</em> <code>Zero</code>, <code>Succ</code> も自動的に定義されるようになったのです。</p>
<p>うーん。どうもわかりませんね……。それの何が嬉しいの……？というところで、そろそろ例の方に戻りましょうか。</p>
<h2 id="返り値問題型リストで引数の一覧を表現する">返り値問題：型リストで引数の一覧を表現する</h2>
<p>百聞は一見にしかず。思い切って実際に使われている定義を見てしまいましょう。次が関数の引数と返り値をエンコードした <code>NAry as a</code> の定義です。<code>as</code> が引数の型の一覧、<code>a</code>が返り値を表します。</p>
<pre class="sourceCode literate haskell"><code class="sourceCode haskell"><span class="kw">data</span> <span class="dt">NAry</span> (<span class="kw">as</span><span class="ot"> ::</span> [<span class="fu">*</span>]) a <span class="kw">where</span>
  <span class="dt">Value</span><span class="ot"> ::</span> a <span class="ot">-&gt;</span> <span class="dt">NAry</span> <span class="ch">&#39;[] a</span>
  <span class="dt">Arg</span><span class="ot">   ::</span> (b <span class="ot">-&gt;</span> <span class="dt">NAry</span> xs a) <span class="ot">-&gt;</span> <span class="dt">NAry</span> (b <span class="ch">&#39;: xs) a</span></code></pre>
<p><code>data</code>宣言なのに<code>where</code>がついている……？それは、<code>DataKinds</code> 機能拡張の他に、<code>GADTs</code>と云う機能拡張が使われているからです。GADTs を簡単に説明すると、データコンストラクタによって型を柔軟に指定出来る機能です。上の定義を GADTs を使わないで書くと下のようになります。</p>
<pre class="sourceCode literate haskell"><code class="sourceCode haskell"><span class="kw">data</span> <span class="dt">NAry&#39;</span> (<span class="kw">as</span><span class="ot"> ::</span> [<span class="fu">*</span>]) a <span class="fu">=</span> (<span class="kw">as</span> <span class="fu">~</span> <span class="ch">&#39;[]) =&gt; Value&#39;</span> a
                         <span class="fu">|</span> forall b xs <span class="fu">.</span> (<span class="kw">as</span> <span class="fu">~</span> (b <span class="ch">&#39;: xs))</span>
                           <span class="ot">=&gt;</span> <span class="dt">Arg&#39;</span> (b <span class="ot">-&gt;</span> <span class="dt">NAry&#39;</span> xs a)</code></pre>
<p><code>Value</code> コンストラクタでは <code>as ~ '[]</code> と云う制約が追加されていて、<code>Arg'</code>コンストラクタでは <code>b, xs</code> が全称量化されていて、更に <code>as ~ (b ': xs)</code> と云う制約が掛かっていますね。<code>b</code> や <code>xs</code> は左辺に登場しないといけないので <code>forall</code> を伴っているわけですが、では後者の制約はどう云う事でしょう？</p>
<p>一般に、<code>a ~ b</code> と云う型制約は「型 <code>a</code> と型 <code>b</code> は等しい」と云う意味です。従って、上の制約 <code>as ~ (b ': xs)</code> は</p>
<pre class="sourceCode literate haskell"><code class="sourceCode haskell">型 <span class="ot">`as`</span> と、型 <span class="ot">`b &#39;: xs`</span> は等しい</code></pre>
<p>と云う意味になります。何故このような制約を掛けているのでしょうか？それは、型引数 <code>as</code> が右辺のデータコンストラクタの定義に一度も登場しないので、具体的に型を決めてやることが出来なくなるからです。一般に、このように左辺にしか登場しない型引数を持つ型を「<strong>幽霊型</strong>(Phantom Type)」と呼びます。</p>
<p>分かり易くするために、例えば、上の<code>NAry'</code>から型同値性の制約を抜いて</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span class="kw">data</span> <span class="dt">NAry&#39;</span> (<span class="kw">as</span><span class="ot"> ::</span> [<span class="fu">*</span>]) a <span class="fu">=</span> <span class="dt">Value&#39;</span> a
                         <span class="fu">|</span> forall b xs<span class="fu">.</span> <span class="dt">Arg&#39;</span> (b <span class="ot">-&gt;</span> <span class="dt">NAry&#39;</span> xs a)</code></pre>
<p>として GHCi で読み込んで実験してみましょう。</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell">ghci<span class="fu">&gt;</span> <span class="fu">:</span>t <span class="dt">Value</span> <span class="st">&quot;hello&quot;</span>
<span class="dt">Value</span> <span class="st">&quot;hello&quot;</span><span class="ot"> ::</span> <span class="dt">NAry</span> ([] <span class="fu">*</span>) [<span class="dt">Char</span>]

ghci<span class="fu">&gt;</span> <span class="fu">:</span>t <span class="dt">Arg</span> <span class="fu">$$</span> <span class="fu">const</span> <span class="fu">$$</span> <span class="dt">Value</span> <span class="st">&quot;hello&quot;</span>
<span class="dt">Arg</span> <span class="fu">$$</span> <span class="fu">const</span> <span class="fu">$$</span> <span class="dt">Value</span> <span class="st">&quot;hello&quot;</span><span class="ot"> ::</span> <span class="dt">NAry</span> ((<span class="fu">:</span>) <span class="fu">*</span> b ([] <span class="fu">*</span>)) [<span class="dt">Char</span>]

ghci<span class="fu">&gt;</span> <span class="fu">:</span>t <span class="dt">Value&#39;</span> <span class="st">&quot;hello&quot;</span>
<span class="dt">Value&#39;</span> <span class="st">&quot;hello&quot;</span><span class="ot"> ::</span> <span class="dt">NAry&#39;</span> <span class="kw">as</span> [<span class="dt">Char</span>]

ghci<span class="fu">&gt;</span> <span class="fu">:</span>t <span class="dt">Arg&#39;</span> <span class="fu">$$</span> <span class="fu">const</span> <span class="fu">$$</span> <span class="dt">Value&#39;</span> <span class="st">&quot;hello&quot;</span>
<span class="dt">Arg&#39;</span> <span class="fu">$$</span> <span class="fu">const</span> <span class="fu">$$</span> <span class="dt">Value&#39;</span> <span class="st">&quot;hello&quot;</span><span class="ot"> ::</span> <span class="dt">NAry&#39;</span> <span class="kw">as</span> [<span class="dt">Char</span>]</code></pre>
<p>GADTs を使って定義した <code>NAry</code> の方は型の第一引数 <code>as</code> の&quot;型リスト&quot;の型までしっかりと決まっているのに対して、同値性制約を抜いた <code>NAry'</code> の方は、<code>as</code> が型変数のまま残されて仕舞っているのがわかると思います。同値性制約を復活させれば、<code>NAry</code>も <code>NAry'</code>も同様の結果を返すようになる筈です。 このように、幽霊型をコンストラクタで指定してやることで、この例では型に引数の情報を持たせてやっている訳です。</p>
<p>それでは、以上を踏まえて型の定義を読み下していきましょう。</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span class="kw">data</span> <span class="dt">NAry</span> (<span class="kw">as</span><span class="ot"> ::</span> [<span class="fu">*</span>]) a <span class="kw">where</span></code></pre>
<p>早速昇格型のおでましです！<code>as</code> のカインド宣言をみてください。これは、リスト型 <code>[]</code> をカインドレベルに昇格して、要素のカインドを <code>*</code> としたものです。<code>*</code>が値を持つ型を表すことを思い出せば、つまり、<code>[*]</code>はとりもなおさず（完全適用形の）型のリストのことです。よって、</p>
<blockquote>
<p>NAry は型のリスト <code>as</code> と型 <code>a</code> を引数に取る</p>
</blockquote>
<p>と読めます。では、コンストラクタの定義を見ていきましょう。</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span class="dt">Value</span><span class="ot"> ::</span> a <span class="ot">-&gt;</span> <span class="dt">NAry</span> <span class="ch">&#39;[] a</span></code></pre>
<p>これは、引数を適用しきって返り値だけになった状態を表すコンストラクタです。<code>'[]</code> はお馴染の空リストですので、「引数を取らずに<code>a</code>の値を返す」と云う気持ちが表現されているのが判ると思います。 ところで、<code>[]</code>の前についている <code>'</code> は何でしょう？これは、型名とコンストラクタの曖昧性を解決するための印です。リストには、リスト自身を表す <em>型名</em> <code>[]</code> と、空リストを表す <em>データコンストラクタ</em> <code>[]</code> があります。これらは同名なので、型名の文脈に <code>[]</code> が現れたときどちらを指すのか曖昧になってしまいます。それをはっきりさせるため、昇格型を表す際には接頭辞として <code>'</code> を付けるのです。</p>
<p><code>haskell Arg   :: (b -&gt; NAry xs a) -&gt; NAry (b ': xs) a</code> これは、「引数を一つ取る」状態を表すコンストラクタです。これも殆んど良いですね。 <code>':</code> はもちろん cons です。型名と被らないし曖昧性は無いと思うんですが、構文解析の都合なのか何なのか、頭に <code>'</code> を付けないと怒られます。</p>
<p>どうでしょう？急に型リストやGADTsなどが出て来てびっくりしたかもしれませんが、引数の型を型リストとして型引数に持っておく、と云うアイデアさえ押さえればそんなに難しくないと思います。</p>
<h2 id="一般の関数と-nary-の相互変換">一般の関数と <code>NAry</code> の相互変換</h2>
<p>さて、引数と返り値の型を明示した<code>NAry</code>型を定義しました。「リストの要素を関数に適用させる」という目的を達成するために、一般の関数と<code>NAry</code>の相互変換を行う関数を定義しましょう。</p>
<p>まずは <code>NAry</code> から関数への変換関数 <code>fromNAry</code> を書いてみましょう。先ずは型から……</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span class="ot">fromNAry ::</span> <span class="dt">NAry</span> <span class="kw">as</span> a <span class="ot">-&gt;</span> <span class="fu">...?</span></code></pre>
<p>おおっと。変換後の関数の引数の個数がわからないので、このままではちゃんと型を書くことが出来ません。<code>as</code> は型リストなので、こいつから再帰的に関数の型を復元する方法があればいいんだけど……。</p>
<p>そこで活躍するのが、<strong>型族</strong>(Type Families)の機能です！型族とは、型レベルの関数を定義する機能です。どんな感じに使われるのかを見れば大体使い方がわかると思いますので、早速見てみましょう！</p>
<pre class="sourceCode literate haskell"><code class="sourceCode haskell"><span class="kw">type</span> family (<span class="ot">xs ::</span> [<span class="fu">*</span>]) <span class="fu">:~&gt;</span><span class="ot"> a ::</span> <span class="fu">*</span>
<span class="kw">type</span> <span class="kw">instance</span> <span class="ch">&#39;[] :~&gt; a = a</span>
<span class="kw">type</span> <span class="kw">instance</span> (x <span class="ch">&#39;: xs) :~&gt; a = x -&gt; xs :~&gt; a</span>

<span class="kw">infixr</span> <span class="fu">:~&gt;</span></code></pre>
<p>一行目は型族 <code>(:~&gt;)</code> のカインドを宣言している部分と見做すことが出来ます。これは、</p>
<blockquote>
<p>引数の型リスト <code>as :: [*]</code> と最終的な返り値の型 <code>a</code> を受け取って、カインド <code>*</code> の型を返す型レベル関数</p>
</blockquote>
<p>と読みます。<code>TypeOperators</code> 拡張を使うことで、 <code>:</code> から始まる型を中置演算子として定義することが出来ます。</p>
<p>二三行目は、引数の型リストが空リストだったとき、cons だったときに場合分けして定義しています。空リストだった場合は単に<code>a</code>の型、<code>x : xs</code> の形だった場合は、再帰的に <code>x</code> を取って <code>xs :~&gt; a</code> を返す関数の型、として定義しています。 このようにパターンマッチと再帰で定義するのは、普通の関数の定義と殆んど同じですね！</p>
<p>さて。これさえ手に入れば、<code>fromNAry</code>の型は簡単に書けてしまうので、あとは素直に再帰で定義を書くだけです！</p>
<pre class="sourceCode literate haskell"><code class="sourceCode haskell"><span class="ot">fromNAry ::</span> <span class="dt">NAry</span> xs a <span class="ot">-&gt;</span> xs <span class="fu">:~&gt;</span> a
fromNAry (<span class="dt">Value</span> a) <span class="fu">=</span> a
fromNAry (<span class="dt">Arg</span> f)   <span class="fu">=</span> fromNAry <span class="fu">.</span> f</code></pre>
<p>実に簡単ですね！</p>
<p>さて、では次に普通の関数から <code>NAry</code> への変換関数 <code>toNAry</code> を定義してみましょう。</p>
<p>これは、<code>fromNAry</code> の時ほど簡単ではありません。何故かと云うと、最初の方に注意した通り、どこまでを引数として見做すのかを教えてやる必要があるからです。それなら、引数にしたい型リストを最初に渡してやればいいんじゃないの？こんな風に。</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span class="ot">toNAry&#39; ::</span> xs <span class="ot">-&gt;</span> (xs <span class="fu">:~&gt;</span> a) <span class="ot">-&gt;</span> <span class="dt">NAry</span> xs a</code></pre>
<p>しかし、これは駄目です！何故か？例えば <code>xs</code> は具体的には <code>'[String, Int, FilePath]</code> のような型を指す訳ですが、じゃあ、<code>'[String, Int, FilePath]</code> 型の値って何があります……？ありませんよね！<code>'[String, Int, FilePath]</code> 型のカインドは <code>[*]</code> ですが、型が値を持つのはカインドが <code>*</code> の時だけです！</p>
<p>ではどうすればいいのか？こう云う時に使われる技法が<strong>シングルトン</strong>と云うものです。 まずは、取り敢えず次の定義を見てください。</p>
<pre class="sourceCode literate haskell"><code class="sourceCode haskell"><span class="kw">data</span> <span class="dt">SList</span> (<span class="kw">as</span><span class="ot"> ::</span> [<span class="fu">*</span>]) <span class="kw">where</span>
  <span class="dt">SNil</span><span class="ot">  ::</span> <span class="dt">SList</span> <span class="ch">&#39;[]</span>
  <span class="dt">SCons</span><span class="ot"> ::</span> a <span class="ot">-&gt;</span> <span class="dt">SList</span> xs <span class="ot">-&gt;</span> <span class="dt">SList</span> (a <span class="ch">&#39;: xs) </span></code></pre>
<p>これがこれから使うリストのシングルトンです。このトリックは、<code>SList</code>のデータ構造そのものは生のリストと同じで、かつそのデータ構造と同型な <code>[*]</code> 型をその型引数として保持している、というところです。例を見てみましょう。GHCi で読み込んで型を見てみます<sup><a href="#fn3" class="footnoteRef" id="fnref3">3</a></sup>。</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell">ghci<span class="fu">&gt;</span> <span class="fu">:</span>t <span class="dt">SNil</span>
<span class="dt">SNil</span><span class="ot"> ::</span> <span class="dt">SList</span> []

ghci<span class="fu">&gt;</span> <span class="fu">:</span>t <span class="dt">SCons</span> (<span class="fu">undefined</span><span class="ot"> ::</span> <span class="dt">Int</span>) <span class="dt">SNil</span>
<span class="dt">SCons</span> (<span class="fu">undefined</span><span class="ot"> ::</span> <span class="dt">Int</span>) <span class="dt">SNil</span><span class="ot"> ::</span> <span class="dt">SList</span> [<span class="dt">Int</span>]

ghci<span class="fu">&gt;</span> <span class="fu">:</span>t <span class="dt">SCons</span> (<span class="fu">undefined</span><span class="ot"> ::</span> <span class="dt">String</span>) (<span class="dt">SCons</span> (<span class="fu">undefined</span><span class="ot"> ::</span> <span class="dt">Int</span>) <span class="dt">SNil</span>)
<span class="dt">SCons</span> (<span class="fu">undefined</span><span class="ot"> ::</span> <span class="dt">String</span>) (<span class="dt">SCons</span> (<span class="fu">undefined</span><span class="ot"> ::</span> <span class="dt">Int</span>) <span class="dt">SNil</span>)<span class="ot"> ::</span> <span class="dt">SList</span> [<span class="dt">String</span>, <span class="dt">Int</span>]</code></pre>
<p>たしかに、値レベルでのリスト構造と、型レベルでのリスト構造が一致していますね！シングルトンとは、このようにデータコンストラクタと対象となる型の構造を同型にして、そこから型の情報を復元出来るようにしたものです<sup><a href="#fn4" class="footnoteRef" id="fnref4">4</a></sup>。</p>
<p>また、これはシングルトンであることを忘れれば、これはリストの各要素の値が異なるヘテロリストとも見ることが出来ます。ややこしいですが、面倒なので、以後この記事では<code>SList</code>を型リストのシングルトンとヘテロリストの両方の用途に使うことにします。</p>
<p>上のシングルトンを使って <code>toNAry'</code> を実装してみたのが以下です。</p>
<pre class="sourceCode literate haskell"><code class="sourceCode haskell"><span class="ot">toNAry&#39; ::</span> <span class="dt">SList</span> xs <span class="ot">-&gt;</span> (xs <span class="fu">:~&gt;</span> a) <span class="ot">-&gt;</span> <span class="dt">NAry</span> xs a
toNAry&#39; <span class="dt">SNil</span> a <span class="fu">=</span> <span class="dt">Value</span> a
toNAry&#39; (<span class="dt">SCons</span> _ ts) f <span class="fu">=</span> <span class="dt">Arg</span> <span class="fu">$$</span> toNAry&#39; ts <span class="fu">.</span> f</code></pre>
<p>ここでキモとなるのは、<strong>第一引数のシングルトンをパターンマッチしてやることで、それと同型な型レベルでのパターンマッチを行っていること</strong>です。シングルトンとは、このように型レベルのパターンマッチを値レベルで行う為の技法なのです。DataKinds と GADTs の合わせ技でこのようなことが可能になる前は、 <code>toNAry</code> の為の型クラスを態々定義して、<code>xs</code>の型ごとにインスタンスを宣言してやる、という何とも面倒な方法を採る必要がありました。</p>
<p>ところで、文脈によっては <code>xs</code> の型は一意に決まってきて推論出来る場合があるので、省略出来ると便利です。そこで、型クラスを用いて次のようなトリックを使います。</p>
<pre class="sourceCode literate haskell"><code class="sourceCode haskell"><span class="kw">class</span> <span class="dt">SingList</span> k <span class="kw">where</span>
<span class="ot">  slist ::</span> <span class="dt">SList</span> k

<span class="kw">instance</span> <span class="dt">SingList</span> <span class="ch">&#39;[] where</span>
  slist <span class="fu">=</span> <span class="dt">SNil</span>

<span class="kw">instance</span> <span class="dt">SingList</span> xs <span class="ot">=&gt;</span> <span class="dt">SingList</span> (x <span class="ch">&#39;: xs) where</span>
  slist <span class="fu">=</span> <span class="dt">SCons</span> <span class="fu">undefined</span> slist

<span class="ot">toNAry ::</span> forall xs a<span class="fu">.</span> <span class="dt">SingList</span> xs <span class="ot">=&gt;</span> (xs <span class="fu">:~&gt;</span> a) <span class="ot">-&gt;</span> <span class="dt">NAry</span> xs a
toNAry <span class="fu">=</span> toNAry&#39; (<span class="ot">slist ::</span> <span class="dt">SList</span> xs)</code></pre>
<p>つまり、<code>xs</code>の型が確定している場合はそこから <code>SList</code> を作れるので、それを生成するようの型クラスを作ってやる訳です。場合分けをシングルトンの方に押し付けることが出来るので、DataKinds 以前の型レベルプログラミングのように、関数ごとに型クラスを定義してやるハックは必要なくなる訳です。</p>
<p>ところで、この関数の型が <code>forall xs a. SingList xs =&gt; (xs :~&gt; a) -&gt; NAry xs a</code> と量化されているのは、関数定義内部で <code>slist :: SList xs</code> として言及する必要があるからです。量化しないで単に <code>SingList xs =&gt; (xs :~&gt; a) -&gt; NAry xs a</code> と云う型宣言だと、関数定義部で <code>xs</code>型に言及出来ないのです。そこで、GHC の <code>ScopedTypeVariables</code> 拡張を使えば、型変数を陽に量化してやることで定義部で型変数に言及することが出来るようになるのです。</p>
<p>こうして、最初に挙げた問題のうち、 「<strong>1. どこまでを引数、どこからを返り値とするか？</strong>」と云う問題は無事解決しました！やりましたね！</p>
<h3 id="singletons-パッケージ">singletons パッケージ</h3>
<p>上で紹介したシングルトンの技法は、型レベルプログラミングでは一般的に用いられる手法です。型からシングルトンを生成する方法も機械的に出来るので、それを自動化したり型ではなく関数を自動的に昇格させてくれたりする <a href="http://hackage.haskell.org/package/singletons">singletons パッケージ</a>と云うものがあります（実際、この技法を知ったのはこのパッケージの原論文のお陰です）。</p>
<p>現行安定板の 7.4.1 では Template Haskell や カインド多相の機能が不測しているため、残念ながら singletons を使うには GHC HEAD が必要です。7.6以降でそうした先進的な機能が使えるようになった暁には、singletons を使った幸福な型レベルプログラミングが約束されるでしょう……！</p>
<h2 id="値を適用しよう">値を適用しよう！</h2>
<p>それでは、遂にリストの値を <code>NAry</code> に適用しましょう！最初の方で、</p>
<ol start="2" type="1">
<li>リストは同一型しか含まないが、関数の引数は異なる型を取り得る</li>
</ol>
<p>と云う問題を提示しました。これは何の仮定も置かなければ当然解けないので、以下では三つの仮定について採り上げてみます。</p>
<ol type="a">
<li>入力をヘテロリストにする</li>
<li>引数の型が全部 Read のインスタンスだとする</li>
<li>引数の型が全部同じだと仮定する</li>
</ol>
<p>それぞれ順に見ていきましょう。</p>
<h3 id="入力をヘテロリストにする">入力をヘテロリストにする</h3>
<p>最初の条件設定からはいきなり外れますが、入力をヘテロリストだと仮定してしまいましょう！面倒なので、長さも同じとしてしまえば、簡単に以下のようにかけてしまいます。</p>
<pre class="sourceCode literate haskell"><code class="sourceCode haskell"><span class="ot">applyHeteroList ::</span> <span class="dt">NAry</span> <span class="kw">as</span> a <span class="ot">-&gt;</span> <span class="dt">SList</span> <span class="kw">as</span> <span class="ot">-&gt;</span> a
applyHeteroList (<span class="dt">Value</span> a) <span class="dt">SNil</span> <span class="fu">=</span> a
applyHeteroList (<span class="dt">Value</span> a) (<span class="dt">SCons</span> _ _) <span class="fu">=</span> a
applyHeteroList (<span class="dt">Arg</span> f)   (<span class="dt">SCons</span> s ts)  <span class="fu">=</span> applyHeteroList (f s) ts
applyHeteroList (<span class="dt">Arg</span> f)   <span class="dt">SNil</span> <span class="fu">=</span> <span class="fu">error</span> <span class="st">&quot;This would never happen&quot;</span></code></pre>
<p>引数の型リストと渡されているヘテロリストの長さは同じなので、本来なら二行目と最後の行のパターンは必要ありませんが、GHC が incomplete pattern だと云うので書きました。これはとても簡単ですね。</p>
<h3 id="引数の型が全部-read-インスタンスだとする-constraint-カインド">引数の型が全部 Read インスタンスだとする 〜Constraint カインド〜</h3>
<p>もし <code>NAry as a</code> の <code>as</code> の要素が全部 <code>Read</code> のインスタンスであれば、適用関数は以下のように書けます。</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell">applyReadList (<span class="dt">Value</span> a) []     <span class="fu">=</span> <span class="kw">Just</span> a
applyReadList (<span class="dt">Arg</span> f)   (x<span class="fu">:</span>xs) <span class="fu">=</span> applyReadList (f <span class="fu">$$</span> <span class="fu">read</span> x) xs
applyReadList _         _      <span class="fu">=</span> <span class="kw">Nothing</span></code></pre>
<p>ここで、リストの値が足りなかったり、多すぎた場合には <code>Nothing</code> を返す、と云う設計にしました。</p>
<p>ではこの型はどう書けばいいでしょう？素直に書くと、</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span class="ot">applyReadList ::</span> <span class="dt">NAry</span> <span class="kw">as</span> a <span class="ot">-&gt;</span> [<span class="dt">String</span>] <span class="ot">-&gt;</span> <span class="dt">Maybe</span> a</code></pre>
<p>となりそうですが、 <code>as</code> の各要素が <code>Read</code> である、と云う条件が付いていないので、これでコンパイルしようとすると GHC に怒られてしまいます……。</p>
<p>というところで、活躍するのが GHC 7.4.1 の新機能、<strong>制約カインド</strong>(<em>Constraint Kind</em>)です！</p>
<p>制約カインドは、型制約を表す <code>Constraint</code> と云う新しいカインドを導入することで、より柔軟な制約の表現を可能にする言語拡張です。<code>Constraint</code> カインドは <code>GHC.Exts</code> モジュールでエクスポートされています。</p>
<p>型制約って型なの……？と思われるかもしれませんが、Haskellでは、内部的には型クラスの情報は「辞書」として関数の引数に渡されているので、カインドで扱うことが出来る訳です。多分。</p>
<p>具体的には、以下の形の物が <code>Constraint</code> のカインドを持ちます。</p>
<ul>
<li><code>Show a</code> のような型クラス制約</li>
<li><code>?x :: Int</code> のような暗黙パラメーター制約</li>
<li><code>a ~ Int</code> のような同値性制約</li>
<li>空も含めた、<code>Constraint</code>カインドを持つ要素からなるタプル。<code>()</code> や <code>(Show a, as ~ '[])</code> など。</li>
</ul>
<p>例えば、<code>Read :: * -&gt; Constraint</code> ですし、<code>MonadReader :: * -&gt; (* -&gt; *) -&gt; Constraint</code>、<code>(Show a, (xs ~ '[String, Int], ?x :: Int, Read a)) :: Constraint</code> です。</p>
<p>これを使うと、型クラスシノニムのようなものも定義出来ます。例えば、<a href="http://www.kotha.net/ghcguide_ja/latest/constraint-kind.html">ユーザガイド</a> からの引用例として以下のものがあります。</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span class="kw">type</span> <span class="dt">Stringy</span> a <span class="fu">=</span> (<span class="kw">Show</span> a, <span class="kw">Read</span> a)

<span class="ot">foo ::</span> <span class="dt">Stringy</span> a <span class="ot">=&gt;</span> a <span class="ot">-&gt;</span> (<span class="dt">String</span>, <span class="dt">String</span> <span class="ot">-&gt;</span> a)
foo x <span class="fu">=</span> (<span class="fu">show</span> x, <span class="fu">read</span>)</code></pre>
<p>ここで、 <code>Stringy a</code> は 「<code>a</code> は <code>Show</code> と <code>Read</code> 両方のインスタンスである」と云う意味の別名になります。これ以前には、 UndecideableInstances 等を使って</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span class="kw">class</span> <span class="dt">Stringy</span> a
<span class="kw">instance</span> (<span class="kw">Show</span> a, <span class="kw">Read</span> a) <span class="ot">=&gt;</span> <span class="dt">Stringy</span> a</code></pre>
<p>などと宣言してやる必要がありましたが、これでかなりすっきり書けます。</p>
<p>閑話休題。これを使うことで、 <code>as</code> の各要素が <code>Read</code>のインスタンスである、と云う制約を表現することが出来ます！</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span class="kw">type</span> family <span class="dt">Readable</span> (<span class="kw">as</span><span class="ot"> ::</span> [<span class="fu">*</span>])<span class="ot"> ::</span> <span class="dt">Constraint</span>
<span class="kw">type</span> <span class="kw">instance</span> <span class="dt">Readable</span> <span class="ch">&#39;[] = ()</span>
<span class="kw">type</span> <span class="kw">instance</span> <span class="dt">Readable</span> (x <span class="ch">&#39;: xs) = (Read x, Readable xs)</span></code></pre>
<p>まず、対象の型リストが空である場合は、特に何の制約も必要がありません。次に、<code>x : xs</code> の形をしていた場合、まず先頭の要素が<code>Read</code>のインスタンスであることを要求して、リストの残り部分については再帰的に <code>Readable</code> を適用してやれば、結局全部の要素が<code>Read</code>であることが云えます。<code>Constraint</code>要素のタプルもまた <code>Constraint</code> であるため、この定義はちゃんと動作するわです！</p>
<p>後で便利なので、制約を <code>Read</code> から任意の制約述語に一般化した型を定義しておきましょう。</p>
<pre class="sourceCode literate haskell"><code class="sourceCode haskell"><span class="kw">type</span> family   <span class="dt">All</span> (<span class="ot">cxt ::</span> <span class="fu">*</span> <span class="ot">-&gt;</span> <span class="dt">Constraint</span>) (<span class="ot">xs ::</span> [<span class="fu">*</span>])<span class="ot"> ::</span> <span class="dt">Constraint</span>
<span class="kw">type</span> <span class="kw">instance</span> <span class="dt">All</span> cxt <span class="ch">&#39;[]        = ()</span>
<span class="kw">type</span> <span class="kw">instance</span> <span class="dt">All</span> cxt (x <span class="ch">&#39;: xs)  = (cxt x, All cxt xs)</span>

<span class="kw">type</span> <span class="dt">Readable</span> <span class="kw">as</span> <span class="fu">=</span> <span class="dt">All</span> <span class="kw">Read</span> <span class="kw">as</span></code></pre>
<p>やっていることは先程と変わっていません。では、これを使って先程の <code>applyReadList</code> を完成させましょう！</p>
<pre class="sourceCode literate haskell"><code class="sourceCode haskell"><span class="ot">applyReadList ::</span> <span class="dt">Readable</span> <span class="kw">as</span> <span class="ot">=&gt;</span> <span class="dt">NAry</span> <span class="kw">as</span> a <span class="ot">-&gt;</span> [<span class="dt">String</span>] <span class="ot">-&gt;</span> <span class="dt">Maybe</span> a
applyReadList (<span class="dt">Value</span> a) []     <span class="fu">=</span> <span class="kw">Just</span> a
applyReadList (<span class="dt">Arg</span> f)   (x<span class="fu">:</span>xs) <span class="fu">=</span> applyReadList (f <span class="fu">$$</span> <span class="fu">read</span> x) xs
applyReadList _         _      <span class="fu">=</span> <span class="kw">Nothing</span></code></pre>
<p>できました！では、早速試してみましょう。</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell">ghci<span class="fu">&gt;</span> applyReadList (toNAry<span class="ot"> (+) ::</span> <span class="dt">NAry</span> <span class="ch">&#39;[Int, Int] Int) [&quot;12&quot;, &quot;23&quot;]</span>
<span class="kw">Just</span> <span class="dv">35</span></code></pre>
<p>どこまでが引数かを指定してやる必要はありますが、上手く動いてますね！やりました！</p>
<h3 id="引数の型が全部同じだと仮定する">引数の型が全部同じだと仮定する</h3>
<p>先程の例が出来てしまえば。こちらもかなり簡単です！答えを書いてしましましょう〜。</p>
<pre class="sourceCode literate haskell"><code class="sourceCode haskell"><span class="ot">applyHomoList ::</span> <span class="dt">All</span> ((<span class="fu">~</span>) b) <span class="kw">as</span> <span class="ot">=&gt;</span> <span class="dt">NAry</span> <span class="kw">as</span> a <span class="ot">-&gt;</span> [b] <span class="ot">-&gt;</span> <span class="dt">Maybe</span> a
applyHomoList (<span class="dt">Value</span> a) []     <span class="fu">=</span> <span class="kw">Just</span> a
applyHomoList (<span class="dt">Arg</span> f)   (x<span class="fu">:</span>xs) <span class="fu">=</span> applyHomoList (f x) xs
applyHomoList _         _      <span class="fu">=</span> <span class="kw">Nothing</span></code></pre>
<p>型レベルの同値性<code>~</code> は二項演算子のように扱えるので<sup><a href="#fn5" class="footnoteRef" id="fnref5">5</a></sup>、このようにして「<code>as</code>の全部の型が<code>b</code>に等しい」と書いてしまえば、瞬殺なわけです！</p>
<h2 id="まとめ">まとめ</h2>
<p>ここまで、GADTs や 型族といった旧来の機能と、GHC 7.4.1 の DataKinds や ConstraintKinds と云った最新の機能を組み合わせることにで、こんなことまで出来るようになるんだぜ！と云うのを見てきました。 上のヘテロリストの例では長さが全く同じものを仮定しましたが、長さが一致していなくてもよい、とするとどうしたらよいか？また、リストが <code>Dynamic</code> のリストだったりしたりしたらどうか？といったことを考えてみるのも面白いのではないでしょうか。</p>
<p>また、ここでは紹介出来ませんでしたが、<strong>カインド多相</strong>言語拡張も中々に強力です。これは、引数のカインドが多相的であるようなデータ型や型クラス、型族を定義出来るようになる拡張です。これを使うことで、例えば今は別れている <code>Typeable</code>, <code>Typeable1</code>, <code>Typeable2</code>といった型クラスを一つの型クラスで代用することが可能になるのです！</p>
<p>また、先程触れた <code>singletons</code> パッケージも、このカインド多相の機能をふんだんに使っているものとなっています。ただ、7.4.* 系では明示的にカインド変数を宣言出来なかったり、まだまだ機能的には制限されたものになっています。幅を効かせてくるのは、きっと GHC 7.6.1 からとなるので、それまで楽しみに待つことにしましょう。</p>
<p>それでは、このあたりで。 Happy Type Checking!</p>
<section class="footnotes">
<hr>
<ol>
<li id="fn1"><p><code>EmptyDataDecls</code> などが入ってくると、少々間違っていることになりますが、まあ気にしなくていいです。<a href="#fnref1">↩</a></p></li>
<li id="fn2"><p>実際には <code>*</code> に加えて、生のデータ（Unboxed Data）であることを表す <code>#</code> カインドがあります。<a href="#fnref2">↩</a></p></li>
<li id="fn3"><p>型レベルリストを表示する際、<code>(:) * Int ([] *)</code> などと表示されて見辛いので、見易いようにカインド引数を省略して、さらにコンマ区切りのリストの記法に直しています。<a href="#fnref3">↩</a></p></li>
<li id="fn4"><p>ここでは、即値のヘテロリストを型リストのシングルトンとして扱っていますが、本当は型の代理となる幽霊型 <code>Proxy a</code> を定義して、</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span class="kw">data</span> <span class="dt">Proxy</span> a <span class="fu">=</span> <span class="dt">Proxy</span>
<span class="kw">data</span> <span class="dt">SList</span> (<span class="kw">as</span><span class="ot"> ::</span> [<span class="fu">*</span>]) <span class="kw">where</span>
  <span class="dt">SNil</span><span class="ot"> ::</span> <span class="dt">SList</span> <span class="ch">&#39;[]</span>
  <span class="dt">SCons</span><span class="ot"> ::</span> <span class="dt">Proxy</span> a <span class="ot">-&gt;</span> <span class="dt">SList</span> <span class="kw">as</span> <span class="ot">-&gt;</span> <span class="dt">SList</span> (a <span class="ch">&#39;: as)</span></code></pre>
<p>のように <code>Proxy</code>のリストとして定義したほうが良いです。このままでは、型からシングルトンを逆に生成するときに、<code>undefined</code>を使う必要があって精神衛生上宜しくありません。また、型レベルリストの各要素がシングルトンを持つ場合、そのシングルトンのリストとして定義したほうがやり易いことが多いです。 じゃあ最初からそうしろよ、と云う話なんですが、ちょっと時間がないので、ヘテロリストとの兼用をするために実装をサボりました。まあ、そんなに手間は掛からない筈ですが……。<a href="#fnref4">↩</a></p></li>
<li id="fn5"><p>セクションは無理っぽいですが……。<a href="#fnref5">↩</a></p></li>
</ol>

