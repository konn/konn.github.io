---
title: Haskell で Gentzen の LK
date: 2012/05/24 00:00:00 JST
author: 石井大海
description: |
  **この記事はQiitaからの移転記事です**。
  大昔に書いた、シーケント計算をHaskellの型システムを使って上手くエンコードした実装の記事ですが、正直何も整理されていないので、記録以上の意味はありません。
tag: 論理学,logic,Haskell,プログラミング,シーケント計算,Qiita移行記事
---

**この記事はQiitaからの移転記事です**。

大学の講義で LK を習ったので、定理証明支援系を書こうと思った。
その前段階として、取り敢えず論理式に推論規則を適用する仕組みを作ることにした。（後はひっくりかえすだけ）

折角なので、推論規則を書いたら勝手に函数を生成してくれる準クォートを定義してそいつをつかってみた。
こんな感じに書ける。

```haskell:LKRules.hs
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fwarn-unused-imports #-}
module LKRules where
import Data.List
import LKDataTypes
import LKMacros

type Rule = [Sequent] -> Maybe Sequent

isInitial :: Sequent -> Bool
isInitial [lkseq| a |- b |] = a == b

[rule|
# Cut

  Γ |- Δ, A   A, Σ |- Π
--------------------------- cut
       Γ, Σ |- Δ, Π

# Contract
 A, A, Γ |- Δ
--------------- contractL
    A, Γ |- Δ

 Γ |- Δ, A, A
--------------- contractR
 Γ |- Δ, A


# Weakening
    Γ ├ Δ
------------- weakenL
 A, Γ ├ Δ

 Γ ├ Δ
------------- weakenR
 Γ ├ Δ, A

# Rules for AND
 Γ ├ Δ, A   Σ ├ Π, B
-------------------------- andRight
  Γ, Σ ├ Δ, Π, A ∧ B

      A, Γ ├ Δ
------------------ andLeftR
  A ∧ B, Γ ├ Δ

      A, Γ ├ Δ
------------------ andLeftL
  B ∧ A, Γ ├ Δ

# Rules for OR
 A, Γ ├ Δ   B, Σ |- Π
-------------------------- orLeft
  A ∨ B, Γ, Σ |- Δ, Π

 Γ ├ Δ, A
---------------- orRightR
 Γ ├ Δ, A ∨ B

 Γ ├ Δ, A
---------------- orRightL
 Γ ├ Δ, B ∨ A

# Rules for Implies
  A, Γ ├ Δ, B
-------------------------- implRight
     Γ ├ Δ, A → B

  Γ ├ Δ, A   B, Σ ├ Π
--------------------------- implLeft
  A → B , Γ, Σ ├ Δ, Π

# Rules for Not
 A, Γ ├ Δ
------------------ notRight
    Γ ├ Δ, ¬ A

      Γ ├ Δ, A
------------------ notLeft
 ¬ A, Γ ├ Δ
|]

swap s1 s2 ss = 
    let [start, end] = sort [s1, s2]
        (ls, a:rs)   = splitAt start ss
        (ls', b:rs') = splitAt (start-end+1) rs
    in ls ++ b : ls' ++ a : rs'

permutationL :: Int -> Int -> Rule
permutationL s1 s2 [ ss :|- ds ] | max s1 s2 < length ss = Just $ swap s1 s2 ss :|- ds 
permutationL _ _ _ = Nothing

permutationR :: Int -> Int -> Rule
permutationR s1 s2 [ ss :|- ds ] | max s1 s2 < length ds = Just $ ss :|- reverse (swap s1 s2 $ reverse ds)
permutationR _ _ _ = Nothing
```

流石に可換法則はそのまま書くのはむずかしそうなのでやめた。
準クォートにしておくと、今度は逆向きのを自動生成する機能を付けたときに Rules を一切弄らなくてすむので楽だ。
上式にない変数が下式に出て来たら、引数として外側に括り出すようにする処理とかもやっている。あと、上式に同じ文字列が出て来たらガード節をくっつけてそいつらの値が一致する、と云う条件も入れるようにしている。

GHCi で動かしてみるとこんな感じになる。

```haskell
ghci> contractL [seqs| A ∧ A, A ∧ A ├ A |]
Just (A ∧ A |- A)

ghci> weakenL [lkf| A |] [seqs| C |- B |]
Just (A, C |- B)

ghci> weakenR [lkf| A |] [seqs| C |- B |]
Just (C |- B, A)
```

例として二重否定除去を示す。

```haskell
ghci>  [seqs| A |- A |]
[A |- A]

ghci> maybeToList $ notRight it
[ |- A, ¬A]

ghci> maybeToList $ notLeft it
[¬¬A |- A]

ghci> maybeToList $ implRight it
[ |- ¬¬A → A]
```

残りのソースは以下の通り。パーザ周りにまだ繰り返しているコードがあるのでそこを今度何とかする。

```haskell:LKMacros.hs
{-# LANGUAGE TemplateHaskell, ViewPatterns #-}
module LKMacros where
import LKDataTypes
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Data.Char
import Data.Generics
import Control.Applicative
import Text.Parsec
import Data.List
import Data.Foldable (foldrM)
import Control.Monad.State
import qualified  Data.Map as M

lkseq :: QuasiQuoter
lkseq = QuasiQuoter { quoteType = undefined
                    , quoteDec  = undefined
                    , quoteExp  = lkSeqExp
                    , quotePat  = lkSeqPat
                    }

lkSeqExp str = dataToExpQ (mkQ Nothing anti) $ parseSequent str
  where
    anti :: [Formula] -> Maybe ExpQ
    anti gs = Just [| concat $(listE $ map (appE [| toFormulaList |] . dataToExpQ (mkQ Nothing trans)) gs) |]
    trans (Var s) | isFree s = Just $ varE (mkName s)
    trans (Var (c:cs))
        | Just prfx <- lookup c greeks = Just $ varE $ mkName $ prfx:"s"
    trans _ = Nothing
lkSeqPat str = dataToPatQ (mkQ Nothing anti) $ parseSequent str
  where
    anti :: [Formula] -> Maybe PatQ
    anti fs = Just $ foldr1 (\a b -> infixP a ('(:)) b) $ map (dataToPatQ (mkQ Nothing trans)) fs
    trans (Var str) | isFree str = Just $ varP $ mkName str
    trans (Var (c:cs))
        | Just prfx <- lookup c greeks = Just $ varP $ mkName $ prfx:"s"
    trans _ = Nothing

seqs :: QuasiQuoter
seqs = QuasiQuoter { quoteType = undefined
                   , quoteDec  = undefined
                   , quoteExp  = seqsExp
                   , quotePat  = seqsPat
                   }

isFree s = not (null s) && isLower (head s)

greeks = zip "ΓΔΣΠΘΛΞΦΩ" "gdsptlxfw"

seqsExp str = dataToExpQ (mkQ Nothing anti) $ run (sequent `sepBy` spaces) str
  where
    anti :: [Formula] -> Maybe ExpQ
    anti gs = Just [| concat $(listE $ map (appE [| toFormulaList |] . dataToExpQ (mkQ Nothing trans)) gs) |]
    trans (Var s) | isFree s = Just $ varE (mkName s)
    trans (Var (c:cs))
        | Just prfx <- lookup c greeks = Just $ varE $ mkName $ prfx:"s"
    trans _ = Nothing
seqsPat str = dataToPatQ (mkQ Nothing anti) $ run(sequent `sepBy` spaces) str
  where
    anti :: [Formula] -> Maybe PatQ
    anti fs = Just $ foldr1 (\a b -> infixP a ('(:)) b) $ map (dataToPatQ (mkQ Nothing trans)) fs
    trans (Var str) | isFree str = Just $ varP $ mkName str
    trans (Var (c:cs))
        | Just prfx <- lookup c greeks = Just $ varP $ mkName $ prfx:"s"
    trans _ = Nothing

lkf :: QuasiQuoter
lkf = QuasiQuoter { quoteType = undefined
                      , quoteDec  = undefined
                      , quoteExp  = formulaExp
                      , quotePat  = undefined
                      }

formulaExp str = dataToExpQ (const Nothing) $ parseFormula str

rule :: QuasiQuoter
rule = QuasiQuoter { quoteType = undefined
                   , quoteExp  = undefined
                   , quotePat  = undefined
                   , quoteDec  = ruleDec
                   }

ruleDec str = do
  let rules = run (many1 deducRule) str
  concat <$> mapM build rules
  where
    arrow a b = [t| $(a) -> $(b) |]
    build (from, name, to) = do
      (from', dic) <- runStateT (everywhereM (mkM renamer) from) M.empty
      pattern <- listP $ map (dataToPatQ (mkQ Nothing pat)) from'
      body    <- dataToExpQ (mkQ Nothing expq) to
      let pnames = everything (++) (mkQ [] extractName) from
          enames = everything (++) (mkQ [] extractName) to
          unknown = enames \\ pnames
          guards = patG $ map (noBindS . appE [| \xs -> and $ zipWith (==) xs (tail xs) |] . listE . map (varE . mkName . normalizeName) . snd) $ filter (not. null . drop 1 . snd) $ M.toList dic
      sig <- sigD (mkName name) $ foldr arrow [t| [Sequent] -> Maybe Sequent |] $
         map (const [t| Formula |]) unknown
      fun <- funD (mkName name) [ clause (map (varP . mkName) unknown++[return pattern])
                                  (guardedB [liftM2 (,) guards $ appE [|Just|] (return body)])
                                  []
                         , clause (wildP:map (const wildP) unknown) (normalB [|Nothing|]) []
                         ]
      return [sig, fun]
    expq :: [Formula] -> Maybe ExpQ
    expq gs = Just [| concat $(listE $ map (appE [| toFormulaList |] . dataToExpQ (mkQ Nothing $ trans varE)) gs) |]
    pat :: [Formula] -> Maybe PatQ
    pat fs = Just $ foldr1 (\a b -> infixP a ('(:)) b) $
               map (dataToPatQ (mkQ Nothing $ trans varP)) fs
    trans var (Var str) = Just $ var $ mkName $ normalizeName str
    trans _ _ = Nothing
    normalizeName [c]
        | Just prfx <- lookup c greeks = prfx : "s"
    normalizeName xs = map toLower xs
    extractName (Var v) = [normalizeName v]
    extractName _       = []
    renamer (Var name) = do
      dic <- get
      case M.lookup name dic of
        Nothing -> do
          put $ M.insert name [name] dic
          return $ Var name
        Just xs -> do
          name' <- show <$> lift (newName name)
          put $ M.insert name (name' : xs) dic
          return $ Var name'
```

```haskell:LKDataTypes.hs
{-# LANGUAGE DeriveDataTypeable, RecordWildCards, FlexibleInstances #-}
module LKDataTypes where
import Text.Parsec.Expr
import Text.Parsec
import qualified Text.Parsec.Token as T
import Text.Parsec.String
import Data.List
import Control.Applicative hiding (many, (<|>))
import Data.List
import Data.Either
import Data.Data (Typeable, Data)

infix  6 :|-
infixr 7 :->:
infixl 8 :\/:
infixl 9 :/\:

data Formula = Var String
             | Not Formula
             | Formula :\/: Formula
             | Formula :->: Formula
             | Formula :/\: Formula
               deriving (Read, Eq, Ord, Data, Typeable)

data Sequent = (:|-) { lefts :: [Formula], rights :: [Formula] }
             deriving (Eq, Ord, Data, Typeable)

instance Show Formula where
  showsPrec _ (Var v)    = showString v
  showsPrec d (Not f)    = showString "¬" . showsPrec 11 f
  showsPrec d (l :\/: r) = showParen (d > 7) $ showsPrec 8 l . showString " ∨ " . showsPrec 8 r
  showsPrec d (l :->: r) = showParen (d > 6) $ showsPrec 7 l . showString " → " . showsPrec 7 r
  showsPrec d (l :/\: r) = showParen (d > 8) $ showsPrec 9 l . showString " ∧ " . showsPrec 9 r

instance Show Sequent where
  showsPrec d (l :|- r) = showParen (d > 10) $ showsFs l . showString " |- " . showsFs (reverse r)
    where
      showsFs fs = foldr (.) id $ intersperse (showString ", ") $ map shows fs

greek = oneOf "ΓΔΣΠΘΛΞΦΨΩ"

lang :: T.LanguageDef ()
lang = T.LanguageDef { T.commentStart = "{-"
                     , T.commentEnd   = "-}"
                     , T.commentLine  = "#"
                     , T.nestedComments = True
                     , T.identStart     = letter <|> greek
                     , T.identLetter    = alphaNum <|> greek <|> char '\''
                     , T.opStart        = empty
                     , T.opLetter       = empty
                     , T.reservedNames  = []
                     , T.reservedOpNames = ["~", "->", "\\/", "/\\", "⊃", "|-"
                                           ,"¬", "→", "∧", "∨", "^", "v", "├"
                                           ]
                     , T.caseSensitive   = True
                     }

T.TokenParser {..} = T.makeTokenParser lang

formula :: Parser Formula
formula = buildExpressionParser table term
      <?> "formula"

term = parens formula
   <|> Var <$> identifier

table = [ [ Prefix $ Not   <$ choice (map reservedOp ["~", "¬"])]
        , [ Infix  ((:/\:) <$ choice (map reservedOp ["∧", "/\\", "^"])) AssocLeft ]
        , [ Infix  ((:\/:) <$ choice (map reservedOp ["∨", "\\/", "v"])) AssocLeft ]
        , [ Infix  ((:->:) <$ choice (map reservedOp ["→", "->", "⊃"])) AssocRight ]
        ]

sequent = (:|-) <$> fs <* (choice $ map reservedOp ["|-", "├"])
                <*> (reverse <$> fs)
  where
    fs = formula `sepBy` comma

parseSequent = run sequent
parseFormula = run formula

run p src =
    case parse (whiteSpace *> p <* eof) "" src of
      Left err -> error $ show err
      Right ans -> ans

class FormulaListable a where
  toFormulaList :: a -> [Formula]

instance FormulaListable Formula where
  toFormulaList = pure

instance FormulaListable [Formula] where
  toFormulaList = id

deducRule = (,,) <$> (sequent `sepBy` whiteSpace)
                 <*  lexeme (skipMany1 (char '-'))
                 <*> identifier
                 <*> sequent
```
