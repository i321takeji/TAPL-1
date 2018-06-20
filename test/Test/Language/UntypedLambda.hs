{-# LANGUAGE OverloadedStrings #-}
module Test.Language.UntypedLambda where

import           Prelude                         hiding (and, fst, head, id,
                                                  not, or, snd, tail)

import           Test.Tasty
import           Test.Tasty.HUnit

import           Language.UntypedLambda
import qualified Language.UntypedLambda.Examples as UL
import           Language.UntypedLambda.Prelude
import           Language.Utils.Pretty

import           Data.Either
import qualified Data.Set                        as Set

test_ul :: TestTree
test_ul = testGroup "UntypedLambda"
  [ testCase "pretty" $ do
      prettyText (TmVar "x") @?= "x"
      prettyText (TmLam "x" "x") @?= "λx. x"
      prettyText (TmApp "x" "y") @?= "x y"
  , testCase "parser" $ do
      runUlParser "x" @?= Right "x"
      runUlParser "x1y" @?= Right "x1y"
      isLeft (runUlParser "Xyz") @?= True
      isLeft (runUlParser "123x") @?= True
      runUlParser "λx. t" @?= Right (TmLam "x" "t")
      isLeft (runUlParser "λ. Ab") @?= True
      runUlParser "s t u" @?= Right UL.example1
      runUlParser "s t u" @?= runUlParser "(s t) u"
      runUlParser "λx. λy. x y x" @?= Right UL.example2
      runUlParser "λx. λy. x y x" @?= runUlParser "λx. (λy. ((x y) x))"
      runUlParser "(λx.x) ((λx.x) (λz.(λx.x) z))" @?= Right UL.example3

      -- Bool
      runUlParser "tru"  @?= Right tru
      runUlParser "fls"  @?= Right fls
      runUlParser "test" @?= Right test
      runUlParser "and"  @?= Right and
      runUlParser "or"   @?= Right or
      runUlParser "not"  @?= Right not

      -- pair
      runUlParser "pair" @?= Right pair
      runUlParser "fst"  @?= Right fst
      runUlParser "snd"  @?= Right snd

      -- church
      runUlParser "c10"      @?= Right (c 10)
      runUlParser "c0"       @?= Right (c 0)
      runUlParser "c123"     @?= Right (c 123)
      runUlParser "scc"      @?= Right scc
      runUlParser "plus"     @?= Right plus
      runUlParser "times"    @?= Right times
      runUlParser "iszro"    @?= Right iszro
      runUlParser "prd"      @?= Right prd
      runUlParser "subtract" @?= Right subtract1
      runUlParser "equal"    @?= Right equal

      -- list
      runUlParser "nil"   @?= Right nil
      runUlParser "head"  @?= Right head
      runUlParser "isnil" @?= Right isnil
      runUlParser "cons"  @?= Right cons
      runUlParser "tail"  @?= Right tail
  , testCase "isClosed" $ do
      isClosed UL.example1 @?= False
      isClosed UL.example2 @?= True
      isClosed UL.example3 @?= True
      isClosed UL.example4 @?= False
      isClosed UL.example5 @?= True
  , testCase "evaluate (NormalOrder)" $ do
      reduceNormalOrder (TmApp (TmLam "x" "x") "y") @?= TmVar "y"
      reduceNormalOrder UL.example6 @?= TmApp (TmApp "u" "r") (TmLam "x" "x")
      reduceNormalOrder (TmApp (TmLam "x" (TmLam "y" (TmApp "x" "y"))) "z") @?= TmLam "y" (TmApp "z" "y")

      -- 評価戦略の共通の例
      reduceNormalOrder UL.example3 @?= TmApp id (TmLam "z" (TmApp id "z"))
      reduceNormalOrder (TmApp id (TmLam "z" (TmApp id "z"))) @?= TmLam "z" (TmApp id "z")
      reduceNormalOrder (TmLam "z" (TmApp id "z")) @?= TmLam "z" "z"
      reduceNormalOrder (TmLam "z" "z") @?= TmLam "z" "z"
  , testCase "evaluate (CallByName)" $ do
      reduceCallByName UL.example3 @?= TmApp id (TmLam "z" (TmApp id "z"))
      reduceCallByName (TmApp id (TmLam "z" (TmApp id "z"))) @?= TmLam "z" (TmApp id "z")
      reduceCallByName (TmLam "z" (TmApp id "z")) @?= TmLam "z" (TmApp id "z")
  , testCase "evaluate (CallByValue)" $ do
      reduceCallByValue UL.example3 @?= TmApp id (TmLam "z" (TmApp id "z"))
      reduceCallByValue (TmApp id (TmLam "z" (TmApp id "z"))) @?= TmLam "z" (TmApp id "z")
      reduceCallByValue (TmLam "z" (TmApp id "z")) @?= TmLam "z" (TmApp id "z")
  , testCase "evaluate" $ do
      eval NormalOrder UL.example3 @?= TmLam "z" "z"
      eval CallByName  UL.example3 @?= TmLam "z" (TmApp id "z")
      eval CallByValue UL.example3 @?= TmLam "z" (TmApp id "z")
  , testCase "Church ブール値" $ do
      eval NormalOrder UL.example7 @?= tru
      eval CallByName  UL.example7 @?= tru
      eval CallByValue UL.example7 @?= tru

      -- and
      eval NormalOrder UL.example8 @?= tru
      eval CallByName  UL.example8 @?= tru
      eval CallByValue UL.example8 @?= tru
      eval NormalOrder UL.example9 @?= fls
      eval CallByName  UL.example9 @?= fls
      eval CallByValue UL.example9 @?= fls

      -- or
      eval NormalOrder (TmApp (TmApp or tru) fls) @?= tru
      eval CallByName  (TmApp (TmApp or tru) fls) @?= tru
      eval CallByValue (TmApp (TmApp or tru) fls) @?= tru
      eval NormalOrder (TmApp (TmApp or fls) fls) @?= fls
      eval CallByName  (TmApp (TmApp or fls) fls) @?= fls
      eval CallByValue (TmApp (TmApp or fls) fls) @?= fls

      -- not
      eval NormalOrder (TmApp not fls) @?= tru
      eval CallByName  (TmApp not fls) @?= tru
      eval CallByValue (TmApp not fls) @?= tru
      eval NormalOrder (TmApp not tru) @?= fls
      eval CallByName  (TmApp not tru) @?= fls
      eval CallByValue (TmApp not tru) @?= fls
  , testCase "二つ組" $ do
      eval NormalOrder UL.example10 @?= TmVar "v"
      eval CallByName  UL.example10 @?= TmVar "v"
      eval CallByValue UL.example10 @?= TmVar "v"
  , testCase "Church数" $ do
      c 0 @?= TmLam "s" (TmLam "z" "z")
      c 1 @?= TmLam "s" (TmLam "z" (TmApp "s" "z"))
      c 2 @?= TmLam "s" (TmLam "z" (TmApp "s" (TmApp "s" "z")))
      c 3 @?= TmLam "s" (TmLam "z" (TmApp "s" (TmApp "s" (TmApp "s" "z"))))

      -- scc
      eval NormalOrder (TmApp scc (c 0)) @?= c 1
      -- 抽象の本体の適用は許可されないため
      eval CallByName  (TmApp scc (c 0)) @?= TmLam "s" (TmLam "z" (TmApp "s" (TmApp (TmApp (c 0) "s") "z")))
      eval CallByValue (TmApp scc (c 0)) @?= TmLam "s" (TmLam "z" (TmApp "s" (TmApp (TmApp (c 0) "s") "z")))
      eval NormalOrder (TmApp scc (c 0)) @?= eval NormalOrder (TmApp scc2 (c 0))
      eval NormalOrder (TmApp scc (c 1)) @?= eval NormalOrder (TmApp scc2 (c 1))
      eval NormalOrder (TmApp scc (c 2)) @?= eval NormalOrder (TmApp scc2 (c 2))

      -- plus
      eval NormalOrder (TmApp (TmApp plus (c 5)) (c 10))  @?= c 15
      eval NormalOrder (TmApp (TmApp plus (c 10)) (c 20)) @?= c 30

      -- times
      eval NormalOrder (TmApp (TmApp times (c 5)) (c 10))  @?= c 50
      eval NormalOrder (TmApp (TmApp times (c 10)) (c 20)) @?= c 200
      eval NormalOrder (TmApp (TmApp times (c 5)) (c 10))  @?= eval NormalOrder (TmApp (TmApp times2 (c 5)) (c 10))
      eval NormalOrder (TmApp (TmApp times (c 10)) (c 20)) @?= eval NormalOrder (TmApp (TmApp times2 (c 10)) (c 20))
      eval NormalOrder (TmApp (TmApp times (c 5)) (c 10))  @?= eval NormalOrder (TmApp (TmApp times3 (c 5)) (c 10))
      eval NormalOrder (TmApp (TmApp times (c 10)) (c 20)) @?= eval NormalOrder (TmApp (TmApp times3 (c 10)) (c 20))

      -- power
      eval NormalOrder (TmApp (TmApp power1 (c 2)) (c 3)) @?= c 8
      eval NormalOrder (TmApp (TmApp power1 (c 2)) (c 0)) @?= c 1

      -- TODO https://github.com/waddlaw/TAPL/issues/13
      eval NormalOrder (TmApp (TmApp power2 (c 2)) (c 1))  @?= c 2
      -- eval NormalOrder (TmApp (TmApp power2 (c 2)) (c 2))  @?= c 4
      eval NormalOrder (TmApp (TmApp power2 (c 0)) (c 2))  @?= c 0
      -- eval NormalOrder (TmApp (TmApp power2 (c 2)) (c 0))  @?= c 1

      -- iszro
      eval NormalOrder (TmApp iszro (c 1)) @?= fls
      eval NormalOrder (TmApp iszro (TmApp (TmApp times (c 0)) (c 2))) @?= tru

      -- prd
      eval NormalOrder (TmApp prd (c 0)) @?= c 0
      eval NormalOrder (TmApp prd (c 1)) @?= c 0
      eval NormalOrder (TmApp prd (c 2)) @?= c 1

      -- subtract1
      eval NormalOrder (TmApp (TmApp subtract1 (c 10)) (c 2)) @?= c 8
      eval NormalOrder (TmApp (TmApp subtract1 (c 0))  (c 2)) @?= c 0
      eval NormalOrder (TmApp (TmApp subtract1 (c 10)) (c 0)) @?= c 10

      -- equal
      eval NormalOrder (TmApp (TmApp equal (c 10)) (c 2)) @?= fls
      eval NormalOrder (TmApp (TmApp equal (c 2))  (c 2)) @?= tru
  , testCase "List" $ do
      -- cons
      eval NormalOrder (TmApp (TmApp cons "x") nil) @?= TmLam "c" (TmLam "n" (TmApp (TmApp "c" "x") "n"))

      -- isnil
      eval NormalOrder (TmApp isnil nil) @?= tru
      eval NormalOrder (TmApp isnil (TmLam "c" (TmLam "n" (TmApp (TmApp "c" "x") "n")))) @?= fls
      eval NormalOrder (TmApp isnil (TmLam "c" (TmLam "n" (TmApp (TmApp "c" "x1") (TmApp (TmApp "c" "x2") "n"))))) @?= fls

      -- head
      eval NormalOrder (TmApp head nil) @?= nil
      eval NormalOrder (TmApp head (TmLam "c" (TmLam "n" (TmApp (TmApp "c" "x") "n")))) @?= "x"

      -- tail
      eval NormalOrder (TmApp tail nil) @?= nil
      eval NormalOrder (TmApp tail (TmLam "c" (TmLam "n" (TmApp (TmApp "c" "x") "n")))) @?= nil
      eval NormalOrder (TmApp tail (TmLam "c" (TmLam "n" (TmApp (TmApp "c" "x") (TmApp (TmApp "c" "y") "n"))))) @?= TmLam "c" (TmLam "n" (TmApp (TmApp "c" "y") "n"))
  , testCase "bindVars" $ do
      bindVars Set.empty UL.example1 @?= Set.empty
      bindVars Set.empty UL.example2 @?= Set.fromList ["x", "y"]
      bindVars Set.empty UL.example3 @?= Set.fromList ["x", "z"]
      bindVars Set.empty UL.example4 @?= Set.fromList ["x"]
      bindVars Set.empty UL.example5 @?= Set.fromList ["z", "x", "y"]
      bindVars Set.empty UL.example6 @?= Set.fromList ["x"]
      bindVars Set.empty UL.example7 @?= Set.fromList ["f", "l", "m", "n", "t"]
      bindVars Set.empty UL.example8 @?= Set.fromList ["b", "c", "f", "t"]
      bindVars Set.empty UL.example9 @?= Set.fromList ["b", "c", "f", "t"]
      bindVars Set.empty UL.example10 @?= Set.fromList ["b", "f", "p", "s", "t"]
      bindVars Set.empty UL.example11 @?= Set.fromList ["c", "n"]
  , testCase "alphaConv" $ do
      alphaConv "x" (TmLam "x" "x") @?= TmLam "x'" "x'"
      alphaConv "x" (TmLam "x" (TmLam "y" "x")) @?= TmLam "x'" (TmLam "y" "x'")
      alphaConv "y" (TmLam "x" (TmLam "y" "x")) @?= TmLam "x" (TmLam "y'" "x")
  ]
