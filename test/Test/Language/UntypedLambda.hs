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

import           Control.Monad.Trans.State.Lazy

import           Data.Either
import qualified Data.Set                        as Set

test_ul :: TestTree
test_ul = testGroup "UntypedLambda"
  [ testCase "pretty" $ do
      prettyText (mkVar "x") @?= "x"
      prettyText (mkLam "x" "x") @?= "λx. x"
      prettyText (mkApp "x" "y") @?= "x y"
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
      evalState (reduceNormalOrder (TmApp (TmLam "x" "x") "y")) 0 @?= TmVar "y"
      evalState (reduceNormalOrder UL.example6) 0 @?= TmApp (TmApp "u" "r") (TmLam "x" "x")
      evalState (reduceNormalOrder (TmApp (TmLam "x" (TmLam "y" (TmApp "x" "y"))) "z")) 0 @?= TmLam "y" (TmApp "z" "y")

      -- 評価戦略の共通の例
      evalState (reduceNormalOrder UL.example3) 0 @?= TmApp id (TmLam "z" (TmApp id "z"))
      evalState (reduceNormalOrder (TmApp id (TmLam "z" (TmApp id "z")))) 0 @?= TmLam "z" (TmApp id "z")
      evalState (reduceNormalOrder (TmLam "z" (TmApp id "z"))) 0 @?= TmLam "z" "z"
      evalState (reduceNormalOrder (TmLam "z" "z")) 0 @?= TmLam "z" "z"
  , testCase "evaluate (CallByName)" $ do
      evalState (reduceCallByName UL.example3) 0 @?= TmApp id (TmLam "z" (TmApp id "z"))
      evalState (reduceCallByName (TmApp id (TmLam "z" (TmApp id "z")))) 0 @?= TmLam "z" (TmApp id "z")
      evalState (reduceCallByName (TmLam "z" (TmApp id "z"))) 0 @?= TmLam "z" (TmApp id "z")
  , testCase "evaluate (CallByValue)" $ do
      evalState (reduceCallByValue UL.example3) 0 @?= TmApp id (TmLam "z" (TmApp id "z"))
      evalState (reduceCallByValue (TmApp id (TmLam "z" (TmApp id "z")))) 0 @?= TmLam "z" (TmApp id "z")
      evalState (reduceCallByValue (TmLam "z" (TmApp id "z"))) 0 @?= TmLam "z" (TmApp id "z")
  , testCase "evaluate" $ do
      runEval NormalOrder UL.example3 @?= TmLam "z" "z"
      runEval CallByName  UL.example3 @?= TmLam "z" (TmApp id "z")
      runEval CallByValue UL.example3 @?= TmLam "z" (TmApp id "z")
  , testCase "Church ブール値" $ do
      runEval NormalOrder UL.example7 @?= tru
      runEval CallByName  UL.example7 @?= tru
      runEval CallByValue UL.example7 @?= tru

      -- and
      runEval NormalOrder UL.example8 @?= tru
      runEval CallByName  UL.example8 @?= tru
      runEval CallByValue UL.example8 @?= tru
      runEval NormalOrder UL.example9 @?= fls
      runEval CallByName  UL.example9 @?= fls
      runEval CallByValue UL.example9 @?= fls

      -- or
      runEval NormalOrder (TmApp (TmApp or tru) fls) @?= tru
      runEval CallByName  (TmApp (TmApp or tru) fls) @?= tru
      runEval CallByValue (TmApp (TmApp or tru) fls) @?= tru
      runEval NormalOrder (TmApp (TmApp or fls) fls) @?= fls
      runEval CallByName  (TmApp (TmApp or fls) fls) @?= fls
      runEval CallByValue (TmApp (TmApp or fls) fls) @?= fls

      -- not
      runEval NormalOrder (TmApp not fls) @?= tru
      runEval CallByName  (TmApp not fls) @?= tru
      runEval CallByValue (TmApp not fls) @?= tru
      runEval NormalOrder (TmApp not tru) @?= fls
      runEval CallByName  (TmApp not tru) @?= fls
      runEval CallByValue (TmApp not tru) @?= fls
  , testCase "二つ組" $ do
      runEval NormalOrder UL.example10 @?= TmVar "v"
      runEval CallByName  UL.example10 @?= TmVar "v"
      runEval CallByValue UL.example10 @?= TmVar "v"
  , testCase "Church数" $ do
      c 0 @?= TmLam "s" (TmLam "z" "z")
      c 1 @?= TmLam "s" (TmLam "z" (TmApp "s" "z"))
      c 2 @?= TmLam "s" (TmLam "z" (TmApp "s" (TmApp "s" "z")))
      c 3 @?= TmLam "s" (TmLam "z" (TmApp "s" (TmApp "s" (TmApp "s" "z"))))

      -- scc
      runEval NormalOrder (TmApp scc (c 0))  @?= c 1
      runEval NormalOrder (TmApp scc (c 1))  @?= c 2
      runEval NormalOrder (TmApp scc2 (c 0)) @?= c 1
      runEval NormalOrder (TmApp scc2 (c 1)) @?= c 2
      -- 抽象の本体の適用は許可されないため
      runEval CallByName  (TmApp scc (c 0)) @?= TmLam "s" (TmLam "z" (TmApp "s" (TmApp (TmApp (c 0) "s") "z")))
      runEval CallByValue (TmApp scc (c 0)) @?= TmLam "s" (TmLam "z" (TmApp "s" (TmApp (TmApp (c 0) "s") "z")))
      runEval NormalOrder (TmApp scc (c 0)) @?= runEval NormalOrder (TmApp scc2 (c 0))
      -- runEval NormalOrder (TmApp scc (c 1)) @?= runEval NormalOrder (TmApp scc2 (c 1))
      -- runEval NormalOrder (TmApp scc (c 2)) @?= runEval NormalOrder (TmApp scc2 (c 2))

      -- plus
      -- runEval NormalOrder (TmApp (TmApp plus (c 5)) (c 10))  @?= c 15
      -- runEval NormalOrder (TmApp (TmApp plus (c 10)) (c 20)) @?= c 30

      -- times
      -- runEval NormalOrder (TmApp (TmApp times (c 5)) (c 10))  @?= c 50
      -- runEval NormalOrder (TmApp (TmApp times (c 10)) (c 20)) @?= c 200
      -- runEval NormalOrder (TmApp (TmApp times (c 5)) (c 10))  @?= runEval NormalOrder (TmApp (TmApp times2 (c 5)) (c 10))
      -- runEval NormalOrder (TmApp (TmApp times (c 10)) (c 20)) @?= runEval NormalOrder (TmApp (TmApp times2 (c 10)) (c 20))
      -- runEval NormalOrder (TmApp (TmApp times (c 5)) (c 10))  @?= runEval NormalOrder (TmApp (TmApp times3 (c 5)) (c 10))
      -- runEval NormalOrder (TmApp (TmApp times (c 10)) (c 20)) @?= runEval NormalOrder (TmApp (TmApp times3 (c 10)) (c 20))

      -- power
      -- runEval NormalOrder (TmApp (TmApp power1 (c 2)) (c 3)) @?= c 8
      -- runEval NormalOrder (TmApp (TmApp power1 (c 2)) (c 0)) @?= c 1

      -- TODO https://github.com/waddlaw/TAPL/issues/13
      -- runEval NormalOrder (TmApp (TmApp power2 (c 2)) (c 1))  @?= c 2
      -- runEval NormalOrder (TmApp (TmApp power2 (c 2)) (c 2))  @?= c 4
      runEval NormalOrder (TmApp (TmApp power2 (c 0)) (c 2))  @?= c 0
      -- runEval NormalOrder (TmApp (TmApp power2 (c 2)) (c 0))  @?= c 1

      -- iszro
      runEval NormalOrder (TmApp iszro (c 1)) @?= fls
      runEval NormalOrder (TmApp iszro (TmApp (TmApp times (c 0)) (c 2))) @?= tru

      -- prd
      -- runEval NormalOrder (TmApp prd (c 0)) @?= c 0
      -- runEval NormalOrder (TmApp prd (c 1)) @?= c 0
      -- runEval NormalOrder (TmApp prd (c 2)) @?= c 1

      -- subtract1
      -- runEval NormalOrder (TmApp (TmApp subtract1 (c 10)) (c 2)) @?= c 8
      -- runEval NormalOrder (TmApp (TmApp subtract1 (c 0))  (c 2)) @?= c 0
      -- runEval NormalOrder (TmApp (TmApp subtract1 (c 10)) (c 0)) @?= c 10

      -- equal
      -- runEval NormalOrder (TmApp (TmApp equal (c 10)) (c 2)) @?= fls
      -- runEval NormalOrder (TmApp (TmApp equal (c 2))  (c 2)) @?= tru
  , testCase "List" $ do
      -- cons
      runEval NormalOrder (TmApp (TmApp cons "x") nil) @?= TmLam "c" (TmLam "n" (TmApp (TmApp "c" "x") "n"))

      -- isnil
      runEval NormalOrder (TmApp isnil nil) @?= tru
      runEval NormalOrder (TmApp isnil (TmLam "c" (TmLam "n" (TmApp (TmApp "c" "x") "n")))) @?= fls
      runEval NormalOrder (TmApp isnil (TmLam "c" (TmLam "n" (TmApp (TmApp "c" "x1") (TmApp (TmApp "c" "x2") "n"))))) @?= fls

      -- head
      runEval NormalOrder (TmApp head nil) @?= nil
      runEval NormalOrder (TmApp head (TmLam "c" (TmLam "n" (TmApp (TmApp "c" "x") "n")))) @?= "x"

      -- tail
      -- runEval NormalOrder (TmApp tail nil) @?= nil
      -- runEval NormalOrder (TmApp tail (TmLam "c" (TmLam "n" (TmApp (TmApp "c" "x") "n")))) @?= nil
      -- runEval NormalOrder (TmApp tail (TmLam "c" (TmLam "n" (TmApp (TmApp "c" "x") (TmApp (TmApp "c" "y") "n"))))) @?= TmLam "c" (TmLam "n" (TmApp (TmApp "c" "y") "n"))
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
      evalState (alphaConv "x" (TmLam "x" "x")) 0 @?= TmLam "x'" "x'"
      evalState (alphaConv "x" (TmLam "x" (TmLam "y" "x"))) 0 @?= TmLam "x'" (TmLam "y" "x'")
      evalState (alphaConv "y" (TmLam "x" (TmLam "y" "x"))) 0 @?= TmLam "x" (TmLam "y'" "x")
  ]
