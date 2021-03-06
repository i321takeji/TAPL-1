{-# LANGUAGE OverloadedStrings #-}
module Test.Language.UntypedLambda.Lib.Church where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Language.UntypedLambda
import           Language.UntypedLambda.Lib.Bool
import           Language.UntypedLambda.Lib.Church

test_ul :: TestTree
test_ul = testGroup "UntypedLambda.Lib.Church"
  [ testCase "c" $ do
    c 0 @?= λ "s" (λ "z" "z")
    c 1 @?= λ "s" (λ "z" ("s" @@ "z"))
    c 2 @?= λ "s" (λ "z" ("s" @@ ("s" @@ "z")))
    c 3 @?= λ "s" (λ "z" ("s" @@ ("s" @@ ("s" @@ "z"))))
  , testCase "scc" $ do
    eval NormalOrder (scc @@ c 0) @?= c 1
    eval CallByName  (scc @@ c 0) @?= λ "s" (λ "z" ("s" @@ (c 0 @@ "s" @@ "z")))
    eval CallByValue (scc @@ c 0) @?= λ "s" (λ "z" ("s" @@ (c 0 @@ "s" @@ "z")))
  , testCase "scc2" $ do
    eval NormalOrder (scc @@ c 0) @?= eval NormalOrder (scc2 @@ c 0) -- TODO: 振る舞い等価
    eval NormalOrder (scc @@ c 1) @?= eval NormalOrder (scc2 @@ c 1) -- TODO: 振る舞い等価
    eval NormalOrder (scc @@ c 2) @?= eval NormalOrder (scc2 @@ c 2) -- TODO: 振る舞い等価
  , testCase "plus" $ do
    eval NormalOrder (mkPlus (c 5) (c 10)) @?= c 15 -- TODO: 振る舞い等価
    eval NormalOrder (mkPlus (c 100) (c 200)) @?= c 300 -- TODO: 振る舞い等価
  , testCase "times" $ do
    eval NormalOrder (mkTimes (c 5)  (c 10)) @?= c 50 -- TODO: 振る舞い等価
    eval NormalOrder (mkTimes (c 10) (c 20)) @?= c 200 -- TODO: 振る舞い等価
  , testCase "times2" $ do
    eval NormalOrder (mkTimes (c 5)  (c 10)) @?= eval NormalOrder (times2 @@ c 5  @@ c 10) -- TODO: 振る舞い等価
    eval NormalOrder (mkTimes (c 10) (c 20)) @?= eval NormalOrder (times2 @@ c 10 @@ c 20) -- TODO: 振る舞い等価
  , testCase "times3" $ do
    eval NormalOrder (mkTimes (c 5)  (c 10)) @?= eval NormalOrder (times3 @@ c 5  @@ c 10) -- TODO: 振る舞い等価
    eval NormalOrder (mkTimes (c 10) (c 20)) @?= eval NormalOrder (times3 @@ c 10 @@ c 20) -- TODO: 振る舞い等価
  , testCase "power1" $ do
    eval NormalOrder (power1 @@ c 2 @@ c 3) @?= c 8 -- TODO: 振る舞い等価
    eval NormalOrder (power1 @@ c 2 @@ c 0) @?= c 1 -- TODO: 振る舞い等価
  -- , testCase "power2" $ do
    -- TODO https://github.com/waddlaw/TAPL/issues/13
    -- eval NormalOrder (TmApp (TmApp power2 (c 2)) (c 3))  @?= c 9
    -- eval NormalOrder (TmApp (TmApp power2 (c 0)) (c 2))  @?= c 1
  , testCase "iszro" $ do
      eval CallByValue (iszro @@ c 0) @?= tru
      eval CallByValue (iszro @@ c 1) @?= fls
  , testCase "prd" $ do
      eval NormalOrder (prd @@ c 0) @?= c 0 -- TODO: 振る舞い等価
      eval NormalOrder (prd @@ c 1) @?= c 0 -- TODO: 振る舞い等価
      eval NormalOrder (prd @@ c 2) @?= c 1 -- TODO: 振る舞い等価
  , testCase "subtract1" $ do
      eval NormalOrder (mkSubtract (c 8) (c 2)) @?= c 6 -- TODO: 振る舞い等価
      eval CallByValue (mkSubtract (c 0) (c 2)) @?= eval CallByValue (c 0)
      eval CallByValue (mkSubtract (c 2) (c 2)) @?= eval CallByValue (c 0)
  , testCase "equal" $ do
      eval CallByValue (mkEqual (c 8) (c 2)) @?= fls
      eval CallByValue (mkEqual (c 2) (c 2)) @?= tru
  , testCase "factorial" $ do
      eval CallByValue (factorial @@ c 0) @?= eval CallByValue (c 1)
      eval NormalOrder (factorial @@ c 1) @?= c 1 -- TODO: 振る舞い等価
      eval NormalOrder (factorial @@ c 2) @?= c 2 -- TODO: 振る舞い等価
      eval NormalOrder (factorial @@ c 3) @?= c 6 -- TODO: 振る舞い等価
  , testCase "complex term" $ do
      eval CallByValue (iszro @@ mkTimes (c 0) (c 2)) @?= tru
      eval CallByValue (mkEqual (c 4) (mkTimes (c 2) (c 2))) @?= tru
      eval CallByValue (mkEqual (c 6) (factorial @@ c 3)) @?= tru
  , testCase "isone" $ do
      eval CallByValue (isone @@ c 0) @?= fls
      eval CallByValue (isone @@ c 1) @?= tru
      eval CallByValue (isone @@ c 2) @?= fls
  ]
