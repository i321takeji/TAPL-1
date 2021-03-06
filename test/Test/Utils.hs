module Test.Utils
  ( evalAllStrategy
  ) where

import           Test.Tasty.HUnit

import           Language.UntypedLambda

evalAllStrategy :: UntypedLambda -> UntypedLambda  -> IO ()
evalAllStrategy term expected = do
  eval NormalOrder term @?= expected
  eval CallByName  term @?= expected
  eval CallByValue term @?= expected
