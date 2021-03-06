{-# LANGUAGE OverloadedStrings #-}
module Language.UntypedLambda.Prelude
  ( prelude
  ) where

import           Prelude                           hiding (and, fst, head, id,
                                                    not, or, snd, tail)

import           Language.UntypedLambda.Lib.Base
import           Language.UntypedLambda.Lib.Bool
import           Language.UntypedLambda.Lib.Church
import           Language.UntypedLambda.Lib.List
import           Language.UntypedLambda.Lib.Pair
import           Language.UntypedLambda.Types

import           Data.Map                          (Map)
import qualified Data.Map                          as Map (fromList)
import           Data.Text                         (Text)

prelude :: Map Text UntypedLambda
prelude = Map.fromList
  [ ("id", id), ("tru", tru), ("fls", fls), ("test", test), ("and", and), ("or", or), ("not", not)
  , ("pair", pair), ("fst", fst), ("snd", snd)
  , ("scc", scc), ("plus", plus), ("times", times), ("power", power1), ("iszro", iszro), ("prd", prd), ("subtract", subtract1), ("equal", equal)
  , ("nil", nil), ("cons", cons), ("isnil", isnil), ("head", head), ("tail", tail)
  ]
