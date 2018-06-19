{-# LANGUAGE OverloadedStrings #-}
module Language.UntypedLambda.Prelude
  ( id
  -- Church ブール値
  , tru
  , fls
  , test
  , and
  , or  -- ^ 演習 5.2.1
  , not -- ^ 演習 5.2.1
  -- 二つ組
  , pair
  , fst
  , snd
  ) where

import           Prelude                hiding (and, id, not, or, fst, snd)

import           Language.UntypedLambda.Types

-- | λx. x
id :: Term
id = TmLam "x" "x"

-- | λt. λf. t
tru :: Term
tru = TmLam "t" (TmLam "f" "t")

-- | λt. λf. f
fls :: Term
fls = TmLam "t" (TmLam "f" "f")

-- | λl. λm. λn. l m n
test :: Term
test = TmLam "l" (TmLam "m" (TmLam "n" (TmApp (TmApp "l" "m") "n")))

-- | λb. λc. b c fls
and :: Term
and = TmLam "b" (TmLam "c" (TmApp (TmApp "b" "c") fls))

-- | λb. λc. b tru c
or :: Term
or = TmLam "b" (TmLam "c" (TmApp (TmApp "b" tru) "c"))

-- | λb. b fls tru
not :: Term
not = TmLam "b" (TmApp (TmApp "b" fls) tru)

-- | λf. λs. λb. b f s
pair :: Term
pair = TmLam "f" (TmLam "s" (TmLam "b" (TmApp (TmApp "b" "f") "s")))

-- | λp. p tru
fst :: Term
fst = TmLam "p" (TmApp "p" tru)

-- | λp. p fls
snd :: Term
snd = TmLam "p" (TmApp "p" fls)