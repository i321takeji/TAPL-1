{-# LANGUAGE OverloadedStrings #-}
module Language.UntypedLambda.Prelude
  ( id
  , prelude
  -- * Church ブール値
  , tru
  , fls
  , test
  , and
  -- ** 演習 5.2.1
  , or
  , not
  -- * 二つ組
  , pair
  , fst
  , snd
  -- * Church 数
  , c
  , scc
  , plus
  , times
  , iszro
  , prd
  -- ** 演習5.2.2
  , scc2
  -- ** 演習5.2.3
  , times2
  , times3
  -- ** 演習5.2.4
  , power1
  , power2
  -- ** 演習5.2.5
  , subtract1
  -- ** 演習5.2.7
  , equal
  -- ** 演習5.2.8
  , nil
  , cons
  , isnil
  , head
  , tail
  ) where

import           Prelude                      hiding (and, fst, head, id, not,
                                               or, snd, tail)

import           Language.UntypedLambda.Types

import           Data.Map                     (Map)
import qualified Data.Map                     as Map (fromList)
import           Data.Text                    (Text)

prelude :: Map Text UntypedLambdaTerm
prelude = Map.fromList
  [ ("id", id), ("tru", tru), ("fls", fls), ("test", test), ("and", and), ("or", or), ("not", not)
  , ("pair", pair), ("fst", fst), ("snd", snd)
  , ("scc", scc), ("plus", plus), ("times", times), ("power", power1), ("iszro", iszro), ("prd", prd), ("subtract", subtract1), ("equal", equal)
  , ("nil", nil), ("cons", cons), ("isnil", isnil), ("head", head), ("tail", tail)
  ]

-- | λx. x
id :: UntypedLambdaTerm
id = TmLam "x" "x"

-- | λt. λf. t
tru :: UntypedLambdaTerm
tru = TmLam "t" (TmLam "f" "t")

-- | λt. λf. f
fls :: UntypedLambdaTerm
fls = TmLam "t" (TmLam "f" "f")

-- | λl. λm. λn. l m n
test :: UntypedLambdaTerm
test = TmLam "l" (TmLam "m" (TmLam "n" (TmApp (TmApp "l" "m") "n")))

-- | λb. λc. b c fls
and :: UntypedLambdaTerm
and = TmLam "b" (TmLam "c" (TmApp (TmApp "b" "c") fls))

-- | λb. λc. b tru c
or :: UntypedLambdaTerm
or = TmLam "b" (TmLam "c" (TmApp (TmApp "b" tru) "c"))

-- | λb. b fls tru
not :: UntypedLambdaTerm
not = TmLam "b" (TmApp (TmApp "b" fls) tru)

-- | λf. λs. λb. b f s
pair :: UntypedLambdaTerm
pair = TmLam "f" (TmLam "s" (TmLam "b" (TmApp (TmApp "b" "f") "s")))

-- | λp. p tru
fst :: UntypedLambdaTerm
fst = TmLam "p" (TmApp "p" tru)

-- | λp. p fls
snd :: UntypedLambdaTerm
snd = TmLam "p" (TmApp "p" fls)

-- |
-- c0 = λs. λz. z
--
-- c1 = λs. λz. s z
--
-- c2 = λs. λz. s (s z)
--
-- c3 = λs. λz. s (s (s z))
c :: Int -> UntypedLambdaTerm
c n = TmLam "s" (TmLam "z" body)
  where
    body = foldr TmApp "z" $ replicate n "s"

-- | λn. λs. λz. s (n s z)
scc :: UntypedLambdaTerm
scc = TmLam "n" (TmLam "s" (TmLam "z" (TmApp "s" (TmApp (TmApp "n" "s") "z"))))

-- | λn. λs. λz. n s (s z)
scc2 :: UntypedLambdaTerm
scc2 = TmLam "n" (TmLam "s" (TmLam "z" (TmApp (TmApp "n" "s") (TmApp "s" "z"))))

-- | λm. λn. λs. λz. m s (n s z)
plus :: UntypedLambdaTerm
plus = TmLam "m" (TmLam "n" (TmLam "s" (TmLam "z" (TmApp (TmApp "m" "s") (TmApp (TmApp "n" "s") "z")))))

-- | λm. λn. m (plus n) c0
times :: UntypedLambdaTerm
times = TmLam "m" (TmLam "n" (TmApp (TmApp "m" (TmApp plus "n")) (c 0)))

-- | λm. λn. λs. λz. m (n s) z
times2 :: UntypedLambdaTerm
times2 = TmLam "m" (TmLam "n" (TmLam "s" (TmLam "z" (TmApp (TmApp "m" (TmApp "n" "s")) "z"))))

-- | λm. λn. λs. m (n s)
times3 :: UntypedLambdaTerm
times3 = TmLam "m" (TmLam "n" (TmLam "s" (TmApp "m" (TmApp "n" "s"))))

-- | λn. λm. m (times n) c1
--
-- n^m
power1 :: UntypedLambdaTerm
power1 = TmLam "n" (TmLam "m" (TmApp (TmApp "m" (TmApp times "n")) (c 1)))

-- | λn. λm. m n
--
-- n^m
power2 :: UntypedLambdaTerm
power2 = TmLam "n" (TmLam "m" (TmApp "m" "n"))

-- | λm. m (λx. fls) tru
iszro :: UntypedLambdaTerm
iszro = TmLam "m" (TmApp (TmApp "m" (TmLam "x" fls)) tru)

-- | pair c0 c0
zz :: UntypedLambdaTerm
zz = TmApp (TmApp pair (c 0)) (c 0)

-- | λp. pair (snd p) (plus c1 (snd p))
ss :: UntypedLambdaTerm
ss = TmLam "p" (TmApp (TmApp pair (TmApp snd "p")) (TmApp (TmApp plus (c 1)) (TmApp snd "p")))

-- | λm. fst (m ss zz)
prd :: UntypedLambdaTerm
prd = TmLam "m" (TmApp fst (TmApp (TmApp "m" ss) zz))

-- | λm. λn. n prd m
subtract1 :: UntypedLambdaTerm
subtract1 = TmLam "m" (TmLam "n" (TmApp (TmApp "n" prd) "m"))

-- | λm. λn. and (iszro (m prd n)) (iszro (n prd m))
equal :: UntypedLambdaTerm
equal = TmLam "m" (TmLam "n" (TmApp (TmApp and (TmApp iszro l)) (TmApp iszro r)))
  where
    l = TmApp (TmApp "m" prd) "n"
    r = TmApp (TmApp "n" prd) "m"

-- | λc. λn. n
nil :: UntypedLambdaTerm
nil = TmLam "c" (TmLam "n" "n")

-- | λh. λt. λc. λn. c h (t c n)
cons :: UntypedLambdaTerm
cons = TmLam "h" (TmLam "t" (TmLam "c" (TmLam "n" (TmApp (TmApp "c" "h") (TmApp (TmApp "t" "c") "n")))))

-- | λl. l (λh. λt. fls) tru
isnil :: UntypedLambdaTerm
isnil = TmLam "l" (TmApp (TmApp "l" (TmLam "h" (TmLam "t" fls))) tru)

-- | λl. l (λh. λt. h) l
head :: UntypedLambdaTerm
head = TmLam "l" (TmApp (TmApp "l" (TmLam "h" (TmLam "t" "h"))) "l")

-- | pair nil nil
nn :: UntypedLambdaTerm
nn = TmApp (TmApp pair nil) nil

-- | λh. λp. pair (snd p) (cons h (snd p))
cc :: UntypedLambdaTerm
cc = TmLam "h" (TmLam "p" (TmApp (TmApp pair (TmApp snd "p")) (TmApp (TmApp cons "h") (TmApp snd "p"))))

-- | λl. fst (l cc nn)
tail :: UntypedLambdaTerm
tail = TmLam "l" (TmApp fst (TmApp (TmApp "l" cc) nn))
