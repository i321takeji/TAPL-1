{-# LANGUAGE OverloadedStrings #-}
module Language.UntypedLambda.Lib.Base
  ( id
    -- * 二つ組
  , pair
  , mkPair
  , fst
  , snd
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
  -- ** 演習5.2.9
  , factorial
  , c
  , scc
  , plus
  , times
  , iszro
  , prd
  -- * fix
  , fix
  , mkFix
  ) where

import           Prelude                         hiding (and, fst, id, snd)

import           Language.UntypedLambda.Lib.Bool
import           Language.UntypedLambda.Types

import           Data.Text                       (Text)

-- | λx. x
id :: UntypedLambda
id = λ "x" "x"

-- | λf. λs. λb. b f s
pair :: UntypedLambda
pair = λ "f" $ λ "s" $ λ "b" $ "b" @@ "f" @@ "s"

-- | λf. λs. pair f s
mkPair :: UntypedLambda -> UntypedLambda -> UntypedLambda
mkPair f s = pair @@ f @@ s

-- | λp. p tru
fst :: UntypedLambda
fst = λ "p" $ "p" @@ tru

-- | λp. p fls
snd :: UntypedLambda
snd = λ "p" $ "p" @@ fls

-- |
-- c0 = λs. λz. z
--
-- c1 = λs. λz. s z
--
-- c2 = λs. λz. s (s z)
--
-- c3 = λs. λz. s (s (s z))
c :: Int -> UntypedLambda
c n = λ "s" $ λ "z" body
  where
    body = foldr (@@) "z" $ replicate n "s"

-- | λn. λs. λz. s (n s z)
scc :: UntypedLambda
scc = λ "n" $ λ "s" $ λ "z" $ "s" @@ ("n" @@ "s" @@ "z")

-- | λn. λs. λz. n s (s z)
scc2 :: UntypedLambda
scc2 = λ "n" $ λ "s" $ λ "z" $ "n" @@ "s" @@ ("s" @@ "z")

-- | λm. λn. λs. λz. m s (n s z)
plus :: UntypedLambda
plus = λ "m" $ λ "n" $ λ "s" $ λ "z" $ "m" @@ "s" @@ ("n" @@ "s" @@ "z")

-- | λm. λn. m (plus n) c0
times :: UntypedLambda
times = λ "m" $ λ "n" $ "m" @@ (plus @@ "n") @@ c 0

-- | λm. λn. λs. λz. m (n s) z
times2 :: UntypedLambda
times2 = λ "m" $ λ "n" $ λ "s" $ λ "z" $ "m" @@ ("n" @@ "s") @@ "z"

-- | λm. λn. λs. m (n s)
times3 :: UntypedLambda
times3 = λ "m" $ λ "n" $ λ "s" $ "m" @@ ("n" @@ "s")

-- | λn. λm. m (times n) c1
--
-- n^m
power1 :: UntypedLambda
power1 = λ "n" $ λ "m" $ "m" @@ (times @@ "n") @@ c 1

-- | λn. λm. m n
--
-- m^n
power2 :: UntypedLambda
power2 = λ "n" $ λ "m" $ "m" @@ "n"

-- | λm. m (λx. fls) tru
iszro :: UntypedLambda
iszro = λ "m" $ "m" @@ λ "x" fls @@ tru

-- | pair c0 c0
zz :: UntypedLambda
zz = mkPair (c 0) (c 0)

-- | λp. pair (snd p) (plus c1 (snd p))
ss :: UntypedLambda
ss = λ "p" $ mkPair (snd @@ "p") (plus @@ c 1 @@ (snd @@ "p"))

-- | λm. fst (m ss zz)
prd :: UntypedLambda
prd = λ "m" $ fst @@ ("m" @@ ss @@ zz)

-- | λm. λn. n prd m
subtract1 :: UntypedLambda
subtract1 = λ "m" $ λ "n" $ "n" @@ prd @@ "m"

-- | λm. λn. and (iszro (m prd n)) (iszro (n prd m))
equal :: UntypedLambda
equal = λ "m" $ λ "n" $ and @@ mkT "m" "n" @@ mkT "n" "m"
  where
    mkT x y = iszro @@ (x @@ prd @@ y)

-- |
--   match = iszro n
--   base = λx. c1
--   rec  = λx. times n (f (prd n))
factorial :: UntypedLambda
factorial = mkFix "n" match base rec
  where
    match = iszro @@ "n"
    base  = c 1
    rec   = times @@ "n" @@ ("f" @@ (prd @@ "n"))

-- | λf. (λx. f (λy. x x y)) (λx. f (λy. x x y))
fix :: UntypedLambda
fix = λ "f" $ t @@ t
  where
    t = λ "x" $ "f" @@ λ "y" ("x" @@ "x" @@ "y")

mkFix :: Text -> UntypedLambda -> UntypedLambda -> UntypedLambda -> UntypedLambda
mkFix v match base rec = fix @@ λ "f" (λ v $ mkTest match (λ "x" base) (λ "x" rec) @@ c 0)
