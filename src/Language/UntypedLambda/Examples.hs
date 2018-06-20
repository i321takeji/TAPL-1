{-# LANGUAGE OverloadedStrings #-}
module Language.UntypedLambda.Examples where

import           Prelude                        hiding (and, fst, id)

import           Language.UntypedLambda
import           Language.UntypedLambda.Prelude

-- | s t u
example1 :: UntypedLambdaTerm
example1 = TmApp (TmApp "s" "t") "u"

-- | λx. (λy. ((x y) x))
example2 :: UntypedLambdaTerm
example2 = TmLam "x" (TmLam "y" (TmApp (TmApp "x" "y") "x"))

-- | (λx.x) ((λx.x) (λz. (λx.x) z))
example3 :: UntypedLambdaTerm
example3 = TmApp id (TmApp id (TmLam "z" (TmApp id "z")))

-- | (λx.x) x
example4 :: UntypedLambdaTerm
example4 = TmApp (TmLam "x" "x") "x"

-- | λz. λx. λy. x (y z)
example5 :: UntypedLambdaTerm
example5 = TmLam "z" (TmLam "x" (TmLam "y" (TmApp "x" (TmApp "y" "z"))))

-- | (λx. x (λx. x)) (u r)
example6 :: UntypedLambdaTerm
example6 = TmApp (TmLam "x" (TmApp "x" (TmLam "x" "x"))) (TmApp "u" "r")

-- | test tru tru fls
example7 :: UntypedLambdaTerm
example7 = TmApp (TmApp (TmApp test tru) tru) fls

-- | and tru tru
example8 :: UntypedLambdaTerm
example8 = TmApp (TmApp and tru) tru

-- | and tru fls
example9 :: UntypedLambdaTerm
example9 = TmApp (TmApp and tru) fls

-- | fst (pair v w)
example10 :: UntypedLambdaTerm
example10 = TmApp fst (TmApp (TmApp pair "v") "w")

-- | [x]
example11 :: UntypedLambdaTerm
example11 = TmLam "c" (TmLam "n" (TmApp (TmApp "c" "x") "n"))
