module Language.UntypedLambda.Types
  ( Term (..)
  , Strategy (..)
  , UntypedLambdaTerm
  , mkVar
  , mkLam
  , mkApp
  ) where

import           Data.String
import           Data.Text                 (Text)
import           Data.Text.Prettyprint.Doc

type UntypedLambdaTerm = Term Text

mkVar :: Text -> UntypedLambdaTerm
mkVar = TmVar

mkLam :: Text -> UntypedLambdaTerm -> UntypedLambdaTerm
mkLam = TmLam

mkApp :: UntypedLambdaTerm -> UntypedLambdaTerm -> UntypedLambdaTerm
mkApp = TmApp

data Term a
  = TmVar a
  | TmLam a (Term a)
  | TmApp (Term a) (Term a)
  deriving Show

instance Eq (Term a) where
  (TmVar _) == (TmVar _) = True
  (TmLam _ t1) == (TmLam _ t2) = t1 == t2
  (TmApp t1 t2) == (TmApp t1' t2') = t1 == t1' && t2 == t2'
  _ == _ = False

instance Pretty a => Pretty (Term a) where
  pretty (TmVar x)     = pretty x
  pretty (TmLam x t)   = pretty "λ" <> pretty x <> pretty "." <+> pretty t
  pretty (TmApp t1 t2) = ppr t1 <+> ppr t2
    where
      ppr t@(TmVar _) = pretty t
      ppr t           = parens (pretty t)

instance IsString a => IsString (Term a) where
  fromString = TmVar . fromString

data Strategy
  = FullBetaReduction -- ^ 完全ベータ簡約
  | NormalOrder       -- ^ 正規順序戦略
  | CallByName        -- ^ 名前呼び戦略
  | CallByValue       -- ^ 値呼び戦略
  deriving (Show, Read, Enum, Bounded)
