module Language.UntypedLambda.Types
  ( Term (..)
  , Strategy (..)
  ) where

import           Data.String
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Text.Prettyprint.Doc

data Term
  = TmVar Text
  | TmLam Text Term
  | TmApp Term Term
  deriving Show

instance Eq Term where
  (TmVar _) == (TmVar _) = True
  (TmLam _ t1) == (TmLam _ t2) = t1 == t2
  (TmApp t1 t2) == (TmApp t1' t2') = t1 == t1' && t2 == t2'
  _ == _ = False

instance Pretty Term where
  pretty (TmVar x)     = pretty x
  pretty (TmLam x t)   = pretty "λ" <> pretty x <> pretty "." <+> pretty t
  pretty (TmApp t1 t2) = ppr t1 <+> ppr t2
    where
      ppr t@(TmVar _) = pretty t
      ppr t           = parens (pretty t)

instance IsString Term where
  fromString = TmVar . T.pack

data Strategy
  = FullBetaReduction -- ^ 完全ベータ簡約
  | NormalOrder       -- ^ 正規順序戦略
  | CallByName        -- ^ 名前呼び戦略
  | CallByValue       -- ^ 値呼び戦略
  deriving (Show, Read, Enum, Bounded)
