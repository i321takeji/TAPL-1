module Language.UntypedLambda
  ( module Language.UntypedLambda.Types
  , module Language.UntypedLambda.Parser
  , isClosed
  , reduceNormalOrder
  , reduceCallByName
  , reduceCallByValue
  , eval
  , evalOneStep
  ) where

import           Language.UntypedLambda.Parser
import           Language.UntypedLambda.Types

import           Data.Set                      (Set)
import qualified Data.Set                      as Set
import           Data.Text                     (Text)

-- | 指定された評価戦略で項を正規系に評価する
eval :: Strategy -> Term -> Term
eval s t
  | result == t = t
  | otherwise = eval s result
  where
    result = evalOneStep s t

-- | 1ステップのみ、指定された評価戦略で評価する
evalOneStep :: Strategy -> Term -> Term
evalOneStep FullBetaReduction _ = undefined -- TODO
evalOneStep NormalOrder       t = reduceNormalOrder t
evalOneStep CallByName        t = reduceCallByName t
evalOneStep CallByValue       t = reduceCallByValue t

-- | 正規順序戦略
reduceNormalOrder :: Term -> Term
reduceNormalOrder (TmApp (TmLam x old) new) = subst x new old
reduceNormalOrder (TmLam v t)               = TmLam v (reduceNormalOrder t)
reduceNormalOrder t                         = t

-- | 名前呼び戦略
reduceCallByName :: Term -> Term
reduceCallByName (TmApp (TmLam x old) new) = subst x new old
reduceCallByName t                         = t

-- | 値呼び戦略
reduceCallByValue :: Term -> Term
reduceCallByValue (TmApp t@(TmLam x old) new)
  | isValue new = subst x new old
  | otherwise   = TmApp t (reduceCallByValue new)
reduceCallByValue t = t

-- | β-reduction
subst :: Text -> Term -> Term -> Term
subst v1 new t@(TmVar v2)
  | v1 == v2  = new
  | otherwise = t
subst v1 new t@(TmLam v2 t')
  | v1 == v2  = t
  | otherwise = TmLam v2 (subst v1 new t')
subst v new (TmApp t1 t2) = TmApp t1' t2'
  where
    t1' = subst v new t1
    t2' = subst v new t2

-- | 与えられた項が閉じているかどうか判定する述語
isClosed :: Term -> Bool
isClosed = Set.null . freeVars Set.empty

-- | 項に含まれる自由変数を返す
freeVars :: Set Text -> Term -> Set Text
freeVars fv (TmVar v)
  | Set.member v fv = Set.empty
  | otherwise = Set.singleton v
freeVars fv (TmLam v t) = freeVars fv' t
  where
    fv' = Set.insert v fv
freeVars fv (TmApp t1 t2) = fv1 `Set.union` fv2
  where
    fv1 = freeVars fv t1
    fv2 = freeVars fv t2

-- | 与えられた項が値かどうか判定する述語
isValue :: Term -> Bool
isValue (TmLam _ _) = True
isValue _           = False