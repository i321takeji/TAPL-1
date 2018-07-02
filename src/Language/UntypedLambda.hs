module Language.UntypedLambda
  ( module Language.UntypedLambda.Types
  , module Language.UntypedLambda.Parser
  , isClosed
  , reduceNormalOrder
  , reduceCallByName
  , reduceCallByValue
  , eval
  , evalWithTrace
  , evalOneStep
  , trace
  , steps
  , subst
  ) where

import           Language.UntypedLambda.Parser
import           Language.UntypedLambda.Types
import           Language.Utils

import           Data.Set                      (Set)
import qualified Data.Set                      as Set
import           Data.Text                     (Text)

-- | 指定された評価戦略で項を正規系に評価する
eval :: Strategy -> UntypedLambda -> UntypedLambda
eval s t
  | result == t = t
  | otherwise = eval s result
  where
    result = evalOneStep s t

-- | デバッグ用
trace :: Strategy -> UntypedLambda -> IO ()
trace s t = mapM_ (putStrLn . render) $ reverse $ evalWithTrace s [t] t

-- | 簡約ステップ列を返す
evalWithTrace :: Strategy -> [UntypedLambda] -> UntypedLambda -> [UntypedLambda]
evalWithTrace s acc t
  | result == t = acc
  | otherwise = evalWithTrace s acc' result
  where
    result = evalOneStep s t
    acc'   = result:acc

-- | 簡約ステップ数を返す
steps :: UntypedLambda -> Int
steps = length . evalWithTrace NormalOrder []

-- | 1ステップのみ、指定された評価戦略で評価する
evalOneStep :: Strategy -> UntypedLambda -> UntypedLambda
evalOneStep FullBetaReduction _ = undefined -- TODO
evalOneStep NormalOrder       t = reduceNormalOrder t
evalOneStep CallByName        t = reduceCallByName t
evalOneStep CallByValue       t = reduceCallByValue t

-- | 正規順序戦略
reduceNormalOrder :: UntypedLambda -> UntypedLambda
reduceNormalOrder (TmApp (TmLam x old) new)             = subst x new old
reduceNormalOrder (TmApp t1@(TmApp _ _) t2@(TmApp _ _)) = TmApp (reduceNormalOrder t1) (reduceNormalOrder t2)
reduceNormalOrder (TmApp t1@(TmApp _ _) t2)             = TmApp (reduceNormalOrder t1) t2
reduceNormalOrder (TmApp t1 t2@(TmApp _ _))             = TmApp t1 (reduceNormalOrder t2)
reduceNormalOrder (TmLam v t)                           = TmLam v (reduceNormalOrder t)
reduceNormalOrder t                                     = t

-- | 名前呼び戦略
reduceCallByName :: UntypedLambda -> UntypedLambda
reduceCallByName (TmApp (TmLam x old) new)             = subst x new old
reduceCallByName (TmApp t1@(TmApp _ _) t2@(TmApp _ _)) = TmApp (reduceCallByName t1) (reduceCallByName t2)
reduceCallByName (TmApp t1@(TmApp _ _) t2)             = TmApp (reduceCallByName t1) t2
reduceCallByName (TmApp t1 t2@(TmApp _ _))             = TmApp t1 (reduceCallByName t2)
reduceCallByName t                                     = t

-- | 値呼び戦略
reduceCallByValue :: UntypedLambda -> UntypedLambda
reduceCallByValue (TmApp t@(TmLam x old) new)
  | isValue new = subst x new old
  | otherwise   = TmApp t (reduceCallByValue new)
reduceCallByValue (TmApp t1@(TmApp _ _) t2@(TmApp _ _)) = TmApp (reduceCallByValue t1) (reduceCallByValue t2)
reduceCallByValue (TmApp t1@(TmApp _ _) t2) = TmApp (reduceCallByValue t1) t2
reduceCallByValue (TmApp t1 t2@(TmApp _ _)) = TmApp t1 (reduceCallByValue t2)
reduceCallByValue t = t

-- | β-reduction
subst :: Text -> UntypedLambda -> UntypedLambda -> UntypedLambda
subst v1 after t@(TmVar v2)
  | v1 == v2  = after
  | otherwise = t
subst v1 after t@(TmLam v2 t')
  | v1 == v2  = t
  | v1 /= v2 && v2 `Set.notMember` freeVars Set.empty after = TmLam v2 (subst v1 after t')
  | otherwise = t -- TODO
subst v after (TmApp t1 t2) = TmApp t1' t2'
  where
    t1' = subst v after t1
    t2' = subst v after t2

-- | 与えられた項が閉じているかどうか判定する述語
isClosed :: UntypedLambda -> Bool
isClosed = Set.null . freeVars Set.empty

-- | 項に含まれる自由変数を返す
freeVars :: Set Text -> UntypedLambda -> Set Text
freeVars fv (TmVar v)
  | Set.member v fv = Set.empty
  | otherwise = Set.singleton v
freeVars fv (TmLam v t) = freeVars fv t `Set.difference` Set.singleton v
freeVars fv (TmApp t1 t2) = fv1 `Set.union` fv2
  where
    fv1 = freeVars fv t1
    fv2 = freeVars fv t2

-- | 与えられた項が値かどうか判定する述語
isValue :: UntypedLambda -> Bool
isValue (TmVar _)   = True
isValue (TmLam _ _) = True
isValue _           = False
