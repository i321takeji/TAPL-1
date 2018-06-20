{-# LANGUAGE OverloadedStrings #-}
module Language.UntypedLambda
  ( module Language.UntypedLambda.Types
  , module Language.UntypedLambda.Parser
  , isClosed
  , reduceNormalOrder
  , reduceCallByName
  , reduceCallByValue
  , runEval
  , eval
  , evalWithTrace
  , evalOneStep
  , trace
  , traceN
  , steps
  , bindVars
  , alphaConv
  , runEvalN
  ) where

import           Language.UntypedLambda.Parser
import           Language.UntypedLambda.Types
import           Language.Utils

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State.Lazy
import           Data.Set                       (Set)
import qualified Data.Set                       as Set
import           Data.Text                      (Text)
import qualified Data.Text                      as T

runEval :: Strategy -> UntypedLambdaTerm -> UntypedLambdaTerm
runEval s t = evalState (eval s t) 0

runEvalN :: Strategy -> UntypedLambdaTerm -> Int ->IO ()
runEvalN s t n = evalStateT (traceN s t n) 0

-- | 指定された評価戦略で項を正規系に評価する
eval :: Strategy -> UntypedLambdaTerm -> State Int UntypedLambdaTerm
eval s t = do
  result <- evalOneStep s t
  if result == t
  then return t
  else eval s result

-- | デバッグ用
trace :: Strategy -> UntypedLambdaTerm -> IO ()
trace s t = mapM_ (putStrLn . render) $ reverse $ evalState (evalWithTrace s [t] t) 0

-- | デバッグ用
traceN :: Strategy -> UntypedLambdaTerm -> Int -> StateT Int IO ()
traceN _ _ 0 = return ()
traceN s t n = do
  cnt <- get
  result <- return $ evalState (evalOneStep s t) cnt
  liftIO $ putStrLn $ render result
  traceN s result (n-1)

-- | 簡約ステップ列を返す
evalWithTrace :: Strategy -> [UntypedLambdaTerm] -> UntypedLambdaTerm -> State Int [UntypedLambdaTerm]
evalWithTrace s acc t = do
  result <- evalOneStep s t
  if result == t
  then return acc
  else evalWithTrace s (result:acc) result

-- | 簡約ステップ数を返す
steps :: UntypedLambdaTerm -> Int
steps term = length $ evalState (evalWithTrace NormalOrder [] term) 0

-- | 1ステップのみ、指定された評価戦略で評価する
evalOneStep :: Strategy -> UntypedLambdaTerm -> State Int UntypedLambdaTerm
evalOneStep FullBetaReduction = undefined
evalOneStep NormalOrder       = reduceNormalOrder
evalOneStep CallByName        = reduceCallByName
evalOneStep CallByValue       = reduceCallByValue

-- | 正規順序戦略
reduceNormalOrder :: UntypedLambdaTerm -> State Int UntypedLambdaTerm
reduceNormalOrder (TmApp (TmLam x old) new) = varCheck x new old
reduceNormalOrder (TmApp t1@(TmApp _ _) t2@(TmApp _ _)) = mkApp <$> reduceNormalOrder t1 <*> reduceNormalOrder t2
reduceNormalOrder (TmApp t1@(TmApp _ _) t2) = mkApp <$> reduceNormalOrder t1 <*> pure t2
reduceNormalOrder (TmApp t1 t2@(TmApp _ _)) = mkApp <$> pure t1 <*> reduceNormalOrder t2
reduceNormalOrder (TmLam v t) = mkLam <$> pure v <*> reduceNormalOrder t
reduceNormalOrder t = pure t

-- | 名前呼び戦略
reduceCallByName :: UntypedLambdaTerm -> State Int UntypedLambdaTerm
reduceCallByName (TmApp (TmLam x old) new)             = varCheck x new old
reduceCallByName (TmApp t1@(TmApp _ _) t2@(TmApp _ _)) = mkApp <$> reduceCallByName t1 <*> reduceCallByName t2
reduceCallByName (TmApp t1@(TmApp _ _) t2)             = mkApp <$> reduceCallByName t1 <*> pure t2
reduceCallByName (TmApp t1 t2@(TmApp _ _))             = mkApp <$> pure t1 <*> reduceCallByName t2
reduceCallByName t                                     = pure t

-- | 値呼び戦略
reduceCallByValue :: UntypedLambdaTerm -> State Int UntypedLambdaTerm
reduceCallByValue (TmApp t@(TmLam x old) new)
  | isValue new = varCheck x new old
  | otherwise   = mkApp <$> pure t <*> reduceCallByValue new
reduceCallByValue (TmApp t1@(TmApp _ _) t2@(TmApp _ _)) = mkApp <$> reduceCallByValue t1 <*> reduceCallByValue t2
reduceCallByValue (TmApp t1@(TmApp _ _) t2) = mkApp <$> reduceCallByValue t1 <*> pure t2
reduceCallByValue (TmApp t1 t2@(TmApp _ _)) = mkApp <$> pure t1 <*> reduceCallByValue t2
reduceCallByValue t = pure t

-- | 束縛変数の重複チェック
varCheck :: Text -> UntypedLambdaTerm -> UntypedLambdaTerm -> State Int UntypedLambdaTerm
varCheck v new old
  | Set.null dup = return $ subst v new old
  | otherwise = do
    new' <- foldM (flip alphaConv) new $ Set.toList dup
    return $ subst v new' old
  where
    dup = vs1 `Set.intersection` vs2
    vs1 = bindVars Set.empty new
    vs2 = bindVars Set.empty old

-- | α-conv
alphaConv :: Text -> UntypedLambdaTerm -> State Int UntypedLambdaTerm
alphaConv v1 term@(TmVar v2) =
  if v1 == v2
  then do
    cnt <- get
    let term' = mkVar $ mkFreshVar v2 cnt
    modify (+1)
    return term'
  else return term
alphaConv v1 (TmLam v2 subTerm) = do
  r <- alphaConv v1 subTerm
  if v1 == v2
  then do
    cnt <- get
    let term = mkLam (mkFreshVar v2 cnt) r
    modify (+1)
    return term
  else return $ mkLam v2 r
alphaConv v (TmApp t1 t2) = mkApp <$> alphaConv v t1 <*> alphaConv v t2

mkFreshVar :: Text -> Int -> Text
mkFreshVar v i = mconcat [v, T.pack $ show i]

-- | β-reduction
subst :: Text -> UntypedLambdaTerm -> UntypedLambdaTerm -> UntypedLambdaTerm
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
isClosed :: UntypedLambdaTerm -> Bool
isClosed = Set.null . freeVars Set.empty

-- | 項に含まれる自由変数を返す
freeVars :: Set Text -> UntypedLambdaTerm -> Set Text
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

-- | 項に含まれる束縛変数を返す
bindVars :: Set Text -> UntypedLambdaTerm -> Set Text
bindVars vs (TmVar _) = vs
bindVars vs (TmLam v t) = bindVars (Set.insert v vs) t
bindVars vs (TmApp t1 t2) = vs1 `Set.union` vs2
  where
    vs1 = bindVars vs t1
    vs2 = bindVars vs t2

-- | 与えられた項が値かどうか判定する述語
isValue :: UntypedLambdaTerm -> Bool
isValue (TmVar _)   = True
isValue (TmLam _ _) = True
isValue _           = False
