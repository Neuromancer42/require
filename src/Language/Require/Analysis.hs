module Language.Require.Analysis
  ( analyzeTerm
  ) where

import Control.Applicative
import Control.Arrow (second)
import Control.Monad.State.Lazy
import qualified Data.Map.Lazy as M
import Data.Ratio ()
import qualified Data.Set as S
import Language.Require.Syntax

-- * Abstract execution to analyze the Dataflow Dependence
type Constraint = [(Rational, S.Set Int)]

isAbsVal :: Term -> Bool
isAbsVal (TmSet _) = True
isAbsVal TmAbs {} = True
isAbsVal _ = False

-- | onestep analysis
analyze1 :: Term -> [(Constraint, Term)]
analyze1 (TmApp (TmAbs _ _ t12) v2)
  | isAbsVal v2 = [([], termSubstTop v2 t12)]
analyze1 (TmApp v1 t2)
  | isAbsVal v1 = do
    (c, t2') <- analyze1 t2
    return (c, TmApp v1 t2')
analyze1 (TmApp t1 t2) = do
  (c, t1') <- analyze1 t1
  return (c, TmApp t1' t2)
analyze1 (TmIf t1 t2 t3 t4) = map ((,) []) [t1, t2, t3, t4]
analyze1 (TmInt _) = [([], TmSet S.empty)]
analyze1 (TmReq i _) = [([], TmSet (S.singleton i))]
analyze1 (TmPlus (TmSet s1) (TmSet s2)) = [([], TmSet (S.union s1 s2))]
analyze1 (TmPlus v1 t2)
  | isAbsVal v1 = do
    (c, t2') <- analyze1 t2
    return (c, TmPlus v1 t2')
analyze1 (TmPlus t1 t2) = do
  (c, t1') <- analyze1 t1
  return (c, TmPlus t1' t2)
analyze1 (TmAss v@(TmSet s) r) = [([(r, s)], v)]
analyze1 (TmAss t r) = do
  (c, t') <- analyze1 t
  return (c, TmAss t' r)
analyze1 _ = []

-- | analyze the whole term
analyze :: StateT Term [] [Constraint]
analyze = many $ StateT analyze1

-- | fetch the result constraint
getConstraints :: Term -> ReqList -> Equations
getConstraints t reql =
  let constraints = concat $ concat $ evalStateT analyze t
  in map (second (M.fromSet (\i -> snd $ reql !! i))) constraints

type Equations = [(Rational, M.Map Int Rational)]

-- | solve a single equation
solve1 :: Rational -> M.Map Int Rational -> M.Map Int Int
solve1 a m =
  let s = M.size m
  in M.map (getRep a s) m
  where
    getRep :: Rational -> Int -> Rational -> Int
    getRep r s p =
      let w = fromRational $ 4 * p * (1 - p)
          t = log $ fromRational r
      in getRepIter s t w 1
    getRepIter :: Int -> Double -> Double -> Int -> Int
    getRepIter s t w c =
      if fromIntegral s * log (1 - w ** (fromIntegral c / 2) / 2) >= t
        then c
        else getRepIter s t w (c + 1)

-- | solve all equations
solve :: Equations -> M.Map Int Int
solve = foldr (M.unionWith max . uncurry solve1) M.empty

-- | turn a term to a analyzed term
updateCons :: ReqList -> M.Map Int Int -> Term -> Term
updateCons r m (TmAss t _) = updateCons r m t
updateCons r m (TmReq n _) =
  case M.lookup n m of
    Just t -> TmDetReq (fst $ r !! n) t
    Nothing -> TmDetReq (fst $ r !! n) 1
updateCons r m (TmAbs x tyT1 t2) = TmAbs x tyT1 (updateCons r m t2)
updateCons r m (TmApp t1 t2) = TmApp (updateCons r m t1) (updateCons r m t2)
updateCons r m (TmPlus t1 t2) = TmPlus (updateCons r m t1) (updateCons r m t2)
updateCons r m (TmIf t1 t2 t3 t4) =
  TmIf (updateCons r m t1) (updateCons r m t2) (updateCons r m t3) (updateCons r m t4)
updateCons _ _ t = t

-- | chain through the whole analyse phase
analyzeTerm :: ReqList -> Term -> Term
analyzeTerm reql t =
  let c = getConstraints t reql
  in let r = solve c
     in updateCons reql r t
