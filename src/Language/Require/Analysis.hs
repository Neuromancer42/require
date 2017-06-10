module Language.Require.Analysis where

import Control.Applicative
import Control.Arrow (second)
import Control.Monad.State.Lazy
import qualified Data.Map.Lazy as M
import Data.Ratio ()
import qualified Data.Set as S
import Language.Require.Syntax

-- * Abstract execution to analyze the Dataflow Dependence
type Constraint = [(Rational, S.Set Int)]

isSet :: Term -> Bool
isSet (TmSet _) = True
isSet _ = False

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

-- | TODO: solving the equation
