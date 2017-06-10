module Language.Require.Syntax where

-- * Definitions for abstract syntax
import Data.Ratio ()
import Data.Set as S

-- ** Data typs fot syntax
-- | terms are either literal numbers or arrows,
-- based on simply typed lambda calculus
data Term
  = TmVar Int
          Int
  | TmAbs String
          Ty
          Term
  | TmApp Term
          Term
  | TmReq Int
          Rational
  | TmInt Int
  | TmPlus Term
           Term
  | TmAss Term
          Rational
  | TmIf Term
         Term
         Term
         Term
  | TmSet (S.Set Int) -- ^ only used in analysis
  | TmDetReq String
             Int -- ^ only used in evaluation
  deriving (Eq, Show)

-- | types are based on STLC, only Integers considered for now
data Ty
  = TyInt
  | TyArr Ty
          Ty
  deriving (Eq, Show)

-- ** Context management
data Binding
  = NameBind
  | VarBind Ty
  deriving (Eq, Show)

type Context = [(String, Binding)]

type ReqList = [(String, Rational)]

addreq :: ReqList -> String -> Rational -> ReqList
addreq reql n r = (n, r) : reql

emptycontext :: Context
emptycontext = []

ctxlength :: Context -> Int
ctxlength = length

addbinding :: Context -> String -> Binding -> Context
addbinding ctx s bind = (s, bind) : ctx

addname :: Context -> String -> Context
addname ctx s = addbinding ctx s NameBind

isnamebound :: Context -> String -> Bool
isnamebound [] _ = False
isnamebound ((y, _):ctxs) x = x == y || isnamebound ctxs x

pickfreshname :: Context -> String -> String
pickfreshname ctx x =
  if isnamebound ctx x
    then pickfreshname ctx (x ++ "'")
    else x

index2name :: Context -> Int -> String
index2name ctx i = fst $ ctx !! i

name2index :: Context -> String -> Int
name2index [] x = error $ "Identifier " ++ x ++ " is unbound."
name2index ((y, _):ctxs) x =
  if x == y
    then 0
    else 1 + name2index ctxs x

getbinding :: Context -> Int -> Binding
getbinding ctx i = snd $ ctx !! i

getTypeFromContext :: Context -> Int -> Ty
getTypeFromContext ctx i =
  case getbinding ctx i of
    VarBind tyT -> tyT
    _ -> error "Error: Wrong kind of binding for variable"

-- ** Shifting
tmmap :: (Int -> Int -> Int -> Term) -> Int -> Term -> Term
tmmap onvar = walk
  where
    walk :: Int -> Term -> Term
    walk c (TmVar x n) = onvar c x n
    walk c (TmAbs x tyT1 t2) = TmAbs x tyT1 (walk (c + 1) t2)
    walk c (TmApp t1 t2) = TmApp (walk c t1) (walk c t2)
    walk c (TmPlus t1 t2) = TmPlus (walk c t1) (walk c t2)
    walk c (TmIf t1 t2 t3 t4) = TmIf (walk c t1) (walk c t2) (walk c t3) (walk c t4)
    walk c (TmAss t r) = TmAss (walk c t) r
    walk _ t = t

termShiftAbove :: Int -> Int -> Term -> Term
termShiftAbove d =
  tmmap
    (\c x n ->
       if x > c
         then TmVar (x + d) (n + d)
         else TmVar x (n + d))

termShift :: Int -> Term -> Term
termShift d = termShiftAbove d 0

-- ** Substitution
-- | substitute a bounded variable with value
termSubst :: Int -> Term -> Term -> Term
termSubst j0 s =
  tmmap
    (\j x n ->
       if x == j
         then termShift j s
         else TmVar x n)
    j0

termSubstTop :: Term -> Term -> Term
termSubstTop s t = termShift (-1) (termSubst 0 (termShift 1 s) t)
