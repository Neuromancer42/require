module Language.Require.TypeCheck (typeof) where

import Data.Ratio
import Language.Require.Syntax

-- | Get the type of a term
typeof :: Context -> Term -> Ty
typeof _ (TmInt _) = TyInt
typeof ctx (TmIf t1 t2 t3 t4) =
  let tyT1 = typeof ctx t1
      tyT2 = typeof ctx t2
      tyT3 = typeof ctx t3
      tyT4 = typeof ctx t4
  in if tyT1 == TyInt && tyT2 == TyInt && tyT3 == tyT4
       then tyT3
       else error "Type error in IF statement"
typeof ctx (TmPlus t1 t2) =
  let tyT1 = typeof ctx t1
      tyT2 = typeof ctx t2
  in if tyT1 == TyInt && tyT2 == TyInt
       then TyInt
       else error "Type error in addtion statement"
typeof ctx (TmAbs x tyT1 t2) =
  let ctx' = addbinding ctx x (VarBind tyT1)
  in let tyT2 = typeof ctx' t2
     in TyArr tyT1 tyT2
typeof ctx (TmApp t1 t2) =
  let tyT1 = typeof ctx t1
      tyT2 = typeof ctx t2
  in (case tyT1 of
        TyArr tyT11 tyT12 ->
          if tyT11 == tyT2
            then tyT12
            else error "Type error in application"
        _ -> error "Type error in application")
typeof ctx (TmAss t _) =
  let tyT = typeof ctx t
  in if tyT == TyInt
       then TyInt
       else error "Error: Assertion can be applied only to numerical vars"
typeof _ (TmReq _ p) =
  if p > (1 % 2)
    then TyInt
    else error "Error: Promise on require's reliability should be larger than 1/2"
typeof ctx (TmVar i _) = getTypeFromContext ctx i
typeof _ _ = error "Non-reacheable"
