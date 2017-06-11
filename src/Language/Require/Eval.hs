module Language.Require.Eval (eval) where

import Control.Arrow ((&&&))
import Data.List (group, sort)
import Language.Require.Syntax

-- | one small step evaluation
eval1 :: Term -> IO (Maybe Term)
eval1 (TmApp (TmAbs _ _ t12) v2@(TmInt _)) = return $ Just $ termSubstTop v2 t12
eval1 (TmApp v1@TmAbs {} t2) = do
  t2' <- eval1 t2
  case t2' of
    Just t' -> return $ Just $ TmApp v1 t'
    Nothing -> return Nothing
eval1 (TmApp t1 t2) = do
  t1' <- eval1 t1
  case t1' of
    Just t' -> return $ Just $ TmApp t' t2
    Nothing -> return Nothing
eval1 (TmPlus (TmInt v1) (TmInt v2)) = return $ Just $ TmInt $ v1 + v2
eval1 (TmPlus v1@(TmInt _) t2) = do
  t2' <- eval1 t2
  case t2' of
    Just t' -> return $ Just $ TmPlus v1 t'
    Nothing -> return Nothing
eval1 (TmPlus t1 t2) = do
  t1' <- eval1 t1
  case t1' of
    Just t' -> return $ Just $ TmPlus t' t2
    Nothing -> return Nothing
eval1 (TmIf (TmInt v1) (TmInt v2) t3 t4) =
  if v1 < v2
    then return $ Just t3
    else return $ Just t4
eval1 (TmIf v1@(TmInt _) t2 t3 t4) = do
  t2' <- eval1 t2
  case t2' of
    Just t' -> return $ Just $ TmIf v1 t' t3 t4
    Nothing -> return Nothing
eval1 (TmIf t1 t2 t3 t4) = do
  t1' <- eval1 t1
  case t1' of
    Just t' -> return $ Just $ TmIf t' t2 t3 t4
    Nothing -> return Nothing
eval1 (TmDetReq n t) = do
  putStrLn $ "Fetch data from source " ++ n ++ " for " ++ show t ++ " times:"
  l <- getNitems t
  return $ Just $ TmInt $ mostOcc l
  where
    getNitems :: Int -> IO [Int]
    getNitems i
      | i > 0 = do
        s <- getLine
        case reads s of
          [(x, _)] -> do
            xs <- getNitems (i - 1)
            return $ x : xs
          _ -> getNitems i
    getNitems _ = return []
    mostOcc :: [Int] -> Int
    mostOcc = snd . maximum . map (length &&& head) . group . sort
eval1 _ = return Nothing

-- | eval until get stuck or reach a value
eval :: Term -> IO Term
eval t = do
  t' <- eval1 t
  case t' of
    Just t'' -> eval t''
    Nothing -> return t
