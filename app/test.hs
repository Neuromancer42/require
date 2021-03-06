module Main where

import Language.Require.Analysis (analyzeTerm)
import Language.Require.Eval (eval)
import Language.Require.Parser (parser)
import Language.Require.Syntax (Term(..), Ty (..))
import Language.Require.TypeCheck (typeof)
import System.Environment
import System.Exit
import System.IO

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fileName] -> do
      inp <- readFile fileName
      let (t, (ctx, reql)) = parser inp
      if typeof ctx t /= TyInt
        then hPutStrLn stderr "Error: The whole program should be integer type" >> exitFailure
        else let t' = analyzeTerm reql t
             in do t'' <- eval t'
                   case t'' of
                     TmInt tt -> putStrLn $ "Result: " ++ show tt
                     _ -> hPutStrLn stderr "Runtime Error: the reduction is incorrect."
    _ -> hPutStrLn stderr "Usage: testRequire <fileName>" >> exitFailure
