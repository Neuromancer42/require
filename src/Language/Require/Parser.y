{
module Language.Require.Parser
  ( parser
  ) where

import Language.Require.Lexer
import Data.Ratio
import Language.Require.Syntax
import Control.Monad.State.Lazy

}

%name parseTerm term
%tokentype { Token }
%error { parseError }

%token
    '+'     { TokenPlus }
    '/'     { TokenSlash }
    to      { TokenArrow }
    ':'     { TokenColon }
    int     { TokenInt $$  }
    var     { TokenVar $$ }
    tyInt   { TokenTyInt }
    lam     { TokenBackslash }
    '('     { TokenLParen }
    ')'     { TokenRParen }
    '.'     { TokenDot }
    '<'     { TokenLT }
    if      { TokenIf }
    then    { TokenThen }
    else    { TokenElse }
    '['     { TokenLBrack }
    ']'     { TokenRBrack }
    '|'     { TokenVert }

%left '+'

%right to

%%

term :: { M Term }
    : appterm                   { $1 }
    | lam var ':' type '.' term { (\t12 ->
                                    state $ \(ctx, reql) ->
                                    ( TmAbs $2 $4 t12
                                    , (addname ctx $2, reql)
                                    )
                                  ) =<< $6
                                }
    | if term '<' term then term else term
                                { do {
                                    t1 <- $2; t2 <- $4;
                                    t3 <- $6; t4 <- $8;
                                    return $ TmIf t1 t2 t3 t4 }
                                }

appterm :: { M Term }
    : aterm                     { $1 }
    | appterm aterm             { do {
                                    t1 <- $1; t2 <- $2;
                                    return $ TmApp t1 t2
                                  }
                                }

aterm :: { M Term }
    : '(' term '|' rat ')'      { $2 >>= \t2 -> return $ TmAss t2 $4 }
    | '(' term ')'              { $2 }
    | '(' term '+' term ')'     { do {
                                    t1 <- $2; t2 <- $4;
                                    return $ TmPlus t1 t2
                                    }
                                }
    | '[' var rat ']'           { state $ \(ctx, reql) ->
                                    ( TmReq (length reql) $3
                                    , (ctx, addreq reql $2 $3)
                                    )
                                }
    | int                       { return $ TmInt $1 }
    | var                       { state $ \(ctx, reql) ->
                                    ( TmVar (name2index ctx $1)
                                            (ctxlength ctx)
                                    , (ctx, reql)
                                    )
                                }

type :: { Ty }
    : type to type              { TyArr $1 $3}
    | '(' type ')'              { $2 }
    | tyInt                     { TyInt }

rat :: { Rational }
    : int '/' int               { fromIntegral $1 % fromIntegral $3 }

{

type M = State (Context, ReqList)

parseError :: [Token] -> a
parseError _ = error "Parse error"

parser :: String -> (Term, (Context, ReqList))
parser = flip runState (emptycontext, emptyreqlist). parseTerm . lexer
}
