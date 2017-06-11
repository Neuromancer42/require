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
%monad { State (Context, ReqList) }

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

term :: { Term }
    : appterm                   { return $1 }
    | lam var ':' type '.' term { State $ \(ctx, reql) ->
                                    ( TmAbs $2 $4 $6
                                    , (addname ctx $2, reql)
                                    )
                                }
    | if term '<' term then term else term
                                { return $ TmIf $2 $4 $6 $8 }

appterm :: { Term }
    : aterm                     { return $1 }
    | appterm aterm             { return $ TmApp $1 $2 }

aterm :: { Term }
    : '(' term '|' rat ')'      { return $ TmAss $2 $4 }
    | '(' term ')'              { return $2 }
    | '(' term '+' term ')'     { return $ TmPlus $2 $4 }
    | '[' var rat ']'           { State $ \(ctx, reql) ->
                                    ( TmReq (length reql) $3
                                    , (ctx, addreq reql $2 $3)
                                    )
                                }
    | int                       { return $ TmInt $1 }
    | var                       { State $ \(ctx, reql) ->
                                    ( TmAbs (name2index ctx $1)
                                            (ctxlength ctx)
                                    , (ctx, reql)
                                    )
                                }

type :: { Ty }
    : type to type              { return $ TyArr $1 $3}
    | '(' type ')'              { return $2 }
    | tyInt                     { return TyInt }

rat :: { Rational }
    : int '/' int               { return $ $1 % $3 }

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

parser :: String -> (Term, (Context, ReqList))
parser = runState parseTerm . lexer
}
