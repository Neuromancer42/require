{
module Lexer
  ( lex
  , Token(..)
  ) where
}

%wrapper "basic"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters
$upper = A-Z
$lower = a-z

tokens :-

  $white+		        	    	;
  "--".*			        	    ;
  \n                                ;
  $digit+				            { \s -> TokenInt (read s) }
  if                                { \s -> TokenIf }
  then                              { \s -> TokenThen }
  else                              { \s -> TokenElse }
  $lower [$alpha $digit \_ \']*		{ \s -> TokenVar s }
  \+                                { \s -> TokenPlus }
  \-\>                              { \s -> TokenArrow }
  Int                               { \s -> TokenTyInt }
  \:                                { \s -> TokenColon }
  \/                                { \s -> TokenSlash }
  \\                                { \s -> TokenBackslash }
  \(                                { \s -> TokenLParen }
  \)                                { \s -> TokenRParen }
  \[                                { \s -> TokenLBrack }
  \]                                { \s -> TokenRBrack }
  \.                                { \s -> TokenDot }
  \<                                { \s -> TokenLT }
  \>                                { \s -> TokenGT }
  \|                                { \s -> TokenVert }

{
-- Each action has type :: String -> Token

-- The token type:
data Token
	= TokenVar String
	| TokenInt Integer
    | TokenIf
    | TokenThen
    | TokenElse
    | TokenArrow
    | TokenPlus
    | TokenTyInt
    | TokenColon
    | TokenSlash
    | TokenBackslash
    | TokenLParen
    | TokenRParen
    | TokenLBrack
    | TokenRBrack
    | TokenDot
    | TokenLT
    | TokenGT
    | TokenVert
	deriving (Eq,Show)

lex :: String -> [Token]
lex = alexScanTokens
}
