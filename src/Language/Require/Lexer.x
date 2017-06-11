{
module Language.Require.Lexer
  ( lexer
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
  if                                { \_ -> TokenIf }
  then                              { \_ -> TokenThen }
  else                              { \_ -> TokenElse }
  $lower [$alpha $digit \_ \']*		{ \s -> TokenVar s }
  \+                                { \_ -> TokenPlus }
  \-\>                              { \_ -> TokenArrow }
  Int                               { \_ -> TokenTyInt }
  \:                                { \_ -> TokenColon }
  \/                                { \_ -> TokenSlash }
  \\                                { \_ -> TokenBackslash }
  \(                                { \_ -> TokenLParen }
  \)                                { \_ -> TokenRParen }
  \[                                { \_ -> TokenLBrack }
  \]                                { \_ -> TokenRBrack }
  \.                                { \_ -> TokenDot }
  \<                                { \_ -> TokenLT }
  \>                                { \_ -> TokenGT }
  \|                                { \_ -> TokenVert }

{
-- Each action has type :: String -> Token

-- The token type:
data Token
	= TokenVar String
	| TokenInt Int
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

lexer :: String -> [Token]
lexer = alexScanTokens
}
