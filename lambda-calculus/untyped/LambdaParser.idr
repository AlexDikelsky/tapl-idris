module LambdaParser

import Text.Parser.Core
import Text.Parser
import Text.Lexer

import Data.List
import Data.Either

import Datatypes

export
Show (ParseError token) where 
   show (Error s l) = s 

valid : String
valid = "abcdefghijklmnopqrstuvwxyz"

exprTokens : TokenMap ExpressionToken
exprTokens =
  [(is 'λ', \x => LambdaToken),
   (is '.', \x => PeriodToken),
   (oneOf valid, \x => NameToken x),
   (is '(', \x => LParen),
   (is ')', \x => RParen),
   (is ' ', \x => Whitespace)
   ]

Rule : Type -> Type
Rule ty = Grammar (TokenData ExpressionToken) True ty

tokenEquals : ExpressionToken -> Rule ()
tokenEquals s = terminal ("Expected " ++ (show s))
                 (\x => case s == (tok x) of 
                    True  => Just ()
                    False => Nothing)

nameLiteral : Rule String
nameLiteral = terminal "Expected name"
    (\x => case tok x of
           NameToken n => Just n
           _ => Nothing)

wh : Rule ()
wh = tokenEquals Whitespace

mutual
  expr : Rule (AST String)
  expr = parens <|> absExpr <|> appExpr <|> term

  term : Rule (AST String)
  term = do
    name <- nameLiteral
    pure (Term name)

  absExpr : Rule (AST String)
  absExpr = do
    tokenEquals LambdaToken
    name <- nameLiteral
    tokenEquals PeriodToken
    body <- expr
    pure (Abs name body)
  
  appExpr : Rule (AST String)
  appExpr = do
    tokenEquals LParen
    n1 <- expr
    wh
    n2 <- expr
    tokenEquals RParen
    pure (App n1 n2)

  parens : Rule (AST String)
  parens = do
    tokenEquals LParen
    e <- expr
    tokenEquals RParen
    pure e
  
export
runParser : String -> Either (ParseError (TokenData ExpressionToken))
                (AST String, List (TokenData ExpressionToken))
runParser s = parse expr (fst (lex exprTokens s))
