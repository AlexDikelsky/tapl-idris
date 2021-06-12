module ArithParser

import Text.Parser.Core
import Text.Parser
import Text.Lexer
import Data.List
import Data.Either

import Datatypes

export
Show (ParseError token) where 
   show (Error s l) = s 

exprTokens : TokenMap ExpressionToken
exprTokens =
  [(exact "true",  \x => Boolean True),
   (exact "false", \x => Boolean False),
   (exact "if",    \x => IfToken),
   (exact "then",  \x => ThenToken),
   (exact "else",  \x => ElseToken),
   (is '(', \x => LParen),
   (is ')', \x => RParen),
   (is ' ', \x => Whitespace),
   (is '0', \x => ZeroToken),
   (exact "succ",   \x => IntFuncToken SuccToken),
   (exact "pred",   \x => IntFuncToken PredToken),
   (exact "iszero", \x => IntFuncToken IsZeroToken)
   ]

Rule : Type -> Type
Rule ty = Grammar (TokenData ExpressionToken) True ty

boolLiteral : Rule AST
boolLiteral = 
  terminal 
    "Failed on Bool parsing" 
    (\x => case tok x of
      Boolean True =>  Just Truth
      Boolean False => Just Falsehood
      _ => Nothing
      )

tokenEquals : ExpressionToken -> Rule ()
tokenEquals s = terminal ("Expected " ++ (show s))
                 (\x => case s == (tok x) of 
                    True  => Just ()
                    False => Nothing)

tokenIsFunc : ExpressionToken -> Bool
tokenIsFunc token = case token of
                      IntFuncToken _ => True
                      _ => False

wh : Rule ()
wh = tokenEquals Whitespace

mutual
  expr : Rule AST
  expr = boolLiteral 
     <|> ifToAst 
     <|> parens 
     <|> succExpr 
     <|> predExpr
     <|> iszeroExpr
     <|> zeroExpr

  ifToAst : Rule AST
  ifToAst = do
    tokenEquals IfToken
    wh
    test <- expr
    wh
    tokenEquals ThenToken
    wh
    was_true <- expr
    wh
    tokenEquals ElseToken
    wh
    was_false <- expr
    pure (If test was_true was_false)
  
  parens : Rule AST
  parens = do
    tokenEquals LParen
    e <- expr
    tokenEquals RParen
    pure e
  
  succExpr : Rule AST
  succExpr = do
    tokenEquals (IntFuncToken SuccToken)
    wh
    v <- expr
    pure (Succ v)

  predExpr : Rule AST
  predExpr = do
    tokenEquals (IntFuncToken PredToken)
    wh
    v <- expr
    pure (Pred v)

  iszeroExpr : Rule AST
  iszeroExpr = do
    tokenEquals (IntFuncToken IsZeroToken)
    wh
    v <- expr
    pure (IsZero v)

  zeroExpr : Rule AST
  zeroExpr = do
    tokenEquals ZeroToken
    pure Zero
  
export
runParser : String -> Either (ParseError (TokenData ExpressionToken))
                (AST, List (TokenData ExpressionToken))
runParser s = parse expr (fst (lex exprTokens s))
