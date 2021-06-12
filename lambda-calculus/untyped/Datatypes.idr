module Datatypes

import Generics.Derive
%language ElabReflection

public export
data AST a =
    Term a
  | Abs a (AST a)
  | App (AST a) (AST a)

%runElab derive "AST" [Generic, Eq]

public export
data ExpressionToken = 
    LambdaToken
  | PeriodToken
  | NameToken String
  | LParen
  | RParen
  | Whitespace

%runElab derive "ExpressionToken" [Generic, Eq]

export
Show ExpressionToken where
  show LambdaToken = "λ"
  show PeriodToken = "."
  show (NameToken s) = show s
  show LParen = "("
  show RParen = ")"
  show Whitespace = " "

export
(Show t) => Show (AST t) where
  show (Datatypes.Term a) = "{" ++ show a ++ "}"
  show (Abs var_name a) = "(lambda " ++ (show var_name) ++ "." ++ (show a) ++ ")"
  show (App a b) = "[" ++ (show a) ++ " " ++  (show b) ++ "]"
