module Datatypes

import Generics.Derive
%language ElabReflection

-- I think you can get around the Show implementation with SOP as well

public export
data IntFuncInnerToken : Type where
  SuccToken   : IntFuncInnerToken
  PredToken   : IntFuncInnerToken
  IsZeroToken : IntFuncInnerToken

export
Show IntFuncInnerToken where
  show SuccToken = "SuccToken"
  show PredToken = "PredToken"
  show IsZeroToken = "IsZeroToken"

%runElab derive "IntFuncInnerToken" [Generic, Eq]

public export
data AST =
    Truth
  | Falsehood
  | If AST AST AST
  | Zero
  | Succ AST
  | Pred AST
  | IsZero AST

%runElab derive "AST" [Generic, Eq]

export
Show AST where
  show Truth = "true"
  show Falsehood = "false"
  show (If a b c) = "(if " ++ (show a) ++ " then " ++ (show b) ++ " else " ++ (show c) ++ ")"
  show Zero = "0"
  show (Succ a) = "(succ " ++ (show a) ++ ")"
  show (Pred a) = "(pred " ++ (show a) ++ ")"
  show (IsZero a) = "(iszero " ++ (show a) ++ ")"

public export
data ExpressionToken = 
    Boolean Bool
  | IfToken
  | ThenToken
  | ElseToken
  | LParen
  | RParen
  | Whitespace
  | ZeroToken
  | IntFuncToken IntFuncInnerToken

%runElab derive "ExpressionToken" [Generic, Eq]

export
Show ExpressionToken where
  show (Boolean x) = "number " ++ show x
  show IfToken = "IfToken"
  show ThenToken = "ThenToken"
  show ElseToken = "ElseToken"
  show LParen = "("
  show RParen = ")"
  show Whitespace = "Whitespace"
  show ZeroToken = "ZeroToken"
  show (IntFuncToken x) = show x
