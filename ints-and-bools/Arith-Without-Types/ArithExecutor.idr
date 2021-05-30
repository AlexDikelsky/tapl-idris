module ArithExecutor

import Text.Parser.Core
import Text.Parser
import Text.Lexer
import Data.List
import Data.Either
import Debug.Trace

import Datatypes
import ArithParser

is_nv : AST -> Bool
is_nv Zero = True
is_nv (Succ x) = is_nv x
is_nv _ = False

-- Left is when we have a Normal Form or "Stuck form" like True, or Pred True
-- Right is when it isn't a Normal Form
eval1 : AST -> Either AST AST

-- Constants
eval1 Truth     = Left Truth
eval1 Falsehood = Left Falsehood
eval1 Zero      = Left Zero

-- E-IfTrue and E-IfFalse
eval1 (If Truth     when_true  _) = Right when_true
eval1 (If Falsehood _ when_false) = Right when_false

-- E-If
eval1 (If test a b) = 
  case eval1 test of 
    Right t' => Right (If t' a b)
    _ => Left (If test a b)

-- E-Succ
eval1 (Succ t) = 
  case eval1 t of
    Right t' => Right (Succ t')
    _ => Left (Succ t)

-- E-PredZero
eval1 (Pred Zero) = 
  Right Zero

eval1 (Pred (Succ nv)) =
  if (is_nv nv) then
    Right nv
  else
    case eval1 nv of
      --  If nv is a normal form, then return that
      Left  adt => Left  $ Pred (Succ adt)
      -- Otherwise, evaluate it
      Right adt => Right $ Pred (Succ adt)

eval1 (Pred t) = 
  case eval1 t of
    Right t' => Right (Pred t')
    _ => Left (Pred t)

eval1 (IsZero Zero) = Right Truth
eval1 (IsZero (Succ nv)) = 
  if (is_nv nv) then
    Right Falsehood
  else
    Left (IsZero (Succ nv))

eval1 (IsZero t) = 
  case eval1 t of
    Right t' => Right (IsZero t')
    _ => Left (IsZero t)

export
eval : AST -> AST
eval ast = 
  -- trace ("AST: " ++ (show ast)) $ 
  case (eval1 ast) of
    Left ast' => ast'
    Right ast' => eval ast'
