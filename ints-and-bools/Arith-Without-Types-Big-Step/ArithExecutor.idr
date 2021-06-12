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

public export
eval : AST -> AST

-- B-Value
eval Truth = Truth
eval Falsehood = Falsehood
eval Zero = Zero -- Need case for all numeric, probably in succ

-- B-IfTrue and B-IfFalse
eval (If t1 t2 t3) =
  case t1_value of
    Truth => t2_value
    Falsehood => t3_value
    _ => (If t1 t2 t3)
  where
    t1_value : AST
    t1_value = eval t1
    t2_value : AST
    t2_value = eval t2
    t3_value : AST
    t3_value = eval t3

-- B-Succ
eval (Succ t1) =
  if is_nv t1_value 
     then t1_value
     else (Succ t1)
  where
    t1_value : AST
    t1_value = eval t1

-- B-PredZero
-- B-PredSucc
eval (Pred t1) =
  if is_nv t1_value
     then if t1_value == Zero
             then Zero
             else Succ t1_value
     else Pred t1
  where
    t1_value : AST
    t1_value = eval t1


-- B-IsZeroZero
-- B-IsZeroSucc
eval (IsZero t1) =
  if is_nv t1_value
     then case t1_value of
               Zero => Truth
               Succ x => Falsehood
               _ => t1
     else t1
  where
    t1_value : AST
    t1_value = eval t1
