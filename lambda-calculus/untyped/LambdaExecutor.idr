module LambdaExecutor
import Datatypes
import LambdaParser

import Data.List

public export
eval : (AST n) -> (AST n)
eval a = a

f : (Eq a) => a -> List a -> Maybe Nat
f n (x :: xs) = if n == x then Just 0 else map (+ 1) (f n xs)
f n [] = Nothing


-- Context has most recent name first
public export
removeNames : Eq a => List a -> AST a -> Maybe (AST Nat)
removeNames context (Term x _) = 
  case f x context of
       Just x => Just $ Term x 0
       Nothing => Nothing

removeNames context (Abs t x) = removeNames (t :: context) x

removeNames context (App a b) = 
  case (removeNames context a, removeNames context b) of
       (Just a, Just b) => Just $ App a b
       _ => Nothing

termShift : Nat -> AST Nat -> AST Nat
termShift d t = walk 0 t
where
  walk : Nat -> AST Nat -> AST Nat 
  walk c (Term x n) = 
    if x >= c 
       then Term (x + d) (n + d) 
       else Term x (n + d)
  walk c (Abs x t1) = Abs x (walk (c + 1) t1)
  walk c (App x y) = App (walk c x) (walk c y)

termSubst : AST Nat -> AST Nat
