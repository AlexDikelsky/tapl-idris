module NamingRules

import Datatypes

Context : List (String, AST a)

-- freshName : Context -> a -> (Context a)

-- printTree : (Show a, Eq a) => List (String, AST a) -> (AST a) -> String
-- printTree ctx (Abs var_name body) = 
--   let (ctx', var_name') = freshName ctx var_name in
--       "(lambda " ++ (show var_name') ++ "." ++ (printTree ctx' body) ++ ")"
-- printTree ctx (App x y) = 
--   "(" ++ (printTree x) ++ " " ++ (printTree y) ++ ")"
-- printTree ctx (Term name) =
--    if ?ctxlen ctx == n
--       then ?index2name ctx x
--       else "!INVALID INDEX for " ++ (show name)
