import Text.Parser.Core
import Text.Parser
import Text.Lexer
import Data.List
import Data.Either
 
data ExpressionToken = 
    Boolean Bool
  | If
  | Then
  | Else
  | OParen
  | CParen
  | Whitespace

Eq ExpressionToken where
  If == If = True
  Then == Then = True
  Else == Else = True
  OParen == OParen = True
  CParen == CParen = True
  (Boolean x) == (Boolean y) = x == y
  Whitespace == Whitespace = True
  _ == _ = False

Show ExpressionToken where
  show (Boolean x) = "number " ++ show x
  show If = "If"
  show Then = "Then"
  show Else = "Else"
  show OParen = "("
  show CParen = ")"
  show Whitespace = "Whitespace"

exprTokens : TokenMap ExpressionToken
exprTokens =
  [(exact "if",    \x => If),
   (exact "then",  \x => Then),
   (exact "else",  \x => Else),
   (exact "true",  \x => Boolean True),
   (exact "false", \x => Boolean False),
   (is '(', \x => OParen),
   (is ')', \x => CParen),
   (is ' ', \x => Whitespace)
   ]

Rule : Type -> Type
Rule ty = Grammar (TokenData ExpressionToken) True ty

boolLiteral : Rule Bool
boolLiteral = 
  terminal 
    "Failed on Bool parsing" 
    (\x => case tok x of
      Boolean i => Just i
      _ => Nothing)

rule' : ExpressionToken -> Rule ()
rule' s = terminal ("Expected " ++ (show s))
                 (\x => case s == (tok x) of 
                    True  => Just ()
                    False => Nothing)
    
wh : Rule ()
wh = rule' Whitespace

-- This evaluates more of the tree than it needs to.
-- TODO: Skip "then" part if test is false, and skip "else" part if test is true
expr : Rule Bool
expr = 
  (do
    rule' If
    wh
    test <- expr
    wh
    rule' Then
    wh
    was_true <- expr
    wh
    rule' Else
    wh
    was_false <- expr
    pure (if test then was_true else was_false)) 
  <|> boolLiteral
  <|> (do
    rule' OParen
    e <- expr
    rule' CParen
    pure e)


export
Show (ParseError token) where 
   show (Error s l) = s 


test : String -> Either (ParseError (TokenData ExpressionToken))
                (Bool, List (TokenData ExpressionToken))
test s = parse expr (fst (lex exprTokens s))

main: IO ()
main = do 
   putStrLn $ show $ getRight $ test "true"
   putStrLn $ show $ getRight $ test "false"
   putStrLn $ show $ getRight $ test "(false)"
   putStrLn $ show $ getRight $ test "if false else true"
   putStrLn $ show $ getRight $ test "(if false then true else true)"
   putStrLn $ show $ getRight $ test "if (if false then true else true) then false else false"
   putStrLn "Done"
