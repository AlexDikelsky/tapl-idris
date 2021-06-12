module Main

import Data.Either
import Text.Parser.Core
import Text.Parser
import Text.Lexer
import Data.List

import Datatypes
import LambdaParser
import LambdaExecutor
import NamingRules

runProgram : String -> Either String (AST String)
runProgram program =
  case runParser program of
    Left parseError => Left $ show parseError
    Right validProg => Right (eval $ fst validProg)

-- ex : AST Integer
-- ex = Abs 5 (Term 3)

main: IO ()
main = do 
  putStrLn $ show $ runProgram "λa.a"
  putStrLn $ show $ runProgram "a"
  putStrLn $ show $ runProgram "(a b)"
  putStrLn $ show $ runProgram "λc.((a b) c)"
  putStrLn "Done"
