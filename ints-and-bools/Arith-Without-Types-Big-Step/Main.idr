module Main

import Data.Either
import Text.Parser.Core
import Text.Parser
import Text.Lexer
import Data.List

import Datatypes
import ArithParser
import ArithExecutor

runProgram : String -> Either String AST
runProgram program =
  case runParser program of
    Left parseError => Left $ show parseError
    Right validProg => Right (eval $ fst validProg)

main: IO ()
main = do 
  putStrLn $ show $ runProgram "sdf"
  putStrLn $ show $ runProgram "true"
  putStrLn $ show $ runProgram "if true then false else false"
  putStrLn $ show $ runProgram "pred false"
  putStrLn $ show $ runProgram "succ pred 0"
  putStrLn $ show $ runProgram "pred succ 0"
  putStrLn $ show $ runProgram "pred pred pred succ succ succ 0"
  putStrLn $ show $ runProgram "if (if false then true else true) then true else false"
  putStrLn $ show $ runProgram "if (iszero 0) then true else false"
  putStrLn $ show $ runProgram "if (iszero (succ 0)) then true else false"
  putStrLn $ show $ runProgram "succ pred 0"
  putStrLn $ show $ runProgram "pred succ 0"
  putStrLn $ show $ runProgram "pred 0"
  putStrLn $ show $ runProgram "succ pred 0"
  putStrLn $ show $ runProgram "pred succ pred 0"
  putStrLn $ show $ runProgram "succ (if false then 0 else (succ 0))"
  putStrLn $ show $ runProgram "succ (if false then 0 else (succ 0))"
  putStrLn $ show $ runProgram "succ (if false then false else (succ 0))"
  putStrLn $ show $ runProgram "succ (if true then false else (succ 0))"
  putStrLn $ show $ runProgram "(if (if (iszero (iszero 0)) then (iszero (if 0 then 0 else 0)) else 0) then (iszero (if (pred 0) then (if 0 then 0 else 0) else 0)) else 0)"
  putStrLn $ show $ runProgram "(if (succ (if (pred 0) then 0 else (iszero 0))) then (succ (pred (iszero 0))) else (iszero (pred (if 0 then 0 else 0))))"
  putStrLn $ show $ runProgram "(if (pred (iszero (pred 0))) then (iszero (pred (iszero 0))) else (iszero (iszero (pred 0))))"
  putStrLn $ show $ runProgram "if 0 then 0 else (iszero (pred (iszero 0)))"
  putStrLn $ show $ runProgram "(pred (if 0 then (iszero 0) else (if (pred 0) then (if 0 then 0 else 0) else (pred 0))))"
  putStrLn $ show $ runProgram "((if true then (iszero 0) else (if (pred 0) then (if 0 then 0 else 0) else (pred 0))))"

  putStrLn "Done"
