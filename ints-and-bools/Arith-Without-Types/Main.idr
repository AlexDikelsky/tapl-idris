module Main

import ArithParser
import Datatypes
import Data.Either
import Text.Parser.Core
import Text.Parser
import Text.Lexer
import Data.List

main: IO ()
main = do 
   putStrLn $ show $ getRight $ test "true"
   putStrLn $ show $ getRight $ test "false"
   putStrLn $ show $ getRight $ test "(false)"
   putStrLn $ show $ getRight $ test "if false else true"
   putStrLn $ show $ getRight $ test "if false then true else true"
   putStrLn $ show $ getRight $ test "(if false then true else true)"
   putStrLn $ show $ getRight $ test "if (if false then true else true) then false else false"
   putStrLn $ show $ getRight $ test "pred (pred 0)"
   putStrLn $ show $ getRight $ test "0"
   putStrLn "Done"
