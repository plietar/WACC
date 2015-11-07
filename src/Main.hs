module Main where
import Parser
import Lexer
import Common
import ScopedMap

import System.Environment
import System.Exit
import SemCheck
import AST
import Data.Map as Map
import Control.Monad.State

exitCodeForResult :: WACCResult a -> ExitCode
exitCodeForResult (OK _)                  = ExitSuccess
exitCodeForResult (Error LexicalError  _) = ExitFailure 100
exitCodeForResult (Error SyntaxError   _) = ExitFailure 100
exitCodeForResult (Error SemanticError _) = ExitFailure 200

frontend :: String -> WACCResult Program
frontend source = do
  tokens <- waccLexer source
  ast <- waccParser tokens
  typeCheckProgram ast
  return ast

main :: IO ()
main = do
  filename <- fmap head getArgs
  contents <- readFile filename

  let result = frontend contents
  case result of
    OK _           -> putStrLn "Success !"
    Error kind msg -> putStrLn ("Error " ++ show kind ++ " " ++ msg)
  exitWith (exitCodeForResult result)
