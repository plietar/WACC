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

frontend :: String -> String -> WACCResult Program
frontend source filename = do
  tokens <- waccLexer filename source
  ast <- waccParser filename tokens
  typeCheckProgram ast
  return ast

main :: IO ()
main = do
  filename <- fmap head getArgs
  contents <- readFile filename

  let (OK tokens) = waccLexer filename contents
      (OK ast) = waccParser filename tokens
  putStrLn (show ast)

  let result = frontend contents filename
  case result of
    OK _           -> putStrLn "Success !"
    Error kind msg -> putStrLn ("Error " ++ show kind ++ " " ++ msg)
  exitWith (exitCodeForResult result)
