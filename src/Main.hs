module Main where
import Parser
import Lexer
import Common
import ScopedMap

import System.Environment
import System.Exit
--import SemCheck
import AST
import Data.Map as Map
import Control.Monad.State

exitCodeForResult :: WACCResult a -> ExitCode
exitCodeForResult (OK _)                  = ExitSuccess
exitCodeForResult (Error LexicalError  _) = ExitFailure 100
exitCodeForResult (Error SyntaxError   _) = ExitFailure 100
exitCodeForResult (Error SemanticError _) = ExitFailure 200

frontend :: String -> String -> WACCResult (Annotated Program SpanA)
frontend source filename = do
  tokens <- waccLexer filename source
  ast <- waccParser filename tokens
  --typeCheckProgram ast
  return ast

main :: IO ()
main = do
  filename <- fmap head getArgs
  contents <- readFile filename

  let result = frontend contents filename
  case result of
    OK _           -> putStrLn "Success !"
    Error kind msg -> do
      putStrLn ("Error " ++ show kind)
      putStr (unlines (reverse msg))
  exitWith (exitCodeForResult result)
