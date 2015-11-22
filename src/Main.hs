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

import BlockGen
import ARMTypes


exitCodeForResult :: WACCResult a -> ExitCode
exitCodeForResult (OK _)                  = ExitSuccess
exitCodeForResult (Error LexicalError  _) = ExitFailure 100
exitCodeForResult (Error SyntaxError   _) = ExitFailure 100
exitCodeForResult (Error SemanticError _) = ExitFailure 200

frontend :: String -> String -> WACCResult Program
frontend source filename = do
  tokens <- waccLexer filename source
  ast <- waccParser filename tokens
  modifiedAst <- typeCheckProgram ast
  return modifiedAst

backend :: Program -> WACCResult [Instruction]
backend (Program funcs block) = do 
  result <- genBlock block
  --let vars = blockGeneration block 
  --return [SUB "jaime", ADD "John Ripper", PUSH (show vars)]
  return result

main :: IO ()
main = do
  filename <- fmap head getArgs
  contents <- readFile filename

  let result = frontend contents filename
  case result of
    OK prog        -> putStrLn ("Success !" ++ "\n" ++ show prog)
    Error kind msg -> do
      putStrLn ("Error " ++ show kind)
      putStr (unlines (reverse msg))
  exitWith (exitCodeForResult result)
