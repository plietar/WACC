module Main where
import Parser
import Lexer
import Common
import ScopedMap
import BlockGen

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
  modifiedAst <- typeCheckProgram ast
  return modifiedAst

backend :: Program -> WACCResult [Instruction]
backend (Program funcs block) = do 
  let vars = blockGeneration block 
  return [SUB "jaime", ADD "John Ripper", PUSH (show vars)]

main :: IO ()
main = do
  filename <- fmap head getArgs
  contents <- readFile filename

  let result = frontend contents filename
  case result of
   -- OK prog        -> putStrLn ("Success AST generation!")
    OK prog -> do 
      putStrLn ("Success AST generation!")
      let assembly = backend prog 
      case assembly of
        --OK instrs -> putStrLn (show instrs)
        OK instrs -> mapM_ (putStrLn . show) instrs 

    Error kind msg -> do
      putStrLn ("Error " ++ show kind)
      putStr (unlines (reverse msg))

  exitWith (exitCodeForResult result) 


