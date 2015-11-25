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

import BlockGen
import ARMTypes


exitCodeForResult :: WACCResult a -> ExitCode
exitCodeForResult (OK _)                  = ExitSuccess
exitCodeForResult (Error LexicalError  _) = ExitFailure 100
exitCodeForResult (Error SyntaxError   _) = ExitFailure 100
exitCodeForResult (Error SemanticError _) = ExitFailure 200

frontend :: String -> String -> WACCResult (Annotated Program TypeA)
frontend source filename = do
  tokens <- waccLexer filename source
  ast <- waccParser filename tokens
  typedAst <- waccSemCheck ast
  return typedAst

backend :: (Annotated Program TypeA) -> WACCResult [Instruction]
backend (_, Program funcs block) = undefined

main :: IO ()
main = do
  filename <- fmap head getArgs
  contents <- readFile filename

  let result = frontend contents filename
  case result of
    OK prog -> do
      putStrLn (show result)
      putStrLn ("Success AST generation!")
      let assembly = backend prog
      case assembly of
        --OK instrs -> putStrLn (show instrs)
        OK instrs -> mapM_ (putStrLn . show) instrs
    Error kind msg -> do
      putStrLn ("Error " ++ show kind)
      putStr (unlines (reverse msg))

  exitWith (exitCodeForResult result)

