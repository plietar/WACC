module Main where
import Parser
import Lexer
import Common
import ScopedMap

import System.Environment
import System.Exit
import SemCheck
import AST

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Control.Monad.State

import ARMTypes
import FunctionCodeGen
import CodeGen

import RegisterAllocation.ControlFlow
import RegisterAllocation.DataFlow
import RegisterAllocation.GraphColouring


exitCodeForResult :: WACCResult a -> ExitCode
exitCodeForResult (OK _)                  = ExitSuccess
exitCodeForResult (Error LexicalError  _) = ExitFailure 100
exitCodeForResult (Error SyntaxError   _) = ExitFailure 100
exitCodeForResult (Error SemanticError _) = ExitFailure 200

showSet s = "(" ++ intercalate ", " (map show (Set.toAscList s)) ++ ")"

frontend :: String -> String -> WACCResult (Annotated Program TypeA)
frontend source filename = do
  tokens <- waccLexer filename source
  ast <- waccParser filename tokens
  typedAst <- waccSemCheck ast
  return typedAst

backend :: (Annotated Program TypeA) -> IO ()
backend (_, Program funcs block) = do
  let ir = genFunction ((), FuncDef TyVoid "main" [] block)
  let (bb, cfg, revCfg) = basicBlocks ir

  forM_ (Map.assocs bb) $ \(idx, irs) -> do
    print idx
    forM_ irs (putStrLn . ("  " ++) . show)
  putStrLn ""
  forM_ (Map.assocs cfg) $ \(source, targets) -> do
    putStrLn (show source ++ " -> " ++ showSet targets)
  putStrLn ""
  forM_ (Map.assocs revCfg) $ \(target, sources) -> do
    putStrLn (show target ++ " <- " ++ showSet sources)
    

main :: IO ()
main = do
  filename <- fmap head getArgs
  contents <- readFile filename

  let result = frontend contents filename
  case result of
    OK prog -> do
      putStrLn (show result)
      putStrLn ("Success AST generation!")
      putStrLn ""
      backend prog
    Error kind msg -> do
      putStrLn ("Error " ++ show kind)
      putStr (unlines (reverse msg))

  exitWith (exitCodeForResult result)

