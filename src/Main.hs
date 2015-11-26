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
import Data.Maybe

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Graph.Inductive.Graph (Graph)
import qualified Data.Graph.Inductive.Graph as Graph

import Control.Monad.State

import ARMTypes
import FunctionCodeGen
import CodeGen

import RegisterAllocation.ControlFlow
import RegisterAllocation.DataFlow
import RegisterAllocation.GraphColouring

import Data.Graph.Inductive.PatriciaTree (Gr)

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
  let cfg = basicBlocks ir :: Gr [IR] ()

  forM_ (Graph.labNodes cfg) $ \(idx, irs) -> do
    print idx
    forM_ irs (putStrLn . ("  " ++) . show)
  putStrLn ""

  forM_ (Graph.nodes cfg) $ \idx -> do
    let ctx = Graph.context cfg idx
    putStrLn (show (Graph.pre' ctx) ++ " -> " ++
              show (Graph.node' ctx) ++ " -> " ++
              show (Graph.suc' ctx))
  putStrLn ""

  let flow = blockDataFlow cfg
  let live = liveVariables cfg flow
  let rig = interferenceGraph live :: Gr Var ()

  forM_ (Graph.nodes rig) $ \idx -> do
    let ctx = Graph.context rig idx
    putStrLn (show (Graph.lab' ctx) ++ " - " ++ show (map (fromJust . Graph.lab rig) (Graph.suc' ctx)))

  putStrLn ""

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

