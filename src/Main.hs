{-# LANGUAGE LambdaCase #-}
module Main where
import Parser
import Lexer
import Common
import Tokens
import ScopedMap

import System.Environment
import System.Exit
import SemCheck
import AST

import Data.List
import Data.Maybe
import Control.Applicative

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Graph.Inductive.Graph (Graph)
import qualified Data.Graph.Inductive.Graph as Graph

import Control.Monad.State
import Control.Monad.Writer

import ARMTypes
import FunctionCodeGen
import CodeGen

import RegisterAllocation.ControlFlow
import RegisterAllocation.DataFlow
import RegisterAllocation.GraphColouring

import Data.Graph.Inductive.PatriciaTree (Gr)

import Arguments

exitCodeForResult :: WACCResult a -> ExitCode
exitCodeForResult (OK _)                  = ExitSuccess
exitCodeForResult (Error LexicalError  _) = ExitFailure 100
exitCodeForResult (Error SyntaxError   _) = ExitFailure 100
exitCodeForResult (Error SemanticError _) = ExitFailure 200
exitCodeForResult (Error CodeGenError  _) = ExitFailure 1

{-
  putStrLn ("Colouring")
  putStrLn ""
  -}

showTokens :: [(Pos, Token)] -> [String]
showTokens = (:[]) . show . map snd

showIR :: [IR] -> [String]
showIR = map show

showCFG :: Graph gr => gr [IR] () -> [String]
showCFG cfg = execWriter $ do
  forM_ (Graph.labNodes cfg) $ \(idx, irs) -> do
    tell [show idx]
    tell (map show irs)
    tell [""]

  forM_ (Graph.nodes cfg) $ \idx -> do
    let ctx = Graph.context cfg idx
    tell [(show (Graph.pre' ctx) ++ " -> " ++
           show (Graph.node' ctx) ++ " -> " ++
           show (Graph.suc' ctx))]

showRIG :: Graph gr => gr Var () -> [String]
showRIG rig = execWriter $ do
  forM_ (Graph.nodes rig) $ \idx -> do
    let ctx = Graph.context rig idx
    tell [(show (Graph.lab' ctx) ++ " - " ++
           show (map (fromJust . Graph.lab rig) (Graph.suc' ctx)))]

showColouring :: Map Int Colour -> [String]
showColouring colouring = execWriter $ do
  forM_ (Map.toList colouring) $ \(v,col) -> do
    tell ["Node: " ++ show v ++ " -> " ++ "Colour: " ++ show col]


compile :: String -> String -> OutputType -> WACCResult [String]
compile filename contents output
  = case output of
    OutputTokens       -> showTokens <$> tokens
    OutputAST          -> (:[]) . show <$> ast
    OutputTypedAST     -> (:[]) . show <$> typedAst
    OutputIR           -> showIR <$> ir
    OutputCFG          -> showCFG <$> cfg
    OutputRIG          -> showRIG <$> rig
    OutputColouring    -> showColouring <$> colouring

  where
    tokens    = waccLexer  filename contents
    ast       = waccParser filename =<< tokens
    typedAst  = waccSemCheck        =<< ast
    block     = (\(_, Program _ b) -> b) <$> typedAst
    ir        = genFunction . (\b -> ((), FuncDef TyVoid "main" [] b)) <$> block
    cfg       = basicBlocks <$> ir :: WACCResult (Gr [IR] ())
    flow      = blockDataFlow <$> cfg
    live      = liveVariables <$> cfg <*> flow
    rig       = interferenceGraph <$> live :: WACCResult (Gr Var ())
    colouring = colourGraph <$> rig <*> pure [0..15] >>= \case
                Just c  -> OK c
                Nothing -> codegenError "Graph Colouring failed"

main :: IO ()
main = do
  args <- waccArguments

  let filename = sourceFile args
  contents <- readFile filename
  
  let result = compile filename contents (outputType args)
  case result of
    OK output -> putStr (unlines output)
    Error kind msg -> do
      putStrLn ("Error " ++ show kind)
      putStr (unlines (reverse msg))

  exitWith (exitCodeForResult result)

