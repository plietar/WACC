{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}

module Main where
import Common.AST
import Common.Span
import Common.WACCResult
import Frontend.Parser
import Frontend.Lexer
import Frontend.Tokens
import Frontend.SemCheck
import CodeGenTypes
import CodeGen
import Arguments
import ARMGen
import OutputFormatting

import RegisterAllocation.ControlFlow
import RegisterAllocation.DataFlow
import RegisterAllocation.GraphColouring

import System.Exit

import Data.List
import Data.Maybe
import Control.Applicative

import Data.Map (Map,(!))
import qualified Data.Map as Map
import qualified Data.Set as Set

import Data.Graph.Inductive.Graph (Graph)
import Data.Graph.Inductive.PatriciaTree (Gr)
import qualified Data.Graph.Inductive.Graph as Graph

import Control.Monad.State
import Control.Monad.Writer

#if WITH_GRAPHVIZ
import qualified Data.GraphViz as GraphViz
import qualified Data.GraphViz.Attributes.Complete as GraphViz
import Data.Text.Lazy (unpack)
#endif

exitCodeForResult :: WACCResult a -> ExitCode
exitCodeForResult (OK _)                  = ExitSuccess
exitCodeForResult (Error LexicalError  _) = ExitFailure 100
exitCodeForResult (Error SyntaxError   _) = ExitFailure 100
exitCodeForResult (Error SemanticError _) = ExitFailure 200
exitCodeForResult (Error CodeGenError  _) = ExitFailure 1

compile :: String -> String -> OutputType -> WACCResult [String]
compile filename contents output
  = case output of
    OutputTokens       -> showTokens <$> tokens
    OutputAST          -> (:[]) . show <$> ast
    OutputTypedAST     -> showTypedAST <$> typedAst
    OutputIR           -> concatMap showIR <$> ir
    OutputCFG          -> concatMap showCFG <$> cfg
    OutputRIG          -> concatMap showRIG <$> rig
    OutputColouring    -> concatMap showColouring <$> colouring
    OutputIRFinal      -> showIR <$> irFinal
    OutputASM          -> asm
#if WITH_GRAPHVIZ
    OutputDotCFG       -> concatMap showDotCFG <$> cfg
    OutputDotRIG       -> concatMap showDotRIG <$> rig
    OutputDotColouring -> concat <$> (zipWith showDotColouring <$> rig <*> colouring)
#endif

  where
    tokens    = waccLexer  filename contents
    ast       = waccParser filename =<< tokens
    typedAst  = waccSemCheck        =<< ast
    funcs     = (\(_, Program fs) -> fs) <$> typedAst
    ir        = map genFunction <$> funcs
    cfg       = map (deadCodeElimination . basicBlocks) <$> ir :: WACCResult [Gr [IR] ()]
    flow      = map blockDataFlow <$> cfg
    live      = zipWith liveVariables <$> cfg <*> flow
    rig       = map interferenceGraph <$> live :: WACCResult [Gr Var ()]
    colouring = sequence <$> map (\g -> colourGraph g (fmap Var [4..12])) <$> rig >>= \case
                Just c  -> OK c
                Nothing -> codegenError "Graph Colouring failed"
    cfgFinal  = zipWith (\c g -> Graph.nmap (applyColouring c) g)
                    <$> colouring <*> cfg

    irFinal   = concatMap (concatMap snd . Graph.labNodes) <$> cfgFinal

    armWriter = genARM <$> irFinal
    asm       = (++) <$> (dataSegment <$> armWriter) <*> (textSegment <$> armWriter)

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

