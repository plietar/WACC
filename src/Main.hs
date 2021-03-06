{-# LANGUAGE CPP #-}

module Main where
import Common.AST
import Common.WACCResult
import Frontend.Parser
import Frontend.Lexer
import Frontend.SemCheck
import CodeGenTypes
import CodeGen
import Arguments
import ARMGen
import OutputFormatting
import Features
import Frontend.Tokens
import Common.Span

import Data.List (zipWith5)
import Data.Maybe (fromMaybe)
import Data.Tuple (swap)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map

import Control.Applicative
import Control.Monad

import RegisterAllocation.ControlFlow
import RegisterAllocation.DataFlow
import RegisterAllocation.GraphColouring

import System.Exit
import System.FilePath.Posix

import Data.Graph.Inductive.PatriciaTree (Gr)
import qualified Data.Graph.Inductive.Graph as Graph

import Control.Monad.Trans

exitCodeForResult :: WACCResult a -> ExitCode
exitCodeForResult (OK _)                  = ExitSuccess
exitCodeForResult (Error LexicalError  _) = ExitFailure 100
exitCodeForResult (Error SyntaxError   _) = ExitFailure 100
exitCodeForResult (Error SemanticError _) = ExitFailure 200
exitCodeForResult (Error CodeGenError  _) = ExitFailure 1

compile :: String -> String -> OutputType -> WACCArguments (WACCResult [String])
compile filename contents output = do
  let tokens = waccLexer filename contents :: WACCResult [(Pos, Token)]

  ast       <- runWACCResultT (waccParser filename =<< waccResultT tokens)
  typeCheck <- runWACCResultT (waccSemCheck =<< waccResultT ast)
  let
    typedAst = fst <$> typeCheck
    typeData = snd <$> typeCheck

  codeGen   <- fmapM2 genProgram typedAst typeData

  let
    irFunctions :: WACCResult [(Bool, [IR])]
    irFunctions = fst <$> codeGen

    async     = map fst <$> irFunctions
    ir        = map snd <$> irFunctions

    irFeatures :: WACCResult (Set Feature)
    irFeatures = snd <$> codeGen

    cfg       :: WACCResult [Gr [IR] ()]
    cfg       = map (deadCodeElimination . basicBlocks) <$> ir

    flow      :: WACCResult [Gr ([IR], FlowInfo) ()]
    flow      = map blockDataFlow <$> cfg

    allVars   = map allVariables <$> cfg
    live      = map liveVariables <$> flow
    rig       = zipWith interferenceGraph <$> allVars <*> live :: WACCResult [Gr Var ()]
    moves     = zipWith movesGraph <$> allVars <*> cfg

    allocation = join (sequence <$> (zipWith5 allocateRegisters <$> async <*> allVars <*> live <*> rig <*> moves))
    cfgFinal  = map (\(x, _, _) -> x) <$> allocation
    rigFinal  = map (\(_, y, _) -> y) <$> allocation
    colouring = map (\(_, _, z) -> z) <$> allocation

    irFinal :: WACCResult [IR]
    irFinal   = concatMap (concatMap snd . Graph.labNodes) <$> cfgFinal

  armWriter <- fmapM genARM irFinal

  let
    armFeatures = ARMGen.features <$> armWriter
    feat      = genFeatures <$> (Set.union <$> armFeatures <*> irFeatures)
    asmSimple = concat <$> sequence
                             [ dataSegment <$> armWriter
                             , fst <$> feat
                             , textSegment <$> armWriter
                             , snd <$> feat] :: WACCResult [String]
    asm       = map tabbedInstruction <$> asmSimple

  return $ case output of
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
    OutputDotColouring -> concat <$> (zipWith showDotColouring <$> rigFinal <*> colouring)
#endif

main :: IO ()
main = withArguments $ do
  filename <- getArgument sourceFile
  let defaultOutputFile = dropExtension (takeFileName filename) <.> "s"

  outFile <- fromMaybe defaultOutputFile <$> getArgument outputFile

  let out = if outFile == "-"
            then putStr
            else writeFile outFile

  contents <- lift $ readFile filename

  outType <- getArgument outputType

  result <- liftArguments $ compile filename contents outType
  case result of
    OK output -> lift $ out (unlines output)
    Error kind msg -> do
      lift $ putStrLn ("Error " ++ show kind)
      lift $ putStr (unlines (reverse msg))

  lift $ exitWith (exitCodeForResult result)

