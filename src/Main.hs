{-# LANGUAGE CPP #-}

module Main where
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

import Data.List (zipWith4)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set

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
  typedAst  <- runWACCResultT (waccSemCheck =<< waccResultT ast)
  codeGen   <- fmapM genProgram typedAst

  let
    ir        = fst <$> codeGen
    irFeatures = snd <$> codeGen
    cfg       = map (deadCodeElimination . basicBlocks) <$> ir :: WACCResult [Gr [IR] ()]
    flow      = map blockDataFlow <$> cfg
    allVars   = map allVariables <$> cfg
    live      = map liveVariables <$> flow
    rig       = zipWith interferenceGraph <$> allVars <*> live :: WACCResult [Gr Var ()]
    moves     = zipWith movesGraph <$> allVars <*> cfg

    allocation = join (sequence <$> (zipWith4 allocateRegisters <$> allVars <*> live <*> rig <*> moves))
    cfgFinal  = map fst <$> allocation
    colouring = map snd <$> allocation

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
    OutputDotColouring -> concat <$> (zipWith showDotColouring <$> rig <*> colouring)
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

