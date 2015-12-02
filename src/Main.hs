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

showColouring :: Map Var Var -> [String]
showColouring colouring = execWriter $ do
  forM_ (Map.toList colouring) $ \(v,col) -> do
    tell [show v ++ " -> " ++ ": " ++ show col]

#if WITH_GRAPHVIZ
showDotCFG :: Graph gr => gr [IR] () -> [String]
showDotCFG = (:[]) . unpack . GraphViz.printDotGraph . GraphViz.setDirectedness GraphViz.graphToDot dotCFGParams

dotCFGParams :: GraphViz.GraphvizParams n [IR] () () [IR]
dotCFGParams = GraphViz.nonClusteredParams
              { GraphViz.globalAttributes = ga
              , GraphViz.fmtNode = fn }
  where
    ga = [ GraphViz.NodeAttrs [ GraphViz.shape GraphViz.BoxShape ]]

    fn (_, l)    = [(GraphViz.toLabel . unlines . map show) l ]

showDotRIG :: Graph gr => gr Var () -> [String]
showDotRIG = (:[]) . unpack . GraphViz.printDotGraph . GraphViz.setDirectedness GraphViz.graphToDot dotRIGParams

dotRIGParams :: GraphViz.GraphvizParams n Var () () Var
dotRIGParams = GraphViz.nonClusteredParams
              { GraphViz.globalAttributes = ga
              , GraphViz.fmtNode = fn }
  where
    ga = [ GraphViz.GraphAttrs [ GraphViz.RankDir GraphViz.FromLeft
                               , GraphViz.bgColor GraphViz.White
                               , GraphViz.Layout GraphViz.Fdp ]

         , GraphViz.NodeAttrs [ GraphViz.shape GraphViz.Ellipse
                              , GraphViz.fillColor GraphViz.White
                              , GraphViz.style GraphViz.filled ] ]

    fn (_, l) = [(GraphViz.toLabel . show) l]

showDotColouring :: (Graph gr, Eq a, Ord a) => gr Var () -> Map Var a -> [String]
showDotColouring cfg colouring
  = (:[]) . unpack . GraphViz.printDotGraph . GraphViz.setDirectedness GraphViz.graphToDot (dotColouringParams colouring) $ cfg

dotColouringParams :: (Eq a, Ord a) => Map Var a -> GraphViz.GraphvizParams Int Var () () Var
dotColouringParams colouring = dotRIGParams { GraphViz.fmtNode = fn }
  where
    fn (_, l) = [(GraphViz.toLabel . show) l
                , GraphViz.FillColor (GraphViz.toColorList [colourMap ! l])]

    colourSet = Set.fromList (Map.elems colouring)
    colourMap = Map.map (\v -> colour (Set.findIndex v colourSet)) colouring

    colourCount = Set.size colourSet
    colourStep = 1.0 / (fromIntegral (colourCount + 1))

    colour    i = GraphViz.HSV (colourHue i) (colourSat i) (colourVal i)
    colourHue i = colourStep * fromIntegral i
    colourSat _ = 1
    colourVal _ = 1

#endif

compile :: String -> String -> OutputType -> WACCResult [String]
compile filename contents output
  = case output of
    OutputTokens       -> showTokens <$> tokens
    OutputAST          -> (:[]) . show <$> ast
    OutputTypedAST     -> (:[]) . show <$> typedAst
    OutputIR           -> concatMap (map show) <$> live
    OutputCFG          -> concatMap showCFG <$> cfg
    OutputRIG          -> concatMap showRIG <$> rig
    OutputColouring    -> concatMap showColouring <$> colouring
    OutputIRAlloc      -> concatMap showIR <$> allocIR
    OutputCFGColoured  -> concatMap showCFG <$> cfgColoured
    OutputASM          -> concatMap (concatMap snd . Graph.labNodes) <$> asm
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
    allocIR   = zipWith applyColouring <$> ir <*> colouring
    cfgColoured = zipWith (\g m -> Graph.nmap ((flip applyColouring) m) g)
                    <$> cfg <*> colouring :: WACCResult [Gr [IR] ()]

    asm       = map (Graph.nmap (assembly . genARM)) <$> cfgColoured

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

