{-# LANGUAGE CPP #-}

module OutputFormatting where
import CodeGenTypes
import Common.AST
import Common.Span
import Frontend.Tokens
import Common.WACCResult

import Control.Monad.RWS
import Control.Monad.Writer
import Control.Monad.Reader

import Data.List
import Data.Maybe
import Control.Applicative

import Data.Map (Map,(!))
import qualified Data.Map as Map
import qualified Data.Set as Set

import Data.Graph.Inductive.Graph (Graph)
import Data.Graph.Inductive.PatriciaTree (Gr)
import qualified Data.Graph.Inductive.Graph as Graph


#if WITH_GRAPHVIZ
import qualified Data.GraphViz as GraphViz
import qualified Data.GraphViz.Attributes.Complete as GraphViz
import Data.Text.Lazy (unpack)
#endif

showTokens :: [(Pos, Token)] -> [String]
showTokens = (:[]) . show . map snd


showProgram :: (Annotated Program TypeA) -> PrintAST ()
showProgram (_, Program fs) = do
  indent <- ask
  tell [ (tabs indent) ++ "Program" ]
  forM_ fs (\f -> local (+ 1) (showFuncDef f))
  return ()

showFuncDef :: (Annotated FuncDef TypeA) -> PrintAST ()
showFuncDef (_, FuncDef t n args block) = do
  indent <- ask
  tell [ (tabs indent) ++ "- "
         ++ (show t) ++ " " ++ (show n) ++ "(" ++ showArgs args ++ ")" ]
  local (+ 1) (showBlock block)
  return ()
  
showArgs :: [(Type, Identifier)] -> String
showArgs ((t, _) : args)
  = (show t) ++ ", " ++ showArgs args
showArgs [] = ""
 
  
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
    fn (_, l) = [(GraphViz.toLabel . show) (l, colourCount)
                , GraphViz.FillColor (GraphViz.toColorList [colourMap ! l])]

    colours = Map.fromList (zip (nub (Map.elems colouring)) (map colour [0..]))
    colourMap = Map.map (colours !) colouring

    colourCount = Map.size colours
    colourStep = 1.0 / (fromIntegral (colourCount + 1))

    colour    i = GraphViz.HSV (colourHue i) (colourSat i) (colourVal i)
    colourHue i = colourStep * fromIntegral i
    colourSat _ = 1
    colourVal _ = 1

#endif


