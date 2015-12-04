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

type PrintAST = RWS Int [String] ()

tabs :: Int -> String
tabs x = concat $ take (2 * x) (repeat " ")

showTypedAST :: (Annotated Program TypeA) -> [String]
showTypedAST program
  = snd (execRWS (showProgram program) 0 ())

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

showStmt :: (Annotated Stmt TypeA) -> PrintAST ()
showStmt (_, StmtSkip) = return ()
showStmt (_, StmtVar t id assignrhs) = do
  indent <- ask
  tell [ (tabs indent) ++ "- "  ++ " VAR " ++ (show t)  ++ " " ++ (show id) ]
  local (+ 1) (showAssignRHS assignrhs)
  return ()

showStmt (_, StmtAssign lhs rhs) = do
  indent <- ask
  tell [ (tabs indent) ++ "- ASSIGN "]
  local (+ 1) (showAssignRHS rhs)
  local (+ 1) (showAssignLHS lhs)
  return ()

showStmt (_, StmtRead lhs) = do
  indent <- ask
  tell [ (tabs indent) ++ "- READ " ]
  local (+ 1) (showAssignLHS lhs)
  return ()

showStmt (_, StmtFree e) = do
  indent <- ask
  tell [ (tabs indent) ++ "- FREE " ]
  local (+ 1) (showExpr e)
  return ()

showStmt (_, StmtReturn e) = do
  indent <- ask
  tell [ (tabs indent) ++ "- RETURN" ]
  local (+ 1) (showExpr e)
  return ()
showStmt (_, StmtExit e) = do
  indent <- ask
  tell [ (tabs indent) ++ "- EXIT" ]
  local (+ 1) (showExpr e)
  return ()

showStmt (_, StmtPrint e b) = do
  indent <- ask
  tell [ (tabs indent) ++ "- PRINT" ++ line ]
  local (+ 1) (showExpr e)
  return ()
  where
    line = if b then "LN" else ""

showStmt (_, StmtIf cond b1 b2) = do
  indent <- ask
  tell [ (tabs indent) ++ "- IF" ]
  tell [ (tabs (indent + 1)) ++ "- COND" ]
  local (+ 2) (showExpr cond)
  tell [ (tabs (indent + 1)) ++ "- THEN" ]
  local (+ 2) (showBlock b1)
  tell [ (tabs (indent + 1)) ++ "- ELSE" ]
  local (+ 2) (showBlock b2)
  return ()

showStmt (_, StmtWhile cond b) = do
  indent <- ask
  tell [ (tabs indent) ++ "- WHILE" ]
  tell [ (tabs (indent + 1)) ++ "- COND" ]
  local (+ 2) (showExpr cond)
  tell [ (tabs (indent + 1)) ++ "- BODY" ]
  local (+ 2) (showBlock b)
  return ()

showStmt (_, StmtScope b) = do
  indent <- ask
  tell [ (tabs indent) ++ "- SCOPE "]
  local (+ 1) (showBlock b)
  return ()

showBlock :: (Annotated Block TypeA) -> PrintAST ()
showBlock ((b, xs), Block stmts) = do
  indent <- ask
  tell [ (tabs indent) ++ "Bool: " ++ show b ++ ", types: "
        ++ (concatMap show xs)]
  mapM_ showStmt stmts
  return ()

showExpr :: (Annotated Expr TypeA) -> PrintAST ()
showExpr (_, ExprLit l) = do
  indent <- ask
  tell [ (tabs indent) ++ "- LITERAL " ++ show l ]

showExpr (_, ExprVar v) = do
  indent <- ask
  tell [ (tabs indent) ++ "- VAR " ++ show v ]

showExpr (_, ExprArrayElem elem) = showArrayElem elem

showExpr (_, ExprUnOp op e) = do
  indent <- ask
  tell [ (tabs indent) ++ "- UNOP " ++ show op]
  local (+ 1) (showExpr e)

showExpr (_, ExprBinOp op e1 e2) = do
  indent <- ask
  tell [ (tabs indent) ++ "- BINOP " ++ show op ]
  local (+ 1) (showExpr e1)
  local (+ 1) (showExpr e2)




showAssignLHS :: (Annotated AssignLHS TypeA) -> PrintAST ()
showAssignLHS (_, LHSVar id) = do
  indent <- ask  
  tell [ (tabs indent) ++ "- LHSVAR " ++ show id ]

showAssignLHS (_, LHSPairElem elem) = do
  indent <- ask  
  tell [ (tabs indent) ++ "- LHSPAIRELEM " ]
  local (+ 1) (showPairElem elem)

showAssignLHS (_, LHSArrayElem elem) = do
  indent <- ask  
  tell [ (tabs indent) ++ "- LHSARRAYELEM " ]
  local (+ 1) (showArrayElem elem)



showAssignRHS :: (Annotated AssignRHS TypeA) -> PrintAST ()
showAssignRHS (_, RHSExpr e) = showExpr e
showAssignRHS (_, RHSArrayLit es) = do
  indent <- ask  
  tell [ (tabs indent) ++ "- ARRAYLIT" ]
  mapM_ showExpr es 
showAssignRHS (_, RHSNewPair e1 e2) = do
  indent <- ask
  tell [ (tabs indent) ++ "- NEWPAIR" ]
  local (+ 1) (showExpr e1)
  local (+ 1) (showExpr e2)
  return ()

showAssignRHS (_, RHSPairElem e) = showPairElem e
showAssignRHS (_, RHSCall id es) = do
  indent <- ask
  tell [ (tabs indent) ++ "- CALL" ++ show id]
  mapM_ (\e -> local (+ 1) (showExpr e)) es
  return ()

showArrayElem :: (Annotated ArrayElem TypeA) -> PrintAST ()
showArrayElem (_, ArrayElem id es) = do
  indent <- ask
  tell [ (tabs indent) ++ "- ARRAYELEM" ]
  mapM_ (\e -> local (+ 1) (showExpr e)) es
showPairElem :: (Annotated PairElem TypeA) -> PrintAST ()
showPairElem (_, PairElem side e) = do
  indent <- ask
  tell [ (tabs indent) ++ "- PAIRELEM" ]
  tell [ (tabs (indent + 1)) ++ "- " ++ show side ]
  local (+ 2) (showExpr e)

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


