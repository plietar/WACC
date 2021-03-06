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

tabbedInstruction :: String -> String
tabbedInstruction inst
  | isSuffixOf ":" inst = "\t"   ++ inst
  | otherwise         = "\t\t" ++ inst

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
  forM_ fs (\f -> local (+ 1) (showDecl f))
  return ()

showDecl :: (Annotated Decl TypeA) -> PrintAST ()
showDecl (_, DeclFunc f) = showFuncDef f
showDecl (_, DeclFFIFunc f) = tell [show f]
showDecl (_, DeclType t) = tell [show t]

showFuncDef :: (Annotated FuncDef TypeA) -> PrintAST ()
showFuncDef (_, FuncDef t _ n args block) = do
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
showStmt (_, StmtSkip)
  = return ()
showStmt (_, StmtVar t id assignrhs) = do
  indent <- ask
  tell [ (tabs indent) ++ "- "  ++ " StmtVar " ++ (show t)  ++ " " ++ (show id) ]
  local (+ 1) (showAssignRHS assignrhs)

showStmt (_, StmtAssign lhs rhs) = do
  indent <- ask
  tell [ (tabs indent) ++ "- StmtAssign "]
  local (+ 1) (showAssignRHS rhs)
  local (+ 1) (showAssignLHS lhs)

showStmt (_, StmtRead lhs) = do
  indent <- ask
  tell [ (tabs indent) ++ "- StmtRead " ]
  local (+ 1) (showAssignLHS lhs)

showStmt (_, StmtFree e) = do
  indent <- ask
  tell [ (tabs indent) ++ "- StmtFree " ]
  local (+ 1) (showExpr e)

showStmt (_, StmtReturn e) = do
  indent <- ask
  tell [ (tabs indent) ++ "- StmtReturn" ]
  local (+ 1) (showExpr e)

showStmt (_, StmtExit e) = do
  indent <- ask
  tell [ (tabs indent) ++ "- StmtExti" ]
  local (+ 1) (showExpr e)

showStmt (_, StmtPrint exprs b) = do
  indent <- ask
  tell [ (tabs indent) ++ "- StmtPrint" ++ line ]
  local (+ 1) (mapM_ showExpr exprs)
  where
    line = if b then "ln" else ""

showStmt (_, StmtIf cond b1 maybeB2) = do
  indent <- ask
  tell [ (tabs indent) ++ "- StmtIf" ]
  tell [ (tabs (indent + 1)) ++ "- COND" ]
  local (+ 2) (showExpr cond)
  tell [ (tabs (indent + 1)) ++ "- THEN" ]
  local (+ 2) (showBlock b1)
  case maybeB2 of
    Just b2 -> do
      tell [ (tabs (indent + 1)) ++ "- ELSE" ]
      local (+ 2) (showBlock b2)
    Nothing -> return ()

showStmt (_, StmtWhile cond b) = do
  indent <- ask
  tell [ (tabs indent) ++ "- StmtWhile" ]
  tell [ (tabs (indent + 1)) ++ "- COND" ]
  local (+ 2) (showExpr cond)
  tell [ (tabs (indent + 1)) ++ "- BODY" ]
  local (+ 2) (showBlock b)

showStmt (_, StmtScope b) = do
  indent <- ask
  tell [ (tabs indent) ++ "- StmtScope"]
  local (+ 1) (showBlock b)

showStmt (_, s) = tell [show s]

showBlock :: (Annotated Block TypeA) -> PrintAST ()
showBlock ((b, xs), Block stmts) = do
  indent <- ask
  tell [ (tabs indent) ++ "Block -- Bool: " ++ show b ++ ", types: "
        ++ (concatMap show xs)]
  mapM_ showStmt stmts

showExpr :: (Annotated Expr TypeA) -> PrintAST ()
showExpr (_, ExprLit l) = do
  indent <- ask
  tell [ (tabs indent) ++ "- ExprLit " ++ show l ]

showExpr (_, ExprVar v) = do
  indent <- ask
  tell [ (tabs indent) ++ "- ExprVar " ++ show v ]

showExpr (_, ExprIndexingElem elem) = showIndexingElem elem

showExpr (_, ExprUnOp op e) = do
  indent <- ask
  tell [ (tabs indent) ++ "- ExprUnOp " ++ show op]
  local (+ 1) (showExpr e)

showExpr (_, ExprBinOp op e1 e2) = do
  indent <- ask
  tell [ (tabs indent) ++ "- ExprBinOp " ++ show op ]
  local (+ 1) (showExpr e1)
  local (+ 1) (showExpr e2)

showAssignLHS :: (Annotated AssignLHS TypeA) -> PrintAST ()
showAssignLHS (_, LHSVar id) = do
  indent <- ask
  tell [ (tabs indent) ++ "- AssignLHSVar " ++ show id ]

showAssignLHS (_, LHSIndexingElem elem) = do
  indent <- ask
  tell [ (tabs indent) ++ "- AssignLhsIndexingElem" ]
  local (+ 1) (showIndexingElem elem)


showAssignRHS :: (Annotated AssignRHS TypeA) -> PrintAST ()
showAssignRHS (_, RHSExpr e) = do
  indent <- ask
  tell [ (tabs indent) ++ "- AssignRhsExpr" ]
  local (+ 1) (showExpr e)
showAssignRHS (_, RHSArrayLit es) = do
  indent <- ask
  tell [ (tabs indent) ++ "- AssignRhsArrayLit" ]
  mapM_ showExpr es

showAssignRHS (_, RHSNewTuple es) = do
  indent <- ask
  tell [ (tabs indent) ++ "- AssignRhsNewTuple" ]
  mapM_ (\e -> local (+ 1) (showExpr e)) es

showAssignRHS (_, RHSCall id es) = do
  indent <- ask
  tell [ (tabs indent) ++ "- AssignRhsCall" ++ show id]
  mapM_ (\e -> local (+ 1) (showExpr e)) es

showIndexingElem :: (Annotated IndexingElem TypeA) -> PrintAST ()
showIndexingElem (_, IndexingElem id es) = do
  indent <- ask
  tell [ (tabs indent) ++ "- IndexingElem " ++ id  ]
  mapM_ (\e -> local (+ 1) (showExpr e)) es

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
    tell [show v ++ " -> " ++ show col]

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
                               , GraphViz.Layout GraphViz.Circo ]

         , GraphViz.NodeAttrs [ GraphViz.shape GraphViz.Ellipse
                              , GraphViz.fillColor GraphViz.White
                              , GraphViz.style GraphViz.filled ] ]

    fn (_, l) = [(GraphViz.toLabel . show) l]

showDotColouring :: (Graph gr, Eq a, Ord a, Show a) => gr Var () -> Map Var a -> [String]
showDotColouring cfg colouring
  = (:[]) . unpack . GraphViz.printDotGraph . GraphViz.setDirectedness GraphViz.graphToDot (dotColouringParams colouring) $ cfg

dotColouringParams :: (Eq a, Ord a, Show a) => Map Var a -> GraphViz.GraphvizParams Int Var () () Var
dotColouringParams colouring = dotRIGParams { GraphViz.fmtNode = fn }
  where
    fn (_, l) = [(GraphViz.toLabel . show) l
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


