module Main where
import Parser
import Lexer
import Common
import ScopedMap

import System.Environment
import System.Exit
import SemCheck
import AST
import Data.Map as Map
import Control.Monad.State

exitCodeForResult :: WACCResult a -> ExitCode
exitCodeForResult (OK _)                  = ExitSuccess
exitCodeForResult (Error LexicalError  _) = ExitFailure 100
exitCodeForResult (Error SyntaxError   _) = ExitFailure 100
exitCodeForResult (Error SemanticError _) = ExitFailure 200

compile :: String -> WACCResult ()
compile source = do
  tokens <- waccLexer source
  ast <- waccParser tokens
  typeCheckProgram ast emptyContext
  return ()

test contents = do
  --let table = Scope (fromList [("z",TyBool),("x",TyInt),("y",TyArray(TyArray TyInt))]) Root
  print (waccLexer contents)
  print (waccLexer contents >>= waccParseStmt)
  print (waccLexer contents >>= waccParseStmt >>= (\ast -> evalStateT (typeCheckStmt ast) context))
  where
    (OK context) = execStateT addTestVariables emptyContext
    addTestVariables = do
      addVariable "z" TyBool
      addVariable "x" TyInt
      addVariable "y" (TyArray (TyArray TyInt))

main = do
  filename <- fmap head getArgs
  contents <- readFile filename
  --test contents

  let result = compile contents
  case result of
    OK _           -> putStrLn "Success !"
    Error kind msg -> putStrLn ("Error " ++ show kind ++ " " ++ msg)
  exitWith (exitCodeForResult result)
