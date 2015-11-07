module Main where
import Parser
import Lexer
import Common

import System.Environment
import System.Exit
import SemCheck
import AST
import Data.Map

exitCodeForResult :: WACCResult a -> ExitCode
exitCodeForResult (OK _)                  = ExitSuccess
exitCodeForResult (Error LexicalError  _) = ExitFailure 100
exitCodeForResult (Error SyntaxError   _) = ExitFailure 100
exitCodeForResult (Error SemanticError _) = ExitFailure 200

compile :: String -> WACCResult ()
compile source = do
  tokens <- waccLexer source
  ast <- waccParser tokens
  return ()

main = do
  filename <- fmap head getArgs
  contents <- readFile filename
--Testing
  let table = Scope (fromList [("z",TyBool),("x",TyInt),("y",TyArray(TyArray TyInt))]) Root
  print $ waccLexer contents
  print $ waccParseExpr =<< waccLexer contents
  print $ (\e -> typeCheckExpr e table) =<< waccParseExpr =<< waccLexer contents
--Testing
  let result = compile contents
  case result of
    OK _           -> putStrLn "Success !"
    Error kind msg -> putStrLn ("Error " ++ show kind ++ " " ++ msg)
  exitWith (exitCodeForResult result)
