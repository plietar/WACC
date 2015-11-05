module Main where
import Parser
import Lexer
import Common

import System.Environment
import System.Exit

exitCodeForResult :: WACCResult a -> ExitCode
exitCodeForResult (OK _)                = ExitSuccess
exitCodeForResult (Error LexicalError)  = ExitFailure 1
exitCodeForResult (Error SyntaxError)   = ExitFailure 100
exitCodeForResult (Error SemanticError) = ExitFailure 200

compile :: String -> WACCResult ()
compile source = do
  tokens <- return (waccLexer source)
  ast <- waccParser tokens
  return ()

main = do
  filename <- fmap head getArgs
  contents <- readFile filename
  let result = compile contents
  case result of
    OK _       -> putStrLn "Success !"
    Error kind -> putStrLn ("Error " ++ show kind)
  exitWith (exitCodeForResult result)

