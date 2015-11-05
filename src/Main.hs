module Main where
import Parser
import Lexer
import System.Environment

main = do
  filename <- fmap head getArgs
  contents <- readFile filename
  print (waccParser (waccLexer contents))
