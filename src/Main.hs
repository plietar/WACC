module Main where
import Parser
import Lexer

main = do
  s <- getContents
  print (waccParser (waccLexer s))
