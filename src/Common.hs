{-# LANGUAGE FlexibleInstances #-}

module Common where

import Control.Applicative

import Control.Monad.State
import Control.Monad.Trans

data ErrorKind = LexicalError
               | SyntaxError
               | SemanticError
               | RuntimeError
               deriving (Show)

data WACCResult a = OK a
                  | Error ErrorKind [String]
                  deriving (Show)

instance Functor WACCResult where
  fmap f (OK value)       = OK (f value)
  fmap _ (Error kind msg) = Error kind msg

instance Applicative WACCResult where
  pure value = OK value

  (OK f)           <*> (OK value)       = OK (f value)
  (OK _)           <*> (Error kind msg) = Error kind msg
  (Error kind msg) <*> _                = Error kind msg

instance Monad WACCResult where
  return = pure

  (OK value)       >>= f = f value
  (Error kind msg) >>= _ = Error kind msg

type Pos = (Int, Int, String)

class ErrorContext m where
  withErrorContext :: String -> m -> m

instance ErrorContext (WACCResult a) where
  withErrorContext msg (OK value)        = (OK value)
  withErrorContext msg (Error kind msgs) = (Error kind (msg : msgs))

instance ErrorContext (StateT s WACCResult a) where
  withErrorContext msg = mapStateT (withErrorContext msg)


lexicalError :: String -> WACCResult a
lexicalError e = Error LexicalError [e]
syntaxError :: String -> WACCResult a
syntaxError e = Error SyntaxError [e]
semanticError :: String -> WACCResult a
semanticError e = Error SemanticError [e]


