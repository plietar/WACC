module Common where

import Control.Applicative

data ErrorKind = LexicalError
               | SyntaxError
               | SemanticError
               deriving (Show)

data WACCResult a = OK a
                  | Error ErrorKind String

instance Functor WACCResult where
  fmap f (OK value)       = OK (f value)
  fmap _ (Error kind msg) = Error kind msg

instance Applicative WACCResult where
  pure value = OK value

  (OK f)           <*> (OK value) = OK (f value)
  (Error kind msg) <*> _          = Error kind msg

instance Monad WACCResult where
  return = pure

  (OK value)       >>= f = f value
  (Error kind msg) >>= _ = Error kind msg

