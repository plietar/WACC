module Common where

data ErrorKind = LexicalError
               | SyntaxError
               | SemanticError
               deriving (Show)

data WACCResult a = OK a
                  | Error ErrorKind

instance Functor WACCResult where
  fmap f (OK value)   = OK (f value)
  fmap _ (Error kind) = Error kind

instance Applicative WACCResult where
  pure value = OK value

  (OK f)       <*> (OK value) = OK (f value)
  (Error kind) <*> _          = Error kind

instance Monad WACCResult where
  return = pure

  (OK value)   >>= f = f value
  (Error kind) >>= _ = Error kind

