{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, StandaloneDeriving #-}

module Common.WACCResult where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Trans

data ErrorKind = LexicalError
               | SyntaxError
               | SemanticError
               | CodeGenError
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

class Monad m => MonadResult m where
  withErrorContext :: String -> m a -> m a
  lexicalError  :: String -> m a
  syntaxError   :: String -> m a
  semanticError :: String -> m a
  codegenError  :: String -> m a

instance MonadResult WACCResult where
  withErrorContext msg (OK value)        = (OK value)
  withErrorContext msg (Error kind msgs) = (Error kind (msg : msgs))

  lexicalError  e = Error LexicalError  [e]
  syntaxError   e = Error SyntaxError   [e]
  semanticError e = Error SemanticError [e]
  codegenError  e = Error CodeGenError  [e]

instance Monad m => MonadResult (WACCResultT m) where
  withErrorContext msg m = WACCResultT $ do
    a <- runWACCResultT m
    return (withErrorContext msg a)

  lexicalError  = WACCResultT . return . syntaxError
  syntaxError   = WACCResultT . return . syntaxError
  semanticError = WACCResultT . return . semanticError
  codegenError  = WACCResultT . return . syntaxError

instance MonadResult m => MonadResult (StateT s m) where
  withErrorContext msg = mapStateT (withErrorContext msg)
  lexicalError  = lift . syntaxError
  syntaxError   = lift . syntaxError
  semanticError = lift . semanticError
  codegenError  = lift . syntaxError

instance MonadResult m => MonadResult (ReaderT s m) where
  withErrorContext msg = mapReaderT (withErrorContext msg)
  lexicalError  = lift . syntaxError
  syntaxError   = lift . syntaxError
  semanticError = lift . semanticError
  codegenError  = lift . syntaxError

newtype WACCResultT m a = WACCResultT { runWACCResultT :: m (WACCResult a) }

instance Functor m => Functor (WACCResultT m) where
  fmap f = WACCResultT . fmap (fmap f) . runWACCResultT

instance Monad m => Applicative (WACCResultT m) where
  pure    = WACCResultT . pure . pure
  f <*> a = WACCResultT $ do
            f' <- runWACCResultT f
            a' <- runWACCResultT a
            return (f' <*> a')

instance Monad m => Monad (WACCResultT m) where
  return  = pure
  m >>= k = WACCResultT $ do
            a <- runWACCResultT m
            case a of
              OK x -> runWACCResultT (k x)
              Error k e -> return (Error k e)

instance MonadTrans WACCResultT where
  lift m = WACCResultT $ OK <$> m

waccResultT :: Monad m => WACCResult a -> WACCResultT m a
waccResultT = WACCResultT . return
