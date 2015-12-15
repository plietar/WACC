{-# LANGUAGE CPP, GeneralizedNewtypeDeriving, StandaloneDeriving #-}

module Arguments where

import Options.Applicative
import Control.Monad.Reader
import Control.Monad.Identity
import Common.WACCResult

data OutputType = OutputTokens
                | OutputAST
                | OutputTypedAST
                | OutputIR
                | OutputCFG
                | OutputRIG
                | OutputColouring
                | OutputIRFinal
                | OutputASM
#if WITH_GRAPHVIZ
                | OutputDotColouring
                | OutputDotCFG
                | OutputDotRIG
#endif
                deriving Show

data Arguments = Arguments
                 { outputType :: OutputType
                 , runtimeEnabled :: Bool
                 , outputFile :: Maybe String
                 , sourceFile :: String
                 }
                 deriving Show

argumentList = Arguments
               <$> (flag' OutputTokens (long "tokens")
                    <|> flag' OutputAST (long "ast")
                    <|> flag' OutputTypedAST (long "types")
                    <|> flag' OutputIR (long "ir")
                    <|> flag' OutputCFG (long "cfg")
                    <|> flag' OutputRIG (long "rig")
                    <|> flag' OutputColouring (long "colouring")
                    <|> flag' OutputIRFinal (long "ir-final")
                    <|> flag' OutputASM (long "asm")
#if WITH_GRAPHVIZ
                    <|> flag' OutputDotCFG (long "dot-cfg")
                    <|> flag' OutputDotRIG (long "dot-rig")
                    <|> flag' OutputDotColouring (long "dot-colouring")
#endif
                    <|> pure OutputASM)
               <*> switch (long "with-runtime")
               <*> (optional (strOption (short 'o' <> metavar "OUTPUT")))
               <*> argument str (metavar "INPUT")

argumentInfo = info (helper <*> argumentList) fullDesc

newtype WACCArgumentsT m a = WACCArgumentsT (ReaderT Arguments m a)
                             deriving (MonadTrans)
type WACCArguments = WACCArgumentsT Identity

deriving instance Functor     m => Functor     (WACCArgumentsT m)
deriving instance Alternative m => Alternative (WACCArgumentsT m)
deriving instance Applicative m => Applicative (WACCArgumentsT m)
deriving instance Monad       m => Monad       (WACCArgumentsT m)
deriving instance MonadPlus   m => MonadPlus   (WACCArgumentsT m)
deriving instance MonadFix    m => MonadFix    (WACCArgumentsT m)
deriving instance MonadIO     m => MonadIO     (WACCArgumentsT m)

getArgument :: Monad m => (Arguments -> a) -> WACCArgumentsT m a
getArgument = WACCArgumentsT . asks

withArguments :: WACCArgumentsT IO a -> IO a
withArguments (WACCArgumentsT m) = do
  args <- execParser argumentInfo
  runReaderT m args

mapArgumentsT :: (m a -> n b) -> WACCArgumentsT m a -> WACCArgumentsT n b
mapArgumentsT f (WACCArgumentsT m) = WACCArgumentsT $ mapReaderT f m

liftArguments :: Monad m => WACCArguments a -> WACCArgumentsT m a
liftArguments = mapArgumentsT (return . runIdentity)

fmapM :: (a -> WACCArguments b) -> WACCResult a -> WACCArguments (WACCResult b)
fmapM f (OK value)  = let (WACCArgumentsT r) = f value
                      in WACCArgumentsT $ do
                          args <- ask
                          (return . OK . runReader r) args 
fmapM f (Error k m) = return (Error k m)

