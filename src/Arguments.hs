{-# LANGUAGE CPP #-}

module Arguments where

import Options.Applicative

data OutputType = OutputTokens
                | OutputAST
                | OutputTypedAST
                | OutputIR
                | OutputCFG
                | OutputRIG
                | OutputColouring
                | OutputIRAlloc
                | OutputCFGColoured
                | OutputASM
#if WITH_GRAPHVIZ
                | OutputDotColouring
                | OutputDotCFG
                | OutputDotRIG
#endif
                deriving Show

data Arguments = Arguments
                 { sourceFile :: String
                 , outputType :: OutputType }
                 deriving Show

argumentList = Arguments
               <$> argument str (metavar "INPUT")
               <*> (flag' OutputTokens (long "tokens") 
                    <|> flag' OutputAST (long "ast") 
                    <|> flag' OutputTypedAST (long "types") 
                    <|> flag' OutputIR (long "ir") 
                    <|> flag' OutputCFG (long "cfg") 
                    <|> flag' OutputRIG (long "rig") 
                    <|> flag' OutputColouring (long "colouring")
                    <|> flag' OutputIRAlloc (long "allocir")
                    <|> flag' OutputCFGColoured (long "cfgcoloured")
                    <|> flag' OutputASM (long "asm")
#if WITH_GRAPHVIZ
                    <|> flag' OutputDotCFG (long "dot-cfg") 
                    <|> flag' OutputDotRIG (long "dot-rig") 
                    <|> flag' OutputDotColouring (long "dot-colouring")
#endif
                    <|> pure OutputASM)

argumentInfo = info (helper <*> argumentList) fullDesc

waccArguments :: IO Arguments
waccArguments = execParser argumentInfo

