module Arguments where

import Options.Applicative

data OutputType = OutputTokens
                | OutputAST
                | OutputTypedAST
                | OutputIR
                | OutputCFG
                | OutputRIG
                | OutputColouring
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
                    <|> pure OutputIR)

argumentInfo = info (helper <*> argumentList) fullDesc

waccArguments :: IO Arguments
waccArguments = execParser argumentInfo

