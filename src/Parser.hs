module Parser where

import qualified Text.Parsec.Prim as P
import Text.Parsec.Prim ((<?>))
import Control.Applicative
import Text.Parsec.Combinator
import Text.Parsec.Pos
import Text.Parsec.Expr
import Tokens
import Common
import AST

type Parser = P.Parsec [(Pos, Token)] ()

showTok :: (Pos, Token) -> String
showTok (_, TokLParen) = "("
showTok (_, TokRParen) = ")"
showTok (_, tok) = show tok

posTok :: (Pos, Token) -> SourcePos
posTok ((line, column), _) = newPos "<input>" line column

identifier :: Parser String
identifier = P.token showTok posTok matchTok <?> "identifier"
  where
    matchTok (_, TokIdent ident) = Just ident
    matchTok _                = Nothing

token :: Token -> Parser Token
token expected = P.token showTok posTok matchTok <?> show expected
  where
    matchTok (_, actual)
      | actual == expected = Just actual
    matchTok _
      = Nothing

keyword :: String -> Parser String
keyword expected = P.token showTok posTok matchTok <?> expected
  where
    matchTok (_, TokKeyword actual)
      | actual == expected = Just actual
    matchTok _
      = Nothing

op :: String -> Parser String
op expected = P.token showTok posTok matchTok 
  where
    matchTok (_, TokOp actual)
      | actual == expected = Just actual
    matchTok _
      = Nothing

literal :: Parser Expr
literal = P.token showTok posTok (\t -> ExprLit <$> matchTok t) <?> "literal"
  where
    matchTok (_, TokIntLit l)  = Just (LitInt l)
    matchTok (_, TokBoolLit l) = Just (LitBool l)
    matchTok (_, TokCharLit l) = Just (LitChar l)
    matchTok (_, TokStrLit l)  = Just (LitString l)
    matchTok _                 = Nothing

semi :: Parser Token
semi = token TokSemiColon
comma :: Parser Token
comma = token TokComma
equal :: Parser Token
equal = token TokEqual

parens :: Parser p -> Parser p
parens = between (token TokLParen) (token TokRParen)

expr :: Parser Expr
expr = buildExpressionParser exprTable term <?> "expression"
  where
    term = parens expr <|> literal <?> "term"
    binary   p typ assoc = Infix (do{ _ <- p; return (ExprBinOp typ) }) assoc
    left     p typ       = binary p typ AssocLeft
    nonassoc p typ       = binary p typ AssocNone
    prefix   p typ       = Prefix (do{ _ <- p; return (ExprUnOp typ) })
    exprTable = [ [ prefix (op "!") UnOpNot, prefix (op "-") UnOpNeg
                  , prefix (keyword "len") UnOpLen, prefix (keyword "ord") UnOpOrd
                  , prefix (keyword "chr") UnOpChr ]
                , [ left (op "*") BinOpMul, left (op "/") BinOpDiv, left (op "%") BinOpRem]
                , [ left (op "+") BinOpAdd, left (op "-") BinOpSub ]
                , [ nonassoc (op ">")  BinOpGT ]
                , [ nonassoc (op ">=") BinOpGT ]
                , [ nonassoc (op "<")  BinOpLT ]
                , [ nonassoc (op "<=") BinOpLT ]
                , [ nonassoc (op "==") BinOpEQ, nonassoc (op "!=") BinOpNE ]
                , [ left (op "&&") BinOpAnd ]
                , [ left (op "||") BinOpOr ]
                ]

skipStmt :: Parser Stmt
skipStmt = StmtSkip <$ keyword "skip"

parseType :: Parser Type
parseType = baseType {-<|> arrayType -} <|> pairType <?> "type"
  where
    baseType = (TyInt  <$ keyword "int") <|>
               (TyBool <$ keyword "bool") <|>
               (TyChar <$ keyword "char") <|>
               (TyArray TyChar <$ keyword "string")
    arrayType = parseType <* token TokLBracket <* token TokRBracket
    pairType :: Parser Type
    pairType = keyword "pair" *> parens (TyPair <$> pairElemType <* comma <*> pairElemType)
    pairElemType = baseType <|> arrayType <|>
                   (TyPair TyAny TyAny <$ keyword "pair")

arrayLit :: Parser AssignRHS
arrayLit = do
  _  <- token TokLBracket
  es <- sepBy expr comma
  _  <- token TokRBracket
  return (RHSArrayLit es) 

assignRHS :: Parser AssignRHS
assignRHS = RHSExpr <$> expr <|>
            arrayLit <|>
            RHSPair <$> pairElem   

pairElem :: Parser PairElem
pairElem = do
  side <- (PairFst <$ keyword "fst") <|>
          (PairSnd <$ keyword "snd")
  e <- expr
  return (PairElem side e) 

arrayElem :: Parser ArrayElem
arrayElem = do
  i <- identifier
  a <- many1 arrayIndex
  return (ArrayElem i a)
  where 
    arrayIndex = between (token TokLBracket) (token TokRBracket) expr  

rhsNewPair :: Parser AssignRHS
rhsNewPair = do
  _  <- keyword "newpair"
  _  <- token TokLParen
  e1 <- expr
  _  <- comma
  e2 <- expr
  _  <- token TokRParen
  return (RHSNewPair e1 e2)

rhsCall :: Parser AssignRHS
rhsCall = do
  _  <- keyword "call"  
  i  <- identifier
  _  <- token TokLParen
  es <- sepBy expr comma
  _  <- token TokRParen
  return (RHSCall i es)



assignLHS :: Parser AssignLHS
assignLHS = LHSVar <$> identifier <|> 
            LHSPair <$> pairElem <|> 
            LHSArray <$> arrayElem 

varStmt :: Parser Stmt
varStmt = do 
  t <- parseType
  i <- identifier
  _ <- token TokEqual
  r <- assignRHS
  return (StmtVar t i r)

whileStmt :: Parser Stmt
whileStmt = do
  _ <- keyword "while"
  e <- expr
  _ <- keyword "do"
  b <- block
  _ <- keyword "end"
  return (StmtWhile e b)

ifStmt :: Parser Stmt
ifStmt = do
  _ <- keyword "if"
  i <- expr
  _ <- keyword "then"
  t <- block
  _ <- keyword "else"
  e <- block
  _ <- keyword "fi"
  return (StmtIf i t e)

scopeStmt :: Parser Stmt
scopeStmt = do
  _ <- keyword "begin"
  b <- block
  _ <- keyword "end"
  return (StmtScope b)

printStmt :: Parser Stmt
printStmt = do
  _ <- keyword "print"
  e <- expr
  return (StmtPrint False e)

printlnStmt :: Parser Stmt
printlnStmt = do
  _ <- keyword "println"
  e <- expr
  return (StmtPrint True e)

assignStmt :: Parser Stmt
assignStmt = do
  l <- assignLHS
  _ <- token TokEqual
  r <- assignRHS
  return (StmtAssign l r)

readStmt :: Parser Stmt
readStmt = do
  l <- assignLHS
  return (StmtRead l)

freeStmt :: Parser Stmt
freeStmt = do
  e <- expr
  return (StmtFree e)

exitStmt :: Parser Stmt
exitStmt = do
  e <- expr
  return (StmtExit e)


stmt :: Parser Stmt
stmt = skipStmt  <|>
       whileStmt <|>
       ifStmt    <|>
       scopeStmt <|>
       printStmt <|>
       printlnStmt <|>
       assignStmt  <|>
       readStmt  <|>
       freeStmt  <|>
       exitStmt  <|>
       varStmt   <?> "statement"

block :: Parser [Stmt]
block = sepBy1 stmt semi <?> "block"

program :: Parser Program
program = do
  _ <- keyword "begin"
  b <- block
  _ <- keyword "end"
  return (Program [] b)

waccParser :: [(Pos, Token)] -> WACCResult Program
waccParser tokens
  = case P.runParser program () "<input>" tokens of
      Left e -> Error SyntaxError (show e)
      Right p -> OK p
