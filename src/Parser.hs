module Parser where

import qualified Text.Parsec.Prim as P
import Text.Parsec.Prim ((<?>))
import Text.Parsec.String ()
import Control.Applicative
import Text.Parsec.Combinator
import Text.Parsec.Pos
import Text.Parsec.Expr
import Tokens
import Common
import AST

infixl 4 $>
($>) :: Functor f => f a -> b -> f b
($>) = flip (<$)

type Parser = P.Parsec [(Pos, Token)] ()

showTok :: (Pos, Token) -> String
showTok (_, TokLParen) = "("
showTok (_, TokRParen) = ")"
showTok (_, tok) = show tok

posTok :: (Pos, Token) -> SourcePos
posTok ((line, column,fname), _) = newPos fname line column

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

parseNull :: Parser Expr
parseNull = keyword "null" $> ExprNull <?> "null"

semi :: Parser Token
semi = token TokSemiColon
comma :: Parser Token
comma = token TokComma
equal :: Parser Token
equal = token TokEqual

parens :: Parser p -> Parser p
parens = between (token TokLParen) (token TokRParen)

brackets :: Parser p -> Parser p
brackets = between (token TokLBracket) (token TokRBracket)

expr :: Parser Expr
expr = buildExpressionParser exprTable term <?> "expression"
  where
    term = parens expr
           <|> literal
           <|> parseNull
           <|> (ExprArrayElem <$> arrayElem)
           <|> (ExprVar <$> identifier)
           <?> "term"

    binary   p typ assoc = Infix (p $> ExprBinOp typ) assoc
    prefix   p typ       = Prefix (p $> ExprUnOp typ)

    left     p typ       = binary p typ AssocLeft
    nonassoc p typ       = binary p typ AssocNone

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
parseType = buildExpressionParser table term <?> "type"
  where
    table = [[ Postfix (do { _ <- token TokLBracket; _ <- token TokRBracket; return TyArray }) ]]
    term = baseType <|> pairType
    baseType = (keyword "int"    $> TyInt) <|>
               (keyword "bool"   $> TyBool) <|>
               (keyword "char"   $> TyChar) <|>
               (keyword "string" $> TyArray TyChar)
    pairType = do
      _ <- P.try (keyword "pair" >> token TokLParen)
      f <- pairElemType
      _ <- comma
      s <- pairElemType
      token TokRParen
      return (TyPair f s)
    pairElemType = parseType <|> (keyword "pair" $> TyPair TyAny TyAny)

arrayLit :: Parser AssignRHS
arrayLit = do
  _  <- token TokLBracket
  es <- sepBy expr comma
  _  <- token TokRBracket
  return (RHSArrayLit es) 

assignRHS :: Parser AssignRHS
assignRHS = RHSExpr <$> expr <|>
            arrayLit <|>
            RHSPair <$> pairElem <|>
            rhsCall <|>
            rhsNewPair

rhsNewPair :: Parser AssignRHS
rhsNewPair = do
  _ <- keyword "newpair"
  parens (RHSNewPair <$> expr <* comma <*> expr)

pairElem :: Parser PairElem
pairElem = do
  side <- (PairFst <$ keyword "fst") <|>
          (PairSnd <$ keyword "snd")
  e <- expr
  return (PairElem side e) 

arrayElem :: Parser ArrayElem
arrayElem = do
  i <- P.try (identifier <* P.lookAhead (token TokLBracket))
  a <- many1 arrayIndex
  return (ArrayElem i a)
  where 
    arrayIndex = brackets expr  

rhsCall :: Parser AssignRHS
rhsCall = do
  _  <- keyword "call"  
  i  <- identifier
  _  <- token TokLParen
  es <- sepBy expr comma
  _  <- token TokRParen
  return (RHSCall i es)

assignLHS :: Parser AssignLHS
assignLHS = LHSArray <$> arrayElem <|>
            LHSPair <$> pairElem <|>
            LHSVar <$> identifier

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
  _ <- keyword "done"
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
  _ <- keyword "read"
  l <- assignLHS
  return (StmtRead l)

freeStmt :: Parser Stmt
freeStmt = do
  _ <- keyword "free"
  e <- expr
  return (StmtFree e)

exitStmt :: Parser Stmt
exitStmt = do
  _ <- keyword "exit"
  e <- expr
  return (StmtExit e)

returnStmt :: Parser Stmt
returnStmt = do
  _ <- keyword "return"
  e <- expr
  return (StmtReturn e)

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
       returnStmt <|>
       varStmt   <?> "statement"

block :: Parser [Stmt]
block = sepBy1 stmt semi <?> "block"

program :: Parser Program
program = do
  _ <- keyword "begin"
  f <- many function
  b <- block
  _ <- keyword "end"
  return (Program f b)

function :: Parser FuncDef
function = do
  (t,i) <- P.try ((,) <$> parseType <*> identifier <* token TokLParen)
  x <- sepBy param comma
  _ <- token TokRParen
  _ <- keyword "is"
  b <- block
  _ <- keyword "end"
  return (FuncDef t i x b)

param :: Parser (Type, String)
param = (,) <$> parseType <*> identifier

waccParser :: String -> [(Pos, Token)] -> WACCResult Program
waccParser fname tokens
  = case P.runParser program () fname tokens of 
      Left e -> Error SyntaxError (show e)
      Right p -> OK p

