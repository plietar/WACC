{-# LANGUAGE TypeFamilies #-}

module Frontend.Parser (waccParser) where

import Common.AST
import Common.Span
import Common.WACCResult
import Frontend.Tokens

import Control.Applicative
import Data.Int
import Text.Parsec.Combinator
import Text.Parsec.Expr
import qualified Text.Parsec.Prim as P
import Text.Parsec.Pos
import Text.Parsec.Prim ((<?>))
import Text.Parsec.String ()

infixl 4 $>
($>) :: Functor f => f a -> b -> f b
($>) = flip (<$)

type Parser = P.Parsec [(Pos, Token)] ()

showTok :: (Pos, Token) -> String
showTok (_, tok) = show tok

posTok :: (Pos, Token) -> SourcePos
posTok ((line, column,fname), _) = newPos fname line column

position :: Parser Pos
position = do
  p <- P.getPosition
  return (sourceLine p, sourceColumn p, sourceName p)

spanned :: Parser a -> Parser (Span, a)
spanned p = do
  initialPos <- position
  node <- p
  finalPos <- position
  return ((initialPos, finalPos), node)

wrapSpan :: (Annotated a SpanA -> b SpanA) -> Annotated a SpanA -> Annotated b SpanA
wrapSpan f x@(sp, _) = (sp, f x)

identifier :: Parser Identifier
identifier = P.token showTok posTok matchTok <?> "identifier"
  where
    matchTok (_, TokIdent ident) = Just ident
    matchTok _                   = Nothing

token :: Token -> Parser Token
token expected = P.token showTok posTok matchTok <?> show expected
  where
    matchTok (_, actual)
      | actual == expected = Just actual
    matchTok _
      = Nothing

keyword :: String -> Parser Token
keyword expected = token (TokKeyword expected)

op :: String -> Parser Token
op expected = token (TokOp expected)

literal :: Parser Literal
literal = (lit >>= check) <?> "literal"
  where
    lit = P.token showTok posTok matchTok
    matchTok (_, TokIntLit l)       = Just (LitInt l)
    matchTok (_, TokBoolLit l)      = Just (LitBool l)
    matchTok (_, TokCharLit l)      = Just (LitChar l)
    matchTok (_, TokStrLit l)       = Just (LitString l)
    matchTok (_, TokKeyword "null") = Just LitNull
    matchTok _                      = Nothing

    check :: Literal -> Parser Literal
    check x@(LitInt l) = if l > toInteger( maxBound :: Int32 )
                         then (fail "Integer litteral too large")
                         else return x
    check x = return x

semi :: Parser Token
semi = token TokSemiColon
colon :: Parser Token
colon = token TokColon
comma :: Parser Token
comma = token TokComma
equal :: Parser Token
equal = token TokEqual

parens :: Parser p -> Parser p
parens = between (token TokLParen) (token TokRParen)

brackets :: Parser p -> Parser p
brackets = between (token TokLBracket) (token TokRBracket)

expr :: Parser (Annotated Expr SpanA)
expr = buildExpressionParser exprTable term <?> "expression"
  where
    term = parens expr
           <|> spanned (ExprLit <$> literal)
           <|> spanned (ExprArrayElem <$> arrayElem)
           <|> spanned (ExprVar <$> identifier)
           <?> "term"

    parsePrefix p typ = do
      (span, _) <- spanned p
      return (\x -> (span, ExprUnOp typ x))

    parseBinary p typ = do
      (span, _) <- spanned p
      return (\x y -> (span, ExprBinOp typ x y))

    prefix   p typ       = Prefix (parsePrefix p typ)
    binary   p typ assoc = Infix (parseBinary p typ) assoc

    left     p typ       = binary p typ AssocLeft
    nonassoc p typ       = binary p typ AssocNone

    exprTable = [ [ prefix (op "!") UnOpNot, prefix (op "-") UnOpNeg
                  , prefix (keyword "len") UnOpLen, prefix (keyword "ord") UnOpOrd
                  , prefix (keyword "chr") UnOpChr ]
                , [ left (op "*") BinOpMul, left (op "/") BinOpDiv, left (op "%") BinOpRem]
                , [ left (op "+") BinOpAdd, left (op "-") BinOpSub ]
                , [ nonassoc (op ">")  BinOpGT ]
                , [ nonassoc (op ">=") BinOpGE ]
                , [ nonassoc (op "<")  BinOpLT ]
                , [ nonassoc (op "<=") BinOpLE ]
                , [ nonassoc (op "==") BinOpEQ, nonassoc (op "!=") BinOpNE ]
                , [ left (op "&&") BinOpAnd ]
                , [ left (op "||") BinOpOr ]
                ]

parseType :: Parser Type
parseType = (do
  t <- notArrayType
  arr <- chainl (token TokLBracket *> token TokRBracket $> TyArray) (return (.)) id
  return (arr t)
  ) <?> "type"
  where
    notArrayType = baseType <|> pairType
    baseType = (keyword "int"    $> TyInt) <|>
               (keyword "bool"   $> TyBool) <|>
               (keyword "char"   $> TyChar) <|>
               (keyword "string" $> TyArray TyChar)
    pairType = do
      _ <- P.try (keyword "pair" <* P.lookAhead (token TokLParen))
      parens (TyPair <$> pairElemType <* comma <*> pairElemType)
    pairElemType = parseType <|> (keyword "pair" $> TyPair TyAny TyAny)

voidType :: Parser Type
voidType = keyword "void" $> TyVoid

pairElem :: Parser (Annotated PairElem SpanA)
pairElem = spanned $ do
  side <- pairSide
  e <- expr
  return (PairElem side e)
  where
    pairSide = (keyword "fst" $> PairFst) <|>
               (keyword "snd" $> PairSnd)

arrayElem :: Parser (Annotated ArrayElem SpanA)
arrayElem = spanned $ do
  i <- P.try (identifier <* P.lookAhead (token TokLBracket))
  a <- many1 arrayIndex
  return (ArrayElem i a)
  where
    arrayIndex = brackets expr

assignLHS :: Parser (Annotated AssignLHS SpanA)
assignLHS
  = spanned $ LHSArrayElem <$> arrayElem <|>
              LHSPairElem <$> pairElem <|>
              LHSVar <$> identifier

assignRHS :: Parser (Annotated AssignRHS SpanA)
assignRHS
  = spanned $ rhsExpr <|>
              rhsArrayLit <|>
              rhsPairElem <|>
              rhsCall <|>
              rhsAwait <|>
              rhsNewPair
    where
      rhsExpr     = RHSExpr <$> expr
      rhsArrayLit = RHSArrayLit <$> brackets (sepBy expr comma) <?> "array literal"
      rhsPairElem = RHSPairElem <$> pairElem
      rhsNewPair = keyword "newpair" *> parens (RHSNewPair <$> expr <* comma <*> expr)
      rhsCall = do
        _  <- keyword "call"
        i  <- identifier
        es <- parens (sepBy expr comma)
        return (RHSCall i es)
      rhsAwait = do
        keyword "await"
        name <- identifier
        args <- parens (sepBy expr comma)
        return (RHSAwait name args)

skipStmt :: Parser (Annotated Stmt SpanA)
skipStmt = spanned $ do
  keyword "skip"
  return StmtSkip

letStmt :: Parser (Annotated Stmt SpanA)
letStmt = spanned $ do
  keyword "let"
  i <- identifier
  t <- (colon >> parseType) <|> return TyAny
  token TokEqual
  r <- assignRHS
  return (StmtVar t i r)

varStmt :: Parser (Annotated Stmt SpanA)
varStmt = spanned $ do
  t <- parseType
  i <- identifier
  _ <- token TokEqual
  r <- assignRHS
  return (StmtVar t i r)

whileStmt :: Parser (Annotated Stmt SpanA)
whileStmt = spanned $ do
  _ <- keyword "while"
  e <- expr
  _ <- keyword "do"
  b <- block
  _ <- keyword "done"
  return (StmtWhile e b)

ifStmt :: Parser (Annotated Stmt SpanA)
ifStmt = spanned $ do
  _ <- keyword "if"
  i <- expr
  _ <- keyword "then"
  t <- block
  _ <- keyword "else"
  e <- block
  _ <- keyword "fi"
  return (StmtIf i t e)

scopeStmt :: Parser (Annotated Stmt SpanA)
scopeStmt = spanned $ do
  _ <- keyword "begin"
  b <- block
  _ <- keyword "end"
  return (StmtScope b)

printStmt :: Parser (Annotated Stmt SpanA)
printStmt = spanned $ do
  _ <- keyword "print"
  e <- expr
  return (StmtPrint e False)

printlnStmt :: Parser (Annotated Stmt SpanA)
printlnStmt = spanned $ do
  _ <- keyword "println"
  e <- expr
  return (StmtPrint e True)

assignStmt :: Parser (Annotated Stmt SpanA)
assignStmt = spanned $ do
  l <- assignLHS
  _ <- token TokEqual
  r <- assignRHS
  return (StmtAssign l r)

readStmt :: Parser (Annotated Stmt SpanA)
readStmt = spanned $ do
  _ <- keyword "read"
  l <- assignLHS
  return (StmtRead l)

freeStmt :: Parser (Annotated Stmt SpanA)
freeStmt = spanned $ do
  _ <- keyword "free"
  e <- expr
  return (StmtFree e)

exitStmt :: Parser (Annotated Stmt SpanA)
exitStmt = spanned $ do
  _ <- keyword "exit"
  e <- expr
  return (StmtExit e)

returnStmt :: Parser (Annotated Stmt SpanA)
returnStmt = spanned $ do
  _ <- keyword "return"
  e <- expr
  return (StmtReturn e)

callStmt :: Parser (Annotated Stmt SpanA)
callStmt = spanned $ do
  _ <- keyword "call"
  name <- identifier
  args <- parens (sepBy expr comma)
  return (StmtCall name args)

awaitStmt :: Parser (Annotated Stmt SpanA)
awaitStmt = spanned $ do
  _ <- keyword "await"
  name <- identifier
  args <- parens (sepBy expr comma)
  return (StmtAwait name args)

fireStmt :: Parser (Annotated Stmt SpanA)
fireStmt = spanned $ do
  _ <- keyword "fire"
  name <- identifier
  args <- parens (sepBy expr comma)
  return (StmtFire name args)

stmt :: Parser (Annotated Stmt SpanA)
stmt = skipStmt    <|>
       whileStmt   <|>
       ifStmt      <|>
       scopeStmt   <|>
       printStmt   <|>
       printlnStmt <|>
       assignStmt  <|>
       readStmt    <|>
       freeStmt    <|>
       exitStmt    <|>
       returnStmt  <|>
       letStmt     <|>
       varStmt     <|>
       callStmt    <|>
       awaitStmt   <|>
       fireStmt    <?> "statement"

block :: Parser (Annotated Block SpanA)
block = spanned $ Block <$> sepBy1 stmt semi

param :: Parser (Type, Identifier)
param = (,) <$> parseType <*> identifier

function :: Parser (Annotated FuncDef SpanA)
function = spanned $ do
  (returnType, async, functionName) <- P.try $ do
    a <- option False (keyword "async" >> return True)
    t <- parseType <|> voidType
    i <- identifier
    lookAhead (token TokLParen)
    return (t, a, i)

  args <- parens (sepBy param comma)
  _ <- keyword "is"
  body <- block
  _ <- keyword "end"
  return (FuncDef returnType async (FuncName functionName) args body)

newFunction :: Parser (Annotated Decl SpanA)
newFunction = wrapSpan DeclFFIFunc <$> (spanned $ do
  keyword "func"
  keyword "ffi"
  async <- option False (keyword "async" >> return True)
  functionName <- identifier
  args <- parens (sepBy parseType comma)
  returnType <- (colon >> (parseType <|> voidType)) <|> return TyVoid
  symbolName <- (equal >> identifier) <|> return functionName
  semi

  return (FFIFunc returnType async functionName args symbolName))

decl :: Parser (Annotated Decl SpanA)
decl = (wrapSpan DeclFuncDef <$> function) <|>
       newFunction

mainFunc :: Parser (Annotated FuncDef SpanA)
mainFunc = spanned $ do
  b <- block
  return (FuncDef TyVoid True MainFunc [] b)

program :: Parser (Annotated Program SpanA)
program = spanned $ do
  _ <- keyword "begin"
  f <- many decl
  m <- mainFunc
  _ <- keyword "end"
  return (Program ((wrapSpan DeclFuncDef m):f))

waccParser :: String -> [(Pos, Token)] -> WACCResult (Annotated Program SpanA)
waccParser fname tokens
  = case P.runParser (program <* eof) () fname tokens of
      Left e -> syntaxError (show e)
      Right p -> OK p

