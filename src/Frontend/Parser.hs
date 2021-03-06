{-# LANGUAGE TypeFamilies, LambdaCase #-}

module Frontend.Parser (waccParser) where

import Common.AST
import Common.Span
import Common.Stuff
import Common.WACCResult
import Frontend.Tokens
import Arguments

import Control.Applicative
import Control.Monad.Trans
import Data.Int
import Data.Tuple (swap)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Text.Parsec.Combinator
import Text.Parsec.Expr
import qualified Text.Parsec.Prim as P
import Text.Parsec.Pos
import Text.Parsec.Prim ((<?>))
import Text.Parsec.String ()

type Parser = P.ParsecT [(Pos, Token)] () WACCArguments

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
identifier = P.tokenPrim showTok (\ _ t _ -> posTok t) matchTok <?> "identifier"
  where
    matchTok (_, TokIdent ident) = Just ident
    matchTok _                   = Nothing

token :: Token -> Parser Token
token expected = P.tokenPrim showTok (\ _ t _ -> posTok t) matchTok <?> show expected
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
    lit = P.tokenPrim showTok (\_ t _ -> posTok t) matchTok
    matchTok (_, TokIntLit l)       = Just (LitInt l)
    matchTok (_, TokBoolLit l)      = Just (LitBool l)
    matchTok (_, TokCharLit l)      = Just (LitChar l)
    matchTok (_, TokStrLit l)       = Just (LitString l)
    matchTok (_, TokKeyword "null") = Just LitNull
    matchTok _                      = Nothing

    check :: Literal -> Parser Literal
    check x@(LitInt l) = if l > toInteger( maxBound :: Int32 )
                         then (fail "Integer literal too large")
                         else return x
    check x = return x

dot :: Parser Token
dot = token TokDot
semi :: Parser Token
semi = token TokSemiColon
comma :: Parser Token
comma = token TokComma
equal :: Parser Token
equal = token TokEqual
colon :: Parser Token
colon = token TokColon

parens :: Parser p -> Parser p
parens = between (token TokLParen) (token TokRParen)

brackets :: Parser p -> Parser p
brackets = between (token TokLBracket) (token TokRBracket)

braces :: Parser p -> Parser p
braces = between (token TokLBrace) (token TokRBrace)

expr :: Parser (Annotated Expr SpanA)
expr = buildExpressionParser exprTable term <?> "expression"
  where
    term = parens expr
           <|> spanned (ExprLit <$> literal)
           <|> spanned (ExprIndexingElem <$> indexingElem)
           <|> spanned (ExprStructElem <$> structElem)
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
    notArrayType = baseType <|>
                   pairType <|>
                   tupleType <|>
                   chanType <|>
                   structType <|>
                   unionType

    baseType = (keyword "int"    $> TyInt) <|>
               (keyword "bool"   $> TyBool) <|>
               (keyword "char"   $> TyChar) <|>
               (keyword "string" $> TyArray TyChar) <|>
               (TyName <$> identifier)

    pairType = do
      _ <- P.try (keyword "pair" <* P.lookAhead (token TokLParen))
      parens (TyTuple <$> (sepBy pairElemType comma))
    pairElemType = parseType <|> (keyword "pair" $> TyTuple [TyAny, TyAny])
    tupleType = do
      _ <- P.try (keyword "tuple")
      parens (TyTuple <$> (sepBy parseType comma))

    chanType = keyword "chan" >> (TyChan <$> parens parseType)

    structType = do
      -- FIXME(paul): Check for duplicated fields
      innerTypes <- braces (sepEndBy (swap <$> ((,) <$> parseType <*> identifier)) comma)
      return (TyStruct (Map.fromList innerTypes))

    unionType = (TyUnion . Set.fromList) <$> (parens $ do
      fstTy <- nullOrType
      op "|"
      rest <- sepBy1 nullOrType (op "|")
      return (fstTy : rest))

voidType :: Parser Type
voidType = keyword "void" $> TyVoid

nullType :: Parser Type
nullType = keyword "null" $> TyNull

nullOrType :: Parser Type
nullOrType = nullType <|> parseType

voidOrType :: Parser Type
voidOrType = voidType <|> parseType

pairElem :: Parser (Annotated IndexingElem SpanA)
pairElem = spanned $ do
  index <- spanned $ ExprLit <$> LitInt <$> ((keyword "fst" $> 0) <|> (keyword "snd" $> 1))
  i    <- identifier
  return (IndexingElem i [index])

indexingElem :: Parser (Annotated IndexingElem SpanA)
indexingElem = spanned $ do
  i <- P.try (identifier <* P.lookAhead (token TokLBracket))
  a <- many1 index
  return (IndexingElem i a)
  where
    index = brackets expr

structElem :: Parser (Annotated StructElem SpanA)
structElem = spanned $ do
  name <- P.try (identifier <* P.lookAhead dot)
  member <- many1 (dot *> identifier)
  return (StructElem name member)

assignLHS :: Parser (Annotated AssignLHS SpanA)
assignLHS
  = spanned $ LHSIndexingElem <$> indexingElem <|>
              LHSIndexingElem <$> pairElem <|>
              LHSStructElem   <$> structElem <|>
              LHSVar <$> identifier

assignRHS :: Parser (Annotated AssignRHS SpanA)
assignRHS
  = spanned $ rhsExpr <|>
              rhsArrayLit <|>
              rhsStructLit <|>
              rhsPairElem <|>
              rhsCall <|>
              rhsAwait <|>
              rhsNewPair <|>
              rhsNewTuple <|>
              rhsNewChan <|>
              rhsChanRecv
    where
      rhsExpr     = RHSExpr <$> expr
      rhsArrayLit = RHSArrayLit <$> brackets (sepBy expr comma) <?> "array literal"
      rhsPairElem = RHSExpr <$> wrapSpan ExprIndexingElem <$> pairElem
      rhsNewPair  = keyword "newpair" *> parens (RHSNewTuple <$> ((\a b -> [a,b]) <$> expr <* comma <*> expr))
      rhsNewTuple = keyword "newtuple" *> parens (RHSNewTuple <$> (sepBy expr comma))

      rhsStructLit = do
        -- FIXME(paul): Check for duplicated fields
        values <- braces (sepEndBy ((,) <$> identifier <* equal <*> expr) comma)
        return (RHSStructLit (Map.fromList values))

      rhsCall = do
        keyword "call"
        name  <- identifier
        args <- parens (sepBy expr comma)
        return (RHSCall name args)

      rhsAwait = do
        keyword "await"
        name <- identifier
        args <- parens (sepBy expr comma)
        return (RHSAwait name args)

      rhsNewChan = do
        keyword "chan"
        parens (return ())
        return RHSNewChan

      rhsChanRecv = do
        op "<-"
        name <- identifier
        return (RHSChanRecv name)

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
  (varType, varName) <- P.try $ do
    t <- parseType
    i <- identifier
    _ <- token TokEqual
    return (t,i)
  rhs <- assignRHS
  return (StmtVar varType varName rhs)

whileStmt :: Parser (Annotated Stmt SpanA)
whileStmt = spanned $ do
  _ <- keyword "while"
  e <- expr
  _ <- keyword "do"
  b <- block
  _ <- keyword "done"
  return (StmtWhile e b)

forStmt :: Parser (Annotated Stmt SpanA)
forStmt = spanned $ do
  _ <- keyword "for"
  i <- identifier
  _ <- keyword "in"
  c <- expr
  _ <- keyword "do"
  b <- block
  _ <- keyword "done"
  return (StmtFor i c b)

ifStmt :: Parser (Annotated Stmt SpanA)
ifStmt = spanned $ do
  _ <- keyword "if"
  i <- expr
  _ <- keyword "then"
  t <- block
  e <- optionMaybe (keyword "else" >> block)
  _ <- keyword "fi"
  return (StmtIf i t e)

switchStmt :: Parser (Annotated Stmt SpanA)
switchStmt = spanned $ do
  _ <- keyword "switch"
  e <- expr
  cs <- many1 caseArm
  _ <- keyword "end"
  return (StmtSwitch e cs)

caseArm :: Parser (Annotated CaseArm SpanA)
caseArm = spanned $ do
  _ <- keyword "case"
  i <- literal
  _ <- colon
  b <- block
  return (CaseArm i b)

typeSwitchStmt :: Parser (Annotated Stmt SpanA)
typeSwitchStmt = spanned $ do
  P.try (keyword "switch" >> keyword "type")
  var <- parens identifier
  cases <- many1 typeCase
  keyword "end"
  return (StmtTypeSwitch var cases)

typeCase :: Parser (Annotated TypeCase SpanA)
typeCase = spanned $ do
  keyword "case"
  ty <- nullOrType
  colon
  body <- block
  return (TypeCase ty body)

scopeStmt :: Parser (Annotated Stmt SpanA)
scopeStmt = spanned $ do
  _ <- keyword "begin"
  b <- block
  _ <- keyword "end"
  return (StmtScope b)

printStmt :: Parser (Annotated Stmt SpanA)
printStmt = spanned $ do
  keyword "print"
  args <- sepBy1 expr comma
  return (StmtPrint args False)

printlnStmt :: Parser (Annotated Stmt SpanA)
printlnStmt = spanned $ do
  keyword "println"
  args <- sepBy1 expr comma
  return (StmtPrint args True)

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

chanSendStmt :: Parser (Annotated Stmt SpanA)
chanSendStmt = spanned $ do
  channel <- P.try (identifier <* op "<-")
  rhs <- assignRHS
  return (StmtChanSend channel rhs)

stmt :: Parser (Annotated Stmt SpanA)
stmt = skipStmt    <|>
       whileStmt   <|>
       forStmt     <|>
       ifStmt      <|>
       typeSwitchStmt <|>
       switchStmt  <|>
       scopeStmt   <|>
       printStmt   <|>
       printlnStmt <|>
       readStmt    <|>
       freeStmt    <|>
       exitStmt    <|>
       returnStmt  <|>
       letStmt     <|>
       callStmt    <|>
       awaitStmt   <|>
       fireStmt    <|>
       chanSendStmt <|>
       varStmt     <|>
       assignStmt  <?> "statement"

block :: Parser (Annotated Block SpanA)
block = spanned $ Block <$> sepBy1 stmt semi

param :: Parser (Type, Identifier)
param = (,) <$> parseType <*> identifier

function :: Parser (Annotated Decl SpanA)
function = wrapSpan DeclFunc <$> (spanned $ do
  async <- option False (keyword "async" >> return True)
  (returnType, functionName) <- P.try $ do
    t <- voidOrType
    i <- identifier
    lookAhead (token TokLParen)
    return (t, i)

  args <- parens (sepBy param comma)
  keyword "is"
  body <- block
  keyword "end"
  return (FuncDef returnType async (FuncName functionName) args body))

typeDecl :: Parser (Annotated Decl SpanA)
typeDecl = wrapSpan DeclType <$> (spanned $ do
  keyword "type"
  typeName <- identifier
  equal
  typeAlias <- parseType
  semi

  return (TypeDef typeName typeAlias))

ffiFunction :: Parser (Annotated Decl SpanA)
ffiFunction = wrapSpan DeclFFIFunc <$> (spanned $ do
  async <- option False (keyword "async" >> return True)
  returnType <- voidOrType
  functionName <- identifier
  args <- parens (sepBy parseType comma)
  symbolName <- (equal >> identifier) <|> return functionName
  semi

  return (FFIFunc returnType async functionName args symbolName))

ffiType :: Parser (Annotated Decl SpanA)
ffiType = wrapSpan DeclType <$> (spanned $ do
  keyword "type"
  name <- identifier
  semi

  return (TypeDef name (TyFFI name)))

decl :: Parser (Annotated Decl SpanA)
decl = function <|>
       typeDecl <|>
       (keyword "ffi" *> (ffiFunction <|> ffiType))

mainFunc :: Parser (Annotated FuncDef SpanA)
mainFunc = spanned $ do
  b <- block
  async <- lift (getArgument runtimeEnabled)
  return (FuncDef TyVoid async MainFunc [] b)

program :: Parser (Annotated Program SpanA)
program = spanned $ do
  _ <- keyword "begin"
  f <- many decl
  m <- mainFunc
  _ <- keyword "end"
  return (Program ((wrapSpan DeclFunc m):f))

waccParser :: String -> [(Pos, Token)] -> WACCResultT WACCArguments (Annotated Program SpanA)
waccParser fname tokens =
  (lift $ P.runParserT (program <* eof) () fname tokens) >>= \case
             Left e -> syntaxError (show e)
             Right p -> return p

