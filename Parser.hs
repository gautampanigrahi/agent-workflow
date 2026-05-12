
module Parser
  ( parseProgram
  ) where

import           Control.Monad                 (void)
import           Text.ParserCombinators.Parsec

import           Syntax

-- ====================================================================
-- 1. Whitespace and comments  (the "lexer" half of the front-end).
-- ====================================================================

-- | Skip any combination of whitespace, @// ...@ line comments, and
--   @/* ... */@ block comments.
ws :: Parser ()
ws = skipMany (try blockComment <|> try lineComment <|> void (many1 space))

lineComment :: Parser ()
lineComment = do
  string "//"
  manyTill anyChar (try (void newline) <|> eof)
  return ()

blockComment :: Parser ()
blockComment = do
  string "/*"
  manyTill anyChar (try (string "*/"))
  return ()

-- | Run a parser, then consume the trailing whitespace.  This is the
--   "lexeme" pattern: every token-level parser is wrapped in 'lexeme'
--   so the rest of the grammar can ignore whitespace.
lexeme :: Parser a -> Parser a
lexeme p = do
  x <- p
  ws
  return x

-- | A literal string treated as a single token.
symbol :: String -> Parser ()
symbol s = lexeme (void (string s))

-- ====================================================================
-- 2. Reserved words and identifiers.
-- ====================================================================

reservedNames :: [String]
reservedNames =
  [ "config", "agent", "from", "let", "if", "then", "else"
  , "fail", "retry", "try", "catch", "print"
  , "FixedAgent", "CustomAI", "prompt", "model"
  , "true", "false", "null"
  , "python", "http", "llm", "mock"
  ]

-- | Match a reserved word as a *whole* word — i.e. the next character
--   must not extend it into an identifier.
keyword :: String -> Parser ()
keyword w = lexeme $ try $ do
  string w
  notFollowedBy (alphaNum <|> char '_')

-- | A non-reserved identifier.  Letters / underscores, then letters /
--   digits / underscores.  Reject any name that collides with a
--   reserved word so we don't accidentally bind @if@ as a variable.
identifier :: Parser String
identifier = lexeme $ try $ do
  c  <- letter <|> char '_'
  cs <- many   (alphaNum <|> char '_')
  let name = c : cs
  if name `elem` reservedNames
    then unexpected ("reserved word " ++ show name)
    else return name

fieldName :: Parser String
fieldName = identifier <|> choice (map reservedField reservedNames)
  where
    reservedField name = try (keyword name >> return name)

-- ====================================================================
-- 3. Literals.
-- ====================================================================

stringLitRaw :: Parser String
stringLitRaw = lexeme $ do
  char '"'
  cs <- many (noneOf "\"")
  char '"'
  return cs

naturalRaw :: Parser Integer
naturalRaw = lexeme $ do
  ds <- many1 digit
  return (read ds)

-- | A non-negative integer or floating-point literal — mirrors the
--   slide's @num@ example, extended with an optional fractional part.
numberRaw :: Parser Double
numberRaw = lexeme $ do
  whole <- many1 digit
  frac  <- option "" $ try $ do
    char '.'
    ds <- many1 digit
    return ('.' : ds)
  return (read (whole ++ frac))

-- ====================================================================
-- 4. Operator literals.
-- ====================================================================

-- | Characters that may continue a multi-character operator.  We use
--   this to make 'op' a longest-match parser: e.g. @=@ should fail
--   when the next character is @=@ (so @==@ and @=>@ aren't shadowed).
opCont :: String
opCont = "=!<>&|.+-*/:"

-- | Match operator @s@ exactly, refusing to consume it if a longer
--   operator is starting.
op :: String -> Parser ()
op s = lexeme $ try $ do
  string s
  notFollowedBy (oneOf opCont)

comma, semi, dot :: Parser ()
comma = symbol ","
semi  = symbol ";"
dot   = symbol "."

parens, braces :: Parser a -> Parser a
parens p = do { symbol "("; x <- p; symbol ")"; return x }
braces p = do { symbol "{"; x <- p; symbol "}"; return x }

commaSep :: Parser a -> Parser [a]
commaSep p = sepBy p comma

-- ====================================================================
-- 5. Expressions.
-- ====================================================================

expr :: Parser Expr
expr = orExpr <?> "expression"

orExpr :: Parser Expr
orExpr = do
  l <- andExpr
  rest l
  where
    rest l =  (do op "||"; r <- andExpr; rest (EBin OpOr l r))
          <|> return l

andExpr :: Parser Expr
andExpr = do
  l <- cmpExpr
  rest l
  where
    rest l =  (do op "&&"; r <- cmpExpr; rest (EBin OpAnd l r))
          <|> return l

-- | Comparisons are non-associative — at most one in a chain.
cmpExpr :: Parser Expr
cmpExpr = do
  l <- addExpr
  option l $ do
    o <- choice
      [ op "==" >> return OpEq
      , op "!=" >> return OpNeq
      , op ">=" >> return OpGte
      , op "<=" >> return OpLte
      , op ">"  >> return OpGt
      , op "<"  >> return OpLt
      ]
    r <- addExpr
    return (EBin o l r)

addExpr :: Parser Expr
addExpr = do
  l <- mulExpr
  rest l
  where
    rest l =  (do op "+"; r <- mulExpr; rest (EBin OpAdd l r))
          <|> (do op "-"; r <- mulExpr; rest (EBin OpSub l r))
          <|> return l

mulExpr :: Parser Expr
mulExpr = do
  l <- postfixExpr
  rest l
  where
    rest l =  (do op "*"; r <- postfixExpr; rest (EBin OpMul l r))
          <|> (do op "/"; r <- postfixExpr; rest (EBin OpDiv l r))
          <|> return l

-- | Chained record-field projections @e.f.g@.
postfixExpr :: Parser Expr
postfixExpr = do
  e <- atom
  loop e
  where
    loop e =  (do dot; f <- fieldName; loop (EProj e f))
          <|> return e

atom :: Parser Expr
atom =  parens expr
    <|> listLit
    <|> recordLit
    <|> stringE
    <|> numberE
    <|> nullE
    <|> boolE
    <|> identAtom

stringE :: Parser Expr
stringE = do
  s <- stringLitRaw
  return (EConst (VString s))

numberE :: Parser Expr
numberE = do
  n <- numberRaw
  return (EConst (VNumber n))

boolE :: Parser Expr
boolE =  (keyword "true"  >> return (EConst (VBool True)))
     <|> (keyword "false" >> return (EConst (VBool False)))

nullE :: Parser Expr
nullE = keyword "null" >> return (EConst VNull)

listLit :: Parser Expr
listLit = do
  symbol "["
  es <- commaSep expr
  symbol "]"
  return (EList es)

recordLit :: Parser Expr
recordLit = do
  fs <- braces (commaSep field)
  return (ERecord fs)
  where
    field = do
      f <- fieldName
      op "="
      e <- expr
      return (f, e)

-- | A bare identifier becomes either a call @A(e,…)@ or a variable.
identAtom :: Parser Expr
identAtom = do
  name  <- identifier
  margs <- optionMaybe (parens (commaSep expr))
  return $ case margs of
    Just args -> ECall name args
    Nothing   -> EVar  name

-- ====================================================================
-- 6. Statements.
-- ====================================================================

program :: Parser Stmt
program = do
  ws
  ss <- sepEndBy1 stmt semi
  eof
  return (foldr1 SSeq ss)

stmt :: Parser Stmt
stmt = choice
  [ stmtBlock
  , stmtConfig
  , stmtAgent
  , stmtLet
  , stmtIf
  , stmtFail
  , stmtRetry
  , stmtTryCatch
  , stmtPrint
  ] <?> "statement"

-- | @{ s₁ ; s₂ ; … }@ is right-folded into 'SSeq'.
stmtBlock :: Parser Stmt
stmtBlock = do
  symbol "{"
  ss <- sepEndBy1 stmt semi
  symbol "}"
  return (foldr1 SSeq ss)

stmtConfig :: Parser Stmt
stmtConfig = do
  keyword "config"
  fs <- braces (commaSep configField)
  return (SConfig fs)
  where
    configField = do
      c <- fieldName
      op "="
      e <- expr
      return (c, e)

stmtAgent :: Parser Stmt
stmtAgent = do
  keyword "agent"
  name <- identifier
  (do keyword "from"
      b <- backendP
      return (SAgentBackend name b))
   <|>
   (do op "="
       agentRhs name)

agentRhs :: String -> Parser Stmt
agentRhs name =
       (do keyword "FixedAgent"
           k <- parens kindP
           return (SAgentFixed name k))
   <|> (do keyword "CustomAI"
           symbol "("
           keyword "prompt"; op "="; pe <- expr
           m <- optionMaybe $ try $ do
             comma
             keyword "model";  op "="; stringLitRaw
           symbol ")"
           return (SAgentCustom name pe m))

backendP :: Parser Backend
backendP = choice
  [ keyword "python" >> op ":" >> (BPython           <$> stringLitRaw)
  , keyword "http"   >> op ":" >> (BHttp             <$> stringLitRaw)
  , keyword "llm"    >> op ":" >> (BLlm              <$> stringLitRaw)
  , keyword "mock"   >> op ":" >> (BMock . VString   <$> stringLitRaw)
  ] <?> "backend"

kindP :: Parser Kind
kindP = do
  k <- identifier
  case k of
    "Planner"             -> return Planner
    "TaskSplitter"        -> return TaskSplitter
    "Extractor"           -> return Extractor
    "Critic"              -> return Critic
    "Writer"              -> return Writer
    "Summarizer"          -> return Summarizer
    "Validator"           -> return Validator
    "Guardrail"           -> return Guardrail
    "Router"              -> return Router
    "Merger"              -> return Merger
    "Ranker"              -> return Ranker
    _                     -> unexpected ("agent kind " ++ show k)

stmtLet :: Parser Stmt
stmtLet = do
  keyword "let"
  x <- identifier
  op "="
  e <- expr
  return (SLet x e)

stmtIf :: Parser Stmt
stmtIf = do
  keyword "if"   ; c  <- expr
  keyword "then" ; s1 <- stmt
  keyword "else" ; s2 <- stmt
  return (SIf c s1 s2)

stmtFail :: Parser Stmt
stmtFail = do
  keyword "fail"
  e <- expr
  return (SFail e)

stmtRetry :: Parser Stmt
stmtRetry = do
  keyword "retry"
  n <- naturalRaw
  s <- stmt
  return (SRetry (fromIntegral n) s)

stmtTryCatch :: Parser Stmt
stmtTryCatch = do
  keyword "try"   ; s1 <- stmt
  keyword "catch" ; x  <- identifier
  op "=>"         ; s2 <- stmt
  return (STryCatch s1 x s2)

stmtPrint :: Parser Stmt
stmtPrint = do
  keyword "print"
  e <- expr
  return (SPrint e)

-- ====================================================================
-- 7. Entry point.
-- ====================================================================

parseProgram :: FilePath -> String -> Either ParseError Stmt
parseProgram = parse program
