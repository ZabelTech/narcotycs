{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Naive.Parser where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language (haskellDef)
import qualified Text.ParserCombinators.Parsec.Token as Token

data Expr = StringLiteral String
          | DictLiteral [(String,String)]
          | Variable String
          | Application String [Expr]
           deriving (Eq,Show)

data Stmt = Assignment String Expr
          | Expression Expr
           deriving (Eq,Show)

lexer :: Token.TokenParser st
lexer = Token.makeTokenParser haskellDef

identifier :: Parser String
identifier = Token.identifier lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

commaSep :: Parser a -> Parser [a]
commaSep = Token.commaSep lexer

colon :: Parser String
colon = Token.colon lexer

braces :: Parser a -> Parser a
braces = Token.braces lexer

stringP :: Parser String
stringP = Token.stringLiteral lexer

symbol :: String -> Parser String
symbol = Token.symbol lexer

whitespace :: Parser ()
whitespace = Token.whiteSpace lexer

stringLiteralP :: Parser Expr
stringLiteralP = StringLiteral <$> stringP

dictLiteralP :: Parser Expr
dictLiteralP = braces $ DictLiteral <$> commaSep keyValue
  where keyValue = do
          StringLiteral key <- stringLiteralP
          colon
          StringLiteral value <- stringLiteralP
          return (key,value)

variableP :: Parser Expr
variableP = Variable <$> identifier

applicationP :: Parser Expr
applicationP = do
  function <- identifier
  args     <- parens $ commaSep expressionP
  return $ Application function args

expressionP :: Parser Expr
expressionP = try dictLiteralP <|> try stringLiteralP <|> try applicationP <|> try variableP

statementP :: Parser Stmt
statementP = try assignmentP <|> (Expression <$> try expressionP)

assignmentP :: Parser Stmt
assignmentP = do
  variable   <- identifier
  symbol "="
  Assignment variable <$> expressionP

parser :: Parser [Stmt]
parser = do
  whitespace
  stmts <- many statementP
  eof
  return stmts

parseString :: String -> Either ParseError [Stmt]
parseString = parse parser ""

parseFile :: String -> IO (Either ParseError [Stmt])
parseFile file = parseString <$> readFile file
