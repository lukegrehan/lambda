module Parser where

import Text.ParserCombinators.Parsec

parse' :: String -> Either ParseError String
parse' = parse lambdaExpr "(unknown)"

varName = do
  i <- letter
  rest <- many alphaNum
  return $ i:rest

app = do
  char '('
  f <- lambdaExpr
  a <- lambdaExpr
  char ')'
  return $ "(" ++ f ++ ":" ++ a ++ ")"

lambdaTerm = do
  char '\\'
  var <- varName
  char '.'
  body <- lambdaExpr
  return $ var ++ "-> (" ++ body ++")"

lambdaExpr =  lambdaTerm
          <|> app
          <|> varName
