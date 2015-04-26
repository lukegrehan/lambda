module Parser where

import Text.ParserCombinators.Parsec
import Defs

parse' :: String -> Either ParseError Lambda
parse' = parse lambdaExpr ""

braces = between (char '(') (char ')')

var' = do
  i <- letter
  rest <- many alphaNum
  return $ i:rest
 
varName = do
  v <- var'
  return $ Var v

app = do
  f <- braces lambdaExpr
  spaces
  a <- lambdaExpr
  return $ App f a

lambdaTerm = do
  char '\\'
  var <- var'
  char '.'
  body <- lambdaExpr
  return $ Abs var body

lambdaExpr =  lambdaTerm
          <|> app
          <|> varName
