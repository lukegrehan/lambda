module Parser (parse) where

import Text.ParserCombinators.Parsec hiding (parse)
import Defs

parse :: String -> IO (Either ParseError [Defn])
parse inp = parseFromFile lambda inp

lambda = defn `sepEndBy` (many newline)
  
defn = do
  name <- defnName
  spaces
  string ":="
  spaces
  defnBody <- lambdaExpr
  return $ Defn name defnBody

defnName = do
  i <- upper
  rest <- many alphaNum
  return $ i:rest

-------------------------------------------------------------------------------

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
