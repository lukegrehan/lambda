module Parser (parse) where

import Text.Parsec hiding (parse)
import Defs

parse :: FilePath -> IO (Either ParseError [Defn])
parse fname = do
  cont <- readFile fname
  return $ runParser lambda (0,[]) fname cont

lambda = defn `sepEndBy` (many newline)
  
defn = do
  resetSyms
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

getSymTable :: String -> Parsec s (Int, [(String, Int)]) Int
getSymTable name = do
  (cnt, vals) <- getState
  case lookup name vals of
    Just n -> return n
    Nothing -> do
      putState (cnt+1, vals++[(name, cnt)])
      return cnt

resetSyms = putState (0, [])

unsetSym name = do
  (cnt, vals) <- getState
  let nVals = deleteBy ((==name).fst) vals
  putState (cnt, nVals)

braces = between (char '(') (char ')')

var' = do
  i <- letter
  rest <- many alphaNum
  return $ i:rest
 
varName = do
  v <- var'
  uid <- getSymTable v
  return $ Var uid

app = do
  f <- braces lambdaExpr
  spaces
  a <- lambdaExpr
  return $ App f a

lambdaTerm = do
  char '\\'
  v <- var'
  var <- getSymTable v
  char '.'
  body <- lambdaExpr
  return $ Abs var body

lambdaExpr =  lambdaTerm
          <|> app
          <|> varName
