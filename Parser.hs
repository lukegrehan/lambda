module Parser (parse) where

import Text.Parsec hiding (parse)
import Defs

parse :: FilePath -> IO (Either ParseError [Defn])
parse fname = do
  cont <- readFile fname
  return $ runParser lambda (0,[]) fname cont

lambda = defn `sepEndBy` (many newline)
  
defn = do
  putState (0, []) -- reset syms
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

getSym :: String -> Parsec s (Int, [(String, Int)]) Int
getSym name = do
  (cnt, vals) <- getState
  case lookup name vals of
    Just n -> return n
    Nothing -> do
      putState (cnt+1, vals++[(name, cnt)])
      return cnt

getSetSym name = do
  (cnt, vals) <- getState
  return $ lookup name vals

--unsetSym name = do
--  (cnt, vals) <- getState
--  let nVals = deleteBy ((==name).fst) vals
--  putState (cnt, nVals)

braces = between (char '(') (char ')')

var' = do
  i <- letter
  rest <- many alphaNum
  return $ i:rest
 
varName = do
  v <- var'
  uid <- getSetSym v
  return $ maybe (Free v) Var $ uid

app = do
  st <- getState
  f <- braces lambdaExpr
  putState st
  spaces
  a <- lambdaExpr
  return $ App f a

lambdaTerm = do
  char '\\'
  v <- var'
  var <- getSym v
  char '.'
  body <- lambdaExpr
  return $ Abs var body

lambdaExpr =  lambdaTerm
          <|> app
          <|> varName
