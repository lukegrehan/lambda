module Parser (parse) where

import Text.Parsec hiding (parse)
import Defs

parse :: FilePath -> IO (Either ParseError [Defn])
parse fname = do
  cont <- readFile fname
  return $ runParser lambdaFile (0,[]) fname cont

lambdaFile = defn `sepEndBy` (many newline)
  
defn = do
  putState (0, []) -- reset syms
  name <- defnName
  spaces
  string ":="
  spaces
  defnBody <- lambda
  return $ (name, defnBody)

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

withState act = do
  st <- getState
  r <- act
  putState st
  return r

--unsetSym name = do
--  (cnt, vals) <- getState
--  let nVals = deleteBy ((==name).fst) vals
--  putState (cnt, nVals)

var' = do
  i <- letter
  rest <- many alphaNum
  return $ i:rest
 
varName = do
  v <- var'
  uid <- getSetSym v
  return $ maybe (Free v) Var $ uid

app = between (char '(') (char ')') $ do
  f <- withState lambda
  a <- lambda
  return $ App f a

lambdaTerm = do
  char '\\'
  v <- var'
  var <- getSym v
  char '.'
  body <- lambda
  return $ Abs var body

lambda =  (lambdaTerm
          <|> app
          <|> varName) <* spaces
