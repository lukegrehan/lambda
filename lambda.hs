import Eval
import Parser
import System.Environment (getArgs)

getFileName :: IO String
getFileName = do
  args <- getArgs
  if(null args)
    then getLine
    else return $ head args

main :: IO ()
main = do
  fname <- getFileName
  l <- parse fname
  case l of
    Left e -> print e
    Right l -> print $ reduceB l
