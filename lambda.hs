import Defs
import Eval
import Parser

test2 = "(((\\a.\\b.\\c.c) x) y) z"

runParse s = reduceB <$> parse' s

main :: IO ()
main = print $ runParse test2
