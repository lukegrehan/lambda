import Defs
import Eval
import Parser

test = App (Abs "a" (Abs "b" (Var "a"))) (Var "x")
testS = "\\x.\\y.\\z.z"

main :: IO ()
main = print $ parse' testS
