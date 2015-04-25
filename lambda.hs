import Defs

test :: Lambda
test = App (Abs "a" (Abs "b" (Var "a"))) (Var "x")

main :: IO ()
main = print $ reduceB test
