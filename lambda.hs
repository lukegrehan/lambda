import Defs

test :: Lambda
test =  Abs "a" (Abs "b" (Var "a"))

main :: IO ()
main = print test
