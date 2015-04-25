module Defs where

data Lambda = Var String
            | Abs { var :: String, body :: Lambda }
            | App { func :: Lambda, arg :: Lambda }

instance Show Lambda where
  show (Var s) = s
  show (Abs v b) = "\\" ++ v ++ "." ++ show b
  show (App f a) = "("++show f++")"++show a

