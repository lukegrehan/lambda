module Defs where

data Defn = Defn { name :: String, defnBody :: Lambda }

data Lambda = Var Int
            | Free String
            | Abs { var :: Int, body :: Lambda }
            | App { func :: Lambda, arg :: Lambda }

instance Show Lambda where
  show (Var s) = "V"++ show s
  show (Free s) = s
  show (Abs v b) = "\\V" ++ show v ++ "." ++ show b
  show (App f a) = "("++show f++") "++show a
