module Defs where

data Lambda = Var String
            | Abs { var :: String, body :: Lambda }
            | App { func :: Lambda, arg :: Lambda }

instance Show Lambda where
  show = const "<lambda>"

