module Eval where
import Defs

reduceB :: Lambda -> Lambda
reduceB l = go l (reduceS l)
  where 
    go p Nothing = p
    go _ (Just n) = go n (reduceS n)

reduceS :: Lambda -> Maybe Lambda
reduceS (Var _)            = Nothing
reduceS (Abs f b)          = Nothing
reduceS (App (Abs v b) ar) = Just $ replace b v ar
reduceS (App f ar)         = (flip App ar) <$> reduceS f

replace :: Lambda -> String -> Lambda -> Lambda 
replace (App f a) s ar = App (replace f s ar) (replace a s ar)
replace (Var s) s' ar   | s == s' = ar
                        | otherwise = Var s
replace (Abs s b) s' ar | s == s' = Abs s b
                        | otherwise = Abs s (replace b s' ar)
