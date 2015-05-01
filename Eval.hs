module Eval (flatten, reduceB, reduceS) where
import Defs
import Data.Maybe
import Data.List
import Control.Applicative

flatten :: [Defn] -> Maybe Lambda
flatten ds = flatten' <$> m <*> ds'
  where
    m = defnBody <$> find isMain ds
    ds' = mkTable $ filter (not.isMain) ds
    mkTable ds = Just $ zip (map name ds) (map defnBody ds)
    isMain = (\d -> name d == "Main")

flatten' :: Lambda -> [(String, Lambda)] -> Lambda
flatten' (Var s) ds = fromMaybe (Var s) $ lookup s ds
flatten' (Abs v b) ds = Abs v $ flatten' b ds
flatten' (App f a) ds = App (flatten' f ds) (flatten' a ds)

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
