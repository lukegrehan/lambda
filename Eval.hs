module Eval (flatten, reduceB, beta) where
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
flatten' (Free s)  ds = fromMaybe (Free s) $ lookup s ds
flatten' (Var s)   ds = (Var s)
flatten' (Abs v b) ds = Abs v $ flatten' b ds
flatten' (App f a) ds = App (flatten' f ds) (flatten' a ds)

reduceB :: Lambda -> Lambda
reduceB l = go l (beta l)
  where 
    go p Nothing = p
    go _ (Just n) = go n (beta n)

beta :: Lambda -> Maybe Lambda
beta (Var _)            = Nothing
beta (Free _)           = Nothing
beta (Abs f b)          = Nothing
beta (App (Abs v b) ar) = Just $ replace b v ar
beta (App f ar)         = (flip App ar) <$> beta f
                        -- = App <$> beta f <*> pure ar

replace :: Lambda -> Int -> Lambda -> Lambda 
replace (App f a) s ar = App (replace f s ar) (replace a s ar)
replace (Free s) _ _ = Free s
replace (Var s) s' ar   | s == s' = ar
                        | otherwise = Var s
replace (Abs s b) s' ar | s == s' = Abs s b
                        | otherwise = Abs s (replace b s' ar)

