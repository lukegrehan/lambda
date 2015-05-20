module Eval (flatten, reduceB, beta) where
import Defs
import Data.Maybe
import Data.List
import Control.Applicative

flatten :: [Defn] -> Maybe Lambda
flatten ds = flatten' <$> main
  where
    main = snd <$> find ((== "Main").fst) resolved

    flatten' :: Lambda -> Lambda
    flatten' (Free s)  = fromMaybe (Free s) $ lookup s resolved
    flatten' (Var s)   = (Var s)
    flatten' (Abs v b) = Abs v $ flatten' b
    flatten' (App f a) = App (flatten' f) (flatten' a)

    resolved = head $ drop 10 $ iterate (map (resolve' <$>)) ds -- I am a bad person... TODO
      where
        resolve' (Var v) = (Var v)
        resolve' (Abs f b) = Abs f $ resolve' b
        resolve' (App f ar) = App (resolve' f) (resolve' ar)
        resolve' (Free f) = fromMaybe (Free f) $ lookup f ds

reduceB :: Lambda -> Lambda
reduceB l = go l (beta l)
  where 
    go p Nothing = p
    go _ (Just n) = go n (beta n)

beta :: Lambda -> Maybe Lambda
beta (Var _)            = Nothing
beta (Free _)           = Nothing
beta (Abs f b)          = Abs <$> pure f <*> beta b
beta (App (Abs v b) ar) = Just $ replace b v ar
beta (App (Free f) ar)  = App (Free f) <$> beta ar
beta (App f ar)         = App <$> beta f <*> pure ar 

replace :: Lambda -> Int -> Lambda -> Lambda 
replace (App f a) s ar = App (replace f s ar) (replace a s ar)
replace (Free s) _ _ = Free s
replace (Var s) s' ar   | s == s' = ar
                        | otherwise = Var s
replace (Abs s b) s' ar | s == s' = Abs s b
                        | otherwise = Abs s (replace b s' ar)

