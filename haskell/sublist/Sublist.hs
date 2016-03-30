module Sublist (Sublist(Equal, Sublist, Superlist, Unequal), sublist) where
import Data.List (tails)
data Sublist = Equal | Sublist | Superlist | Unequal
   deriving (Show, Eq)

--sublist xs ys 
--   | isPrefix && ((length xs) == (length ys)) = Equal
--   | isPrefix && ((length xs) < (length ys))  = Sublist
--   | isPrefix && ((length xs) > (length ys))  = Superlist
--   | otherwise = Unequal
--   where 
--     isPrefix = all id $ zipWith (==) xs ys

sublist xs ys 
   | xs == ys = Equal
   | contains xs ys = Sublist
   | contains ys xs  = Superlist
   | otherwise = Unequal 
   where 
     isPrefix xs ys = (all id $ zipWith (==) xs ys) && ((length xs) <= (length ys))
     contains xs ys = any (xs `isPrefix`) $ tails ys

