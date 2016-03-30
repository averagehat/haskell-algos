module Accumulate where

--accumulate :: (a -> b) -> [a] -> [b]
accumulate' f [] = []
accumulate' f (x:xs) = (f x) : (accumulate f xs)


data Hole = Hole
--accumulate :: (a -> b) -> [a] -> [b]
accumulate f xs = foldr accum [] xs
  where accum x z = (f x) : z

main = putStrLn $ show $ accumulate (\x -> x + 3) [1..6]
