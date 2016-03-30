module Strain

       where
keep :: (a -> Bool) -> [a] -> [a]
keep p xs =  foldr keep' [] xs
  where keep' y ys = if (p y) then (y:ys) else ys

discard :: (a -> Bool) -> [a] -> [a]
discard p xs =  foldr discard' [] xs
  where discard' y ys = if (not $ p y) then (y:ys) else ys
--keep _ [] = []
--keep f (x:xs) = if (f x) then x : (keep f xs) else (keep f xs)
--discard :: (a -> Bool) -> [a] -> [a]
--discard _ [] = []
--discard f (x:xs) = if (not $ f x) then x : (discard f xs) else (discard f xs)
