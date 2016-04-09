import Data.List (unfoldr, intersperse)
import Data.Foldable (foldr')
import Data.Char (isDigit)
-- compress:
-- #input:  aaaabbccdddddeefgg
-- #output: a4b2c2d5e2fg2
--
--compress :: String -> String
--compress s = intersperse "" (map f xs) ---
--  where
--    f (x, 1) = x
--    f (x, n) = x ++ (show n)
--    xs = compress s 
--    compress' :: String -> [(Char, Int)]
--    compress' s = foldr' f [] s 
--      where
--        f :: [(Char, Int)] -> Char -> [(Char, Int)]
--        f (y:ys) x = if ( (fst y) == x) then ((x, (snd y) + 1) : ys) else ((x, 1) : (y:ys))
--        f []     x = [(x, 1)]
--    
---- decompress:
---- #input: a4b2c2d5e2fg2h10
---- #output: aaaabbccdddddeefgghhhhhhhhhh
---- input: a
--decompress [] = []
--decompress (x:xs) = (replicate n x) ++ decompress rest
--  where
--    (n, rest) =  if (null n') then (1,xs) else ((read n' :: Int), ys)
--    (n', ys) = span isDigit xs
--    
--    
--compress' []     = [] 
--compress' (x:xs) = (x:count) ++ (compress' rest)
--  where
--    count = if (null xx) then "" else show $ (length xx) + 1
--    (xx, rest) = span (x ==) xs

main = do
--  let res = compress' "aaaabbccdddddeefgghhhhhhhhhh"
--  putStrLn res
  let res' = compress'' "aaaabbccdddddeefgghhhhhhhhhh"
  print res'


splitEq []     =  Nothing 
splitEq (x:xs) = Just $ (x:xs', ys')
   where (xs', ys') = span (== x) xs
         
compress'' xs =  map str $ unfoldr splitEq xs
  where 
    str (x:[]) = [x]
    str (x:xs) = x:(show $ length xs + 1)

--compress'' xs =  unfoldr f xs
--  where
--    f (x:xs) = Just $ ((str xs'), ys')
--      where
--        (xs', ys') = span (== x) xs
--        str [] = [x]
--        str _  = x:(show $ (length xs) + 1)
--    f []     =  Nothing
