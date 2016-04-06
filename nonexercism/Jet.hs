-- compress:
-- #input:  aaaabbccdddddeefgg
-- #output: a4b2c2d5e2fg2

compress :: String -> String
compress s = intersperse "" (map f xs) ---
  where
    f (x, 1) = x
    f (x, n) = x ++ (show n)
    xs = compress s 
    compress' :: String -> [(Char, Int)]
    compress' s = foldr' f [] s 
      where
        f :: [(Char, Int)] -> Char -> [(Char, Int)]
        f (y:ys) x = if ( (fst y) == x) then ((x, (snd y) + 1) : ys) else ((x, 1) : (y:ys))
        f []     x = [(x, 1)]
    
-- decompress:
-- #input: a4b2c2d5e2fg2h10
-- #output: aaaabbccdddddeefgghhhhhhhhhh
-- input: a
decompress [] = []
decompress (x:xs) = (repeat x n) ++ decompress rest
  where
    (n, rest) =  if (null n') then (1,xs) else ((read n' :: Int), ts)
    (n', ys) = span isDigit xs
    
    
compress' []     = [] 
compress' (x:xs) = (x:count) ++ (compress' rest)
  where
    count = if (null xx) then "" else show $ (length xx) + 1
    (xx, rest) = span (x ==) xs

main = do
  let res = compress' "aaaabbccdddddeefgghhhhhhhhhh"
  putStrLn res


