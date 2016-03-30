module DNA
       where

hammingDistance :: String -> String -> Int
--hammingDistance = length . (filter id) . (zipWith (/=))
hammingDistance xs ys = length $ filter id $ zipWith (/=) xs ys
