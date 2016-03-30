module SumOfMultiples
       where 

sumOfMultiples :: [Int] -> Int -> Int
sumOfMultiples [] _ = 0
sumOfMultiples xs n = sum mults
  where
    --mults x = takeWhile (<= n) [x,x+x..]
    mults = takeWhile (< n) $ filter isMul [1,2..]
      where isMul y = any (\x -> (y `mod` x) == 0) xs
            
sumOfMultiplesDefault :: Int -> Int
sumOfMultiplesDefault = sumOfMultiples [3, 5]
