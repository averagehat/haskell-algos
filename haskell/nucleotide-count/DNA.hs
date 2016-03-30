module DNA
       where

import qualified Data.Map as M
import Data.Maybe (fromMaybe)

type Counter = M.Map Char Int
count :: Char -> String -> Int
count x ys = fromMaybe 0 $ lookupErr x $ toCounter ys

--lookupErr x m = if (x `elem` "ACGT") then (M.lookup x m) else (error ("invalid nucleotide " ++ (show x)))
lookupErr x m 
   | x `notElem` "ACGT" = error ("invalid nucleotide " ++ (show x))
   | otherwise = M.lookup x m
--nucleotideCounts :: String -> [(Char, Int)]
nucleotideCounts = toCounter

toCounter :: String -> Counter
toCounter = foldr f (M.fromList [('A',0),('C',0),('G',0),('T',0)])
  where
    f x z = M.insert x (1 + (fromMaybe 0 $ lookupErr x z)) z

main = do
  --putStrLn $ show $ toCounter "GPAC"
  --putStrLn $ show $ count 'X' "GACT"
  putStrLn $ show $ nucleotideCounts "GPAC"

