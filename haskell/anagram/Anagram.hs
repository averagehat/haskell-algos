module Anagram
       where
--import qualified Data.Set as S
import qualified Data.Char as C
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
type Counter = M.Map Char Int

anagramsFor :: String -> [String] -> [String]
anagramsFor x ys = filter notEqualX $ filter ((xcount ==) . toCounter) ys
  where
    xcount = (toCounter x)
    notEqualX y = (map C.toLower x) /= (map C.toLower y)
  --filter (isAnagram $ S.fromList $ map C.toLower s) xs

toCounter :: String -> Counter
toCounter xs = foldr f (M.fromList []) $ map C.toLower xs
  where
    f :: Char -> Counter -> Counter
    f x z = M.insert x (1 + (fromMaybe 0 $ M.lookup x z)) z

--  where
--    isAnagram xs ys = foldr f (False, set)
--      f x (b,set) = if (x `S.member` set) then 
--     where set = S.fromList
--isAnagram :: Counter -> String -> Bool
--isAnagram set ys
--  | S.null set && null ys = True
--  | S.null set `xor` null ys = False
--  | otherwise  = if (y `S.member` set) then isAnagram (S.delete y set) (tail ys) else False
--  where
--    y = C.toLower (head ys)
--    xor a b = (a || b) && not (a && b)
--isAnagram _ [] = False     
