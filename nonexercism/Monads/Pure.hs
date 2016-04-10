import Data.Map hiding (map)
import Prelude hiding (lookup)
import Data.Maybe (fromMaybe, isJust)
import Data.List (find)
data Words = Words (Map Char Words) | Leaf
  deriving (Show)
-- https://leetcode.com/problems/add-and-search-word-data-structure-design/

-- right now the below returns the query, want to return the found word
searchWord :: String -> Words -> Maybe String
searchWord x m = searchWord' x m
  where
    searchWord' (x':xs) (Words m') = do
      if (x' == '.') then
        fromMaybe Nothing $ find (isJust) $ map (searchWord' xs) (elems m')
      else  do
        next <- x' `lookup` m'
        searchWord' xs next
    searchWord'  [] Leaf = (Just x)
    searchWord'  [] (Words _) = Nothing

addWord' :: String -> Words -> Words
addWord'   []   _         = Leaf
addWord' (x:xs) Leaf      = Words $ fromList [(x, addWord' xs Leaf)]
addWord' (x:xs) (Words m) = Words $ insert x (addWord' xs defaultLeaf) m
  where
    defaultLeaf = fromMaybe Leaf $ x `lookup` m 
  -- insert the letter via creating a new map
  -- if the letter already is in `m`, just recur
  -- need to return the child node
main = do
  print "foo"
  let ws = addWord' "finz" Leaf
  print $ addWord' "fibd" ws
  print $ searchWord "finz" ws
  print $ searchWord "fi.." ws
  print $ searchWord "inz" ws
  print $ searchWord "fin" ws
  --print $ addWord' "nn" ws
