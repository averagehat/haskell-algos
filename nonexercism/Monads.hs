
import Control.Monad.State  
import Control.Monad.Identity -- (Identity) 

type Strings = [String]
--addWord :: String -> State Strings () 
addWord x = StateT (\s -> ((), ((), x:s)))

searchWord :: String -> State Strings (Maybe String)
searchWord x = state $ \s ->  (find (x ==) s, s)
  where
  --  find' f xs = foldr (\x _ -> if (f x) then (Just x) else Nothing)
    find f xs = safeHead $ filter f xs
      where 
        safeHead [] = Nothing
        safeHead (x:xs) = Just x
-- (* -> * -> *)
doState = do
  addWord "x"
  return 
  --searchWord "f"
main = do
  print $ show  doState
    

