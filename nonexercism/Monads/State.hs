module Monads where

import Control.Applicative
import Data.Functor
import Control.Monad.State  
import Control.Monad
import Data.List (find)
--import Control.Monad.Identity -- (Identity) 
--
type Strings = [String]

addWord :: String -> State Strings ()
addWord x = state (\s -> ((),  x:s))

doState :: State Strings (Maybe String)
doState = do
  addWord "xXx"
  searchWord "x.x"

searchWord :: String -> State Strings (Maybe String)
searchWord x = state $ \s -> (find (match x) s, s)
   where 
     match x y = all id $ zipWith match' x y 
     match' '.' _ = True
     match'  x y  = (x == y)

     
main = do 
  -- running the state gets you a pure value!
  -- runState :: State s a -> s -> (a, s)
  print $ runState doState []
