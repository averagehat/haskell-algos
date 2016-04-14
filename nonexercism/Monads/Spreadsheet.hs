import Control.Monad.State
import Data.Functor
import Data.Tuple.Extra
import qualified Data.Map as M
import Control.Applicative 
import Data.Maybe
type Cell = (Int, Int)
type DepMap = M.Map Cell [Cell]
type Sheet = State (M.Map Cell Int, DepMap)

value :: Cell -> Sheet (Maybe Int) 
value c =  ((M.lookup c) . fst) <$> get 

setConstant :: Cell -> Int -> Sheet ()
setConstant c x = do
  oldV <- value c
  modify $ first (M.insert c x)
  depCells <- ((M.lookup c) . snd) <$> get
  maybe (return ()) setDeps $ liftA2 (,) oldV depCells
  -- depCells is a maybe
  -- oldV' is a maybe
  where
    setDeps (oldV', depCells')  = do 
      depVals <- sequence $ map value depCells'
      let depVals' =  map (fromMaybe (error "If in the DM should have a value set.")) $  depVals
      let newVals = map (\n -> n - oldV' + x) depVals'
      zipWithM_ setConstant depCells' newVals
      
  
-- A value will be updated if a cell it is dependent on is changed.
-- update it by subtracting that cell (it is depenendent on)'s old value and then adding the new value.
setSum :: Cell -> Cell -> Cell -> Sheet ()
setSum c d1 d2 = do
  v1 <- value d1
  v2 <- value d2
  -- could do fromMaybe error
  let v' =  (+) <$> v1 <*> v2
  -- Maybe does something only if it's not Nothing. Exploit laziness/short-circuiting.
  -- maybe :: b -> (a -> b) -> Maybe a -> b 
  maybe (return ()) (setConstant c) v'
  -- modify updates the state.
  modify $ second $ maybeUpdate (c:) d1 []  --M.update (Just . (c:)) d1
  modify $ second $ maybeUpdate (c:) d2 []
  where 
    maybeUpdate f k v' m = M.insert k (f (M.findWithDefault  v' k m)) m

spreadStuff = do
  setConstant (1, 3) 3
  setConstant (2, 4) 2
  setSum (9, 9) (1, 3) (2, 4)
  setConstant (1, 3) 99
  value (9, 9)
  
spreadStuff' = do
  setConstant (1, 3) 3
  setConstant (2, 4) 6
  setSum (9, 9) (1, 3) (2, 4)
  setConstant (0, 0) 2
  setSum (2, 4) (0, 0) (1, 3) -- (2, 4) = 2 + 3
  value (9, 9) -- 2 + 3 + 3
  
main :: IO ()  
main = do
  print $ runState spreadStuff (M.empty, M.empty) 
  print $ runState spreadStuff' (M.empty, M.empty) 
