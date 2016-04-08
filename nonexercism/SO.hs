import Data.List (splitAt)
import Data.Tuple (swap)
import Control.Monad (forM_)


rotate :: (Show a) => Int -> [a] -> [a]
rotate k xs = uncurry (++) $ swap $ splitAt (k `mod` length xs) xs 

main = do
  let img = ["XX XX",          
        "  X  ",
        "XX XX"]      
  putStrLn "Up is positive"
  forM_ (rotate 1 img) print
  putStrLn "Down is negative"
  forM_ (rotate (-1) img) print

  --return
  --print $ rotate (-1) img -- down 1
