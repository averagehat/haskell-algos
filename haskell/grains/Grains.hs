module Grains
       where

total = foldr (+) 0 squares

squares = take 64 $ iterate (* 2) 1

square n = (squares !! (n - 1))
main :: IO ()
main = do
  putStrLn $ show squares
