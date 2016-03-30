module Bob
       where
import Data.Char

responseFor :: String -> String
responseFor x
  | all (`elem` "\n\r \t\v\xA0\x2002") x = "Fine. Be that way!"
  | (not $ null alphas) && (all isUpper alphas) = "Whoa, chill out!"
  | last x == '?' = "Sure."
  | otherwise = "Whatever."
    where alphas = filter isAlpha  x
   
