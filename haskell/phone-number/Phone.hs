module Phone where
import Data.Char (isDigit)
import Data.Maybe (fromMaybe) 
import Control.Applicative

data PhoneNum = PhoneNum String String String

fixNumber :: String -> Maybe String
fixNumber xs
  | len < 10 = Nothing
  | len == 11 = if ((head xs) == '1') then Just (tail xs) else Nothing
  | len > 11 = Nothing
  | otherwise = Just xs
  where len = (length xs)
        
        
areaCode :: String -> String
areaCode xs = fromMaybe "0" $ (take 3) <$> fixNumber xs

number :: String -> String
number ('(':a:b:c:')':' ':rest) = number' (a:b:c:rest)
number xs = fromMaybe "0000000000" $ fixNumber $ number' xs 
number' xs = area ++ first ++ second
      where (PhoneNum area first second) = sections xs
            
sections :: String -> PhoneNum
sections xs = PhoneNum area first' second'
  where
    (area, rest) = splitAt 3 xs
    ((f:first), (s:second)) = splitAt 3 rest
    first' = if (f `elem` ". -") then first else (f:first)
    second' = takeWhile isDigit $ if (s `elem` ". -") then second else (s:second)
    

prettyPrint :: String -> String 
prettyPrint xs = fromMaybe "0000000" $ prettyPrint' <$> fixNumber xs
  where
    prettyPrint' xs = "(" ++ area ++ ")" ++ " " ++ first ++ "-" ++ second
      where (PhoneNum area first second) = sections xs
    
