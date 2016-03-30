
import qualified Data.Time.Clock as C

import Data.Time.Calendar
import Data.Time.Clock

--fromDay day = addUTCTime (10 ^ 9) $ UTCTime day 0
fromDay day = utctDay $ addUTCTime (10 ^ 9) $ UTCTime day 0

fizzBuzz x 
 | x %? 3  = "Fizz"
 | x %? 5  = "Buzz"
 | x %? 5 && x %? 3 = "FizzBuzz"
   where (%?) q d = (q `mod` d) == 0


--fizzSeq s n = filter fizzBuzz [s..(n+s)]

multiples = [ (x,y) | x <- [1,3..], y <- [1,5..] ]

--interpret (x,y) = 
reds :: (Ord a ) => [a] -> [a]
reds xs = xs

--ffizz acc three five stop 
--  | x >= stop = acc
--  | x == y = fizz ("FizzBuzz" : acc)  three+3 five stop
--  | x <  y = fizz ("Fizz" : acc) three+3 five stop
--  | x >  y = fizz ("Buzz" : acc) three five+5 stop

--fizz :: Int -> Int -> Int -> [String]
--fizz three five stop 
--  | three >= stop = []
--  | three == five = ["FizzBuzz"] ++ (fizz  (three+3) five stop)
--  | three <  five = ["Fizz"] ++ (fizz  (three+3) five stop)
--  | three >  five = ["Buzz"] ++ (fizz  three (five+5) stop)
--

-- it appears this works
fizz  (x:xs) (y:ys) stop
  | (x >= stop) = []
  | x == y = ["FizzBuzz"] ++ fizz xs ys stop
  | x < y = ["Fizz"] ++ fizz xs (y:ys) stop 
  | otherwise = ["Buzz"] ++ fizz (x:xs) ys stop

fives = [5,10..]
threes = [3,6..]
  
main = do
   --putStrLn $ show $ fizz  3 5 100
--   putStrLn $ show $ fizz threes fives 100
--   putStrLn $ show $ fizzSeq 1 100
   putStrLn $ show $ reverseInt 123 
   putStrLn $ show $ mergeSorted [1, 2, 3] [1, 5, 10]
   putStrLn $ show $ isPalindrome "racecar"
   putStrLn $ show $ isPalindrome "racecare"
   putStrLn $ show $ isPalindrome "racazcar"
   putStrLn $ show $ isPalindrome "r"
   putStrLn $ show $ isPalindrome ""
   putStrLn $ show $ solution 5



ffizzBuzz x 
 | x %? 5 && x %? 3 = "FizzBuzz"
 | x %? 3  = "Fizz"
 | x %? 5  = "Buzz"
 | otherwise = ""
   where (%?) q d = (q `mod` d) == 0


fizzSeq s n = filter (not . null) $ map fizzBuzz [s..(n+s)]


reverseInt' :: Int -> [Int]
reverseInt' x = map snd $ tail $ digits
  where 
    f (x, _) = (x `div` 10, x `mod` 10)
    stream = iterate f (x, x)
    digits = until ++ (take 2 rest)
    (until, (rest)) = span (\(x,_) -> not (x < 10)) $ stream
    --takeUntil f xs = (\(xs', x) -> xs' ++ x) $ span f xs
    --(digits, (r:rest)) = takeWhile (\(x,_) -> not (x < 10)) $ iterate f (x, x) 
    --digits = takeUntil (\(x,_) -> not (x < 10)) $ iterate f (x, x) 

joinInt :: [Int] -> Int
joinInt xs = fst $ foldr f (0, 1) xs 
  where f x (num,place) = ((num + place*x), place * 10)

-- reverseInt' is our non-infinite stream, 
-- joinInt receives it, does a right-fold, all lazy, O(N)
reverseInt = joinInt . reverseInt'
--reverseInt x = foldl (++) "" $ map show $ reverseInt' x

mergeSorted :: (Ord a) => [a] -> [a] -> [a]
mergeSorted [] ys = ys
mergeSorted xs [] = xs
mergeSorted (x:xs) (y:ys) 
  | x < y = x : mergeSorted xs (y:ys)
  | x > y = y : mergeSorted (x:xs) ys
  | otherwise = x : y : mergeSorted xs ys

--isPalindrome x = x == (reverse x)
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome xs = isPalindrome' xs 0 ((length xs) - 1)
  where 
    isPalindrome' x l r 
      | l >= r = True
      | otherwise = ((x !! l) == (x !! r)) && (isPalindrome' x (l+1) (r-1))
  
--'
--def isPalindrome(s: str, left (0, -1)) -> bool: 
--   return s[

unders n = takeWhile (< n) squares
  where squares = iterate (* 2) 1


matches x = map oneAt (unders x)
  where oneAt y = if ((x `mod` y) == 0) then 1 else 0
        
solution n = map sum $ map matches [1..(n+1)]

--f = map ($ 0) [1, 2]

--foo :: [a -> a] -> a -> a
--foo xs = foldr bar id xs
--  where
--    bar = (:)

filterNothing :: [Maybe a] -> [a]
filterNothing = filter _body _body2

f :: a -> Maybe a
f x = _f_body

g :: [a] -> a
g [] = _f_body
g (x:xs) = _f_body

-- alt+? -> error at position (or hole type)
-- alt+t -> case split
-- alt+/ -> autocomplete
-- space-m-h-h (ctrl+c-ctrl+h) -> hoogle
-- ctrl+t -> type
-- alt+n -> next error
-- ctrl+c-ctrl+a -> auto-refine hole
