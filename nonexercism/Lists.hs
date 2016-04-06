import Data.Set (insert, member, fromList)
import Data.Foldable (foldr', foldl', find)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Functor ( (<$>) )
import Control.Applicative ( (<*>) )
import Control.Monad (liftM2)
--twoSum ::  [Int] -> [Int] -> Int -> Maybe (Int, Int)
----twoSum xs ys n = twoSum' xs (fromList ys)
----  where
----    twoSum' [] _     = Nothing
----    twoSum' (x:xs') set = if ((n - x) `member` set) then (Just (x, n - x)) else twoSum' xs' set
--    
--y = (+) <$> (Just 3) <*> (Just 5)
----Control.Applicative (<*>) :: Applicative f => f (a -> b) -> f a -> f b
----Data.Functor (<$>) :: Functor f => (a -> b) -> f a -> f b
--threeSum :: [Int] -> [Int] -> [Int] -> Int -> (Int, Int)
--threeSum xs ys zs n = ((,) <*> a <$> b)
--  where
--    stream = [(n - (x - y),(xi,yi)) | (x, xi) <- (zip xs [0..]), (y,yi) <- (zip ys [0..]) ]
--    m = M.fromList stream
--    a = fst <$> find ((`M.member` m) . fst) (zip zs [0..])
--    Just (Just (b, c)) = (`M.lookup` m) <$> a
    
    
main :: IO () 
main = do
  putStrLn $ show $ twoSum [1..5] [0..3] 6 
  putStrLn $ show $ twoSum [1..5] [0..3] 6 
  putStrLn $ show $ take' 3 [1..5]
  putStrLn $ show $ foldl (flip (:)) [] [1..10]
  putStrLn $ show $ singleNum [1, 2, 3, 4, 4, 6, 2, 1, 2 ]
  putStrLn $ show $ romanToInt [M,M,X,V,I]
  putStrLn $ show $ romanToInt [X,L,I,V]
  --putStrLn $ show $ threeSum [1..5] [0..4] []  3

twoSumFind xs ys n = find (\x -> (n - x) `member` set) xs
  where set = fromList ys
        
twoSumFold ::  [Int] -> [Int] -> Int -> Maybe (Int, Int)
twoSumFold xs ys n = foldr f Nothing xs
  where
    f _ (Just v) = (Just v)
    f x Nothing = if ((n - x) `member` set) then Just (x, n - x) else Nothing
    set = fromList ys
    --f (Just v) _ = Just v



take' :: Int -> [a] -> [a]
take' n xs = foldr step (const []) xs n
  where
    step x g 0 = []
    step x g n = x:g (n-1)
--Prelude uncurry :: (a -> b -> c) -> ((a, b) -> c)
--[1, 2, 3, 4] [-3 10] 14 = (4, 1)
twoSum :: [Int] -> [Int] -> Int -> Maybe (Int, Int)
twoSum xs ys n = ret
  where
    ret = liftM2 (,) x (fmap snd y)
    x = fromMaybe Nothing $ (`M.lookup` m) <$> (fmap fst y)
    y = find ((`M.member` m) . fst) (zipIndex ys)
    m = M.fromList $ zipIndex (map (n -) xs)
    zipIndex xs = zip xs [0..]

    --fromList :: (Ord a) => [(a, b)] -> Map ab
    -- Single Number
--Given an array of integers, every element appears twice except for one. Find that single one
--newtype Input = Int

--singleNum :: [Input] -> Maybe Input
--singleNum' :: [Input] ->  Map Input Int -> Input
-- runtime 2*N, N space
--[1,2,3,4,5,1,2,3,4,5]
type IntMap = M.Map Int Int
singleNum :: [Int] -> Maybe Int
singleNum xs = fmap fst $ find (\(_,x) -> x == 1) $ M.toList m
  where
    m :: IntMap
    m = foldr' f (M.fromList [] :: M.Map Int Int) xs
    f :: Int -> IntMap -> IntMap
    f x m' = (M.insert x (count + 1) m')
      where
        count = fromMaybe 0 (M.lookup x m')
      
--  counts :: Map Input Int
  
  
  
--you have an array of integers, find a subarray with maximum sum, with at least one element

--[-10, 1, 2, 6, -1, 4, -22, 20, 2]
--create 1-length subarray
--if you find a positive number, extend your subarray
--if it's negative, conditionally extend subarray
  -- build second subarray while iterating. only extend first with it if non-negative (at which point create competing subarray starting there)
romanToInt :: [Roman] -> Int
romanToInt [] = 0
romanToInt (I:V:xs) = (4   + romanToInt xs)
romanToInt (I:X:xs) = (9   + romanToInt xs)
romanToInt (X:L:xs) = (40  + romanToInt xs)
romanToInt (X:C:xs) = (90  + romanToInt xs)
romanToInt (C:D:xs) = (400 + romanToInt xs)
romanToInt (C:M:xs) = (900 + romanToInt xs)
romanToInt (I:xs) = 1 + romanToInt xs
romanToInt (V:xs) = 5 + romanToInt xs
romanToInt (X:xs) = 10 + romanToInt xs
romanToInt (L:xs) = 50 + romanToInt xs
romanToInt (C:xs) = 100 + romanToInt xs
romanToInt (D:xs) = 500 + romanToInt xs
romanToInt (M:xs) = 1000 + romanToInt xs
  
data Roman = I | V | X | L | C | D | M
--riMap = M.fromList [('X', 10), ('V', 5), ('I', 1), ('L', 50), ('C', 100), ('D', 500), ('M', 1000)]
--
swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

rotate :: (Show a) => Int -> [a] -> [a]
rotate k xs = (uncurry (++)) $ swap $ splitAt ((k - len) `mod` len) xs 
  where 
    len = length xs
    splitAt :: Int -> [a] -> ([a], [a])
    splitAt k xs = (map fst x, map fst y)
       where (x, y) = span ((<= k) . snd) $ zip xs [0..]
       
--rotate 7 [1, 2, 3]
--[3, 1, 2]
rotateShow = do
  let res =  rotate 3 [1..7] -- [5,6,7,1,2,3,4]
  print $ show res
  print $ show $ rotate 1 [1, 2, 3]
