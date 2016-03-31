import Test.QuickCheck

                 
--prop_add x y =  x == y
--prop_revapp :: (Ord a, Num a) => [a] -> [a] -> Bool

prop_revapp :: [Int] -> [Int] -> Bool
prop_revapp xs ys = reverse (xs++ys) == reverse ys ++ reverse xs

allNotNull f xs = (not $ null xs) && all f xs

isHex ('0':'x':xs) = allNotNull (`elem` hexChars) xs
  where
    hexChars = ['0'..'9'] ++ ['a'..'f'] ++ ['A'..'F']
isHex _ = False

isDigit = (`elem` "0123456789")

isNum :: String -> Bool
isNum n = (isHex n) || isInt l && (rOkay r || null r')
  where
    
    dropChar c = dropWhile (== c)
    trim = reverse .  trim' . reverse . trim'
    trim' = dropChar ' '
    (l, r') = span isDigit $ dropChar '-' (trim n)
    r = dropChar '.' r'
    isInt xs = allNotNull isDigit xs
    rOkay   xs      = (isInt xs) || (eNum xs) || (isInt xl) && (eNum xr)
       where
         (xl, xr) = span isDigit xs
         eNum ('e':'-':xs) = isInt xs
         eNum ('e':xs)  = isInt xs
         eNum _ = False
    
prop_isnum_double :: Double -> Bool
prop_isnum_double = isNum . show

prop_isnum_int :: Int -> Bool
prop_isnum_int = isNum . show

main = do
  putStrLn "Failisms"
  putStrLn $ show $ isNum ".99"
  putStrLn $ show $ isNum "0."
  putStrLn $ show $ isNum "0x"
  putStrLn $ show $ isNum "ABC"
  putStrLn $ show $ isNum "0xz1"
  putStrLn "Truisms"
  putStrLn $ show $ isNum "0xA1"
  putStrLn $ show $ isNum "0xa"
  putStrLn $ show $ isNum "0x142"
  quickCheckWith    stdArgs { maxSuccess = 5000 } prop_isnum_int
  quickCheckWith    stdArgs { maxSuccess = 5000 } prop_isnum_double
--main = putStrLn $ show $ prop_add 1 2

-- it's not possible to create an aribtrary of a typeclass
-- (becasue it's a type constructor not a value constructor.)
--prop_RevRev xs = reverse (reverse xs) == xs
--  where types = xs::[Int]
--instance Arbitrary (Num a) where
--  arbitrary = oneof [choose (minBound :: Int, maxBound :: Int),
--                     choose (minBound :: Double, maxBound :: Double)]
