module LeapYear where

isLeapYear x = (ediv 4) && not ((ediv 100) && not (ediv 400))
 where ediv y = (x `mod` y) == 0

