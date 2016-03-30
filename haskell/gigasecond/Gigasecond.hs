module Gigasecond
       where

--import qualified Data.Time.Clock as C

import Data.Time.Calendar
import Data.Time.Clock

fromDay day = utctDay $ addUTCTime (10 ^ 9) $ UTCTime day 0
--fromDay day = addUTCTime (10 ^ 9) $ UTCTime day 0
--fromDay :: C.UTCTime -> C.UTCTime
--fromDay t = C.addUTCTime (C.secondsToDiffTime 10^9) t
--year * (gig `div` year)
--fromSeconds gig = sum $ map fst $ scanl [year, day, hour, minute] (0, gig)
--  where
--    f n (_, t) = (t `div` n, t `mod` n)
--    year =   365 * day
--    day  =   24 * hour
--    hour =   60 * minute
--    minute = 60
