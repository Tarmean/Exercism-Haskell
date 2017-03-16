module Meetup (Weekday(..), Schedule(..), meetupDay) where

import Data.Time.Calendar 
import Data.Time.Calendar.WeekDate
import Data.List (find)

data Weekday = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving (Enum)
ordDay :: Weekday -> Int
ordDay = (1+) . fromEnum

data Schedule = First | Second | Third | Fourth | Teenth | Last deriving Enum
schedRange :: Schedule -> [Int]
schedRange Last   = reverse [1..31]
schedRange Teenth = [13..19]
schedRange s      = take 7 [fromEnum  s * 7 + 1..]

dayOfWeek :: Day -> Int
dayOfWeek day = day'
  where (_, _, day') = toWeekDate day

meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day
meetupDay schedule weekday year month = fromJust $ find fittingDay daysInRange
  where 
    fittingDay = (ordDay weekday ==) . dayOfWeek
    daysInRange = fromGregorian year month <$> schedRange schedule
    fromJust (Just r) = r

