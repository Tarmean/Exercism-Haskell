module Clock (clockHour, clockMin, fromHourMin, toString) where
import Control.Applicative (liftA2)
import Text.Printf

newtype Clock = Clock { getMin :: Int } deriving (Show, Eq)


wrap = flip mod (24 * 60) 
instance Num Clock where 
    negate = Clock . ((24 * 60) -) . getMin
    (+) a b = Clock . wrap $ (getMin a) +  (getMin b)
    (*) a b = Clock . wrap $ (getMin a) *  (getMin b)
    abs = undefined
    signum = undefined
    fromInteger = Clock . Prelude.fromInteger

-- The task is to create the data type `Clock`, with `Eq`,
-- `Show` and `Num` instances, and implement the functions below.
-- The function `fromInteger`, from `Num`, must converts minutes
-- to 24 hour clock time. It is not necessary to have a sensible
-- implementation of `abs` or `signum`.

clockHour :: Clock -> Int
clockHour = flip div 60 . getMin

clockMin :: Clock -> Int
clockMin = flip mod 60 . getMin

fromHourMin :: Int -> Int -> Clock
fromHourMin h m = Clock . wrap $ h * 60 + m

toString :: Clock -> String
toString = liftA2 (printf "%02i:%02i") clockHour clockMin

