module LeapYear (isLeapYear) where

isLeapYear year = divisibleBy 400 || divisibleBy 4 && ( not . divisibleBy $ 100)
        where divisibleBy divider = year `mod` divider == 0
