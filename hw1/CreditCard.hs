module CreditCard where

-------------------------------------------------------------------------------
-- Exercise 1

-- | Return the digits of a positive integer in reversed order.
toDigitsRev :: Integer -> [Integer]
toDigitsRev n | n < 0     = []
              | n < 10    = [n]
              | otherwise = r : toDigitsRev q
  where (q, r) = n `quotRem` 10

-- | Return the digits of a positive integer. For example:
--   
-- > toDigits 1234 == [1, 2, 3, 4]
toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

-------------------------------------------------------------------------------
-- Exercise 2

-- | Double every second number in a list, beginning from the right.
--   For example:
--
-- > doubleEveryOther [8, 7, 6, 5] == [16, 7, 12, 5]
doubleEveryOther :: Num a => [a] -> [a]
doubleEveryOther = reverse . doubleEveryOtherRev . reverse

-- | Double every second number in a list, beginning from the left.
--   For example:
--
-- > doubleEveryOtherRev [5, 6, 7, 8] == [5, 12, 7, 16]
doubleEveryOtherRev :: Num a => [a] -> [a]
doubleEveryOtherRev (x:y:ys) = x : 2*y : doubleEveryOtherRev ys
doubleEveryOtherRev xs       = xs

-------------------------------------------------------------------------------
-- Exercise 3

-- | Compute the sum of all digits of a list of one- and two-digit numbers.
--   For example:
--
-- > sumDigits [16, 7, 12, 5] == 1 + 6 + 7 + 1 + 2 + 5 == 22
sumDigits :: [Integer] -> Integer
sumDigits = sum . map sumDigits'

-- | Compute the sum of digits of a one- or two-digit number.
sumDigits' :: Integer -> Integer
sumDigits' n = q + r
  where (q, r) = n `quotRem` 10

-------------------------------------------------------------------------------
-- Exercise 4

-- | Check whether a number can be a valid credit card number.
--   For example:
--
-- > validate 4012888888881881 == True
-- > validate 4012888888881882 == False 
validate :: Integer -> Bool
validate = (== 0) . (`rem` 10) . sumDigits . doubleEveryOtherRev . toDigitsRev
