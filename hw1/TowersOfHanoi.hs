module TowersOfHanoi where

type Peg = String
type Move = (Peg, Peg)

------------------------------------------------------------------------------
-- Exercise 5

-- | Return the moves necessary to solve the Towers of Hanoi puzzle
--   with three pegs. For 'hanoi n a b c' n discs stacked in increasing
--   order on peg a must be moved to peg b, using peg c as temporary storage.
--   For example:
--
-- > hanoi 2 "a" "b" "c" == [("a", "c"), ("a", "b"), ("c", "b")]
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a b _ = [(a, b)]
hanoi 2 a b c = [(a, c), (a, b), (c, b)]
hanoi n a b c = hanoi (n-1) a c b ++ hanoi 1 a b c ++ hanoi (n-1) c b a
