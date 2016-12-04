module Common
    ( fib
    , assertEq
    , isPalindrome
    , digits
    ) where

import qualified Data.List as L

-- test if solution is correct and create response
assertEq :: (Show a, Eq a) => a -> a -> String -> String
assertEq a b str =
    if a /= b
    then "ERROR: " ++ str ++ " answer " ++ show a ++ " not equal to " ++ show b
    else str ++ " = " ++ show a

-- pretty quick individaul fibonacci function
fib :: Int -> Integer
fib n = fst (fibs n)

fibs :: Int -> (Integer, Integer)
fibs 1 = (1, 0)
fibs n = if even n then (p, q) else (p + q, p)
    where (a, b) = fibs (n `div` 2)
          p = (2 * b + a) * a
          q = a * a + b * b

-- does list contain a palindrome; read same from and back
-- [9,0,0,9] == True
isPalindrome :: Eq a => [a] -> Bool
isPalindrome []    = False
isPalindrome [x]   = False
isPalindrome [x,y] = x == y
isPalindrome xs    = head xs == last xs && xs == reverse xs

-- Integer number to list of digits
-- from http://stackoverflow.com/questions/3963269/split-a-number-into-its-digits-with-haskell
digits :: Integer -> [Integer]
digits = reverse . L.unfoldr (\x -> if x == 0 then Nothing else Just (mod x 10, div x 10))
