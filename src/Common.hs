module Common
    ( fib
    , fibs
    , assertEq
    , isPalindrome
    , digits
    , factors
    , groupPrimeFactors
    ) where

import           Control.Arrow       ((&&&))
import           Data.List           (group, unfoldr)
import           Data.Numbers.Primes (primeFactors)

-- test if solution is correct and create response
assertEq :: (Show a, Eq a) => a -> a -> String -> String
assertEq a b str =
    if a /= b
    then "ERROR: " ++ str ++ " answer " ++ show a ++ " not equal to " ++ show b
    else str ++ " = " ++ show a


-- pretty quick individaul fibonacci function
fib :: Int -> Integer
fib n
    | n < 1 = 0
    | otherwise = fst (fib' n)

fib' :: Int -> (Integer, Integer)
fib' 1 = (1, 0)
fib' n = if even n then (p, q) else (p + q, p)
    where (a, b) = fib' (n `div` 2)
          p = (2 * b + a) * a
          q = a * a + b * b

-- finite list of fibonacci numbers
-- from https://wiki.haskell.org/The_Fibonacci_sequence
fibs = unfoldr (\(a,b) -> Just (a,(b,a+b))) (0,1)


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
digits = reverse . unfoldr (\x -> if x == 0 then Nothing else Just (mod x 10, div x 10))

-- tupled group primeFactors [(prime, count), ...], groupPrimeFactors 28 == [(2,2),(7,1)]
groupPrimeFactors :: Integer -> [(Integer, Int)]
groupPrimeFactors = map (head &&& length) . group . primeFactors

-- list of factors of a number, factors 28 == [1,7,2,14,4,28]
factors :: Integer -> [Integer]
factors = map product . mapM (\(p,m)-> [p^i | i<-[0..m]]) . groupPrimeFactors
