module Common
    ( assertEq
    , getData
    , splitOn
    , toIntChars
    , fib
    , fibs
    , isPalindrome
    , digits
    , factors
    , numDivisors
    , groupPrimeFactors
    , sumFactors
    , sumFactorsStream
    , cartesianProduct
    ) where

import           Control.Arrow       ((&&&))
import           Control.Monad       (liftM2)
import           Data.Array.ST
import           Data.List           (group, unfoldr)
import           Data.Numbers.Primes (primeFactors)
import           Paths_eulerHaskell

-- | Report if solution is correct and create response.
assertEq :: (Show a, Eq a) => a -> a -> String -> String
assertEq a b str =
    if a /= b
    then "ERROR: " ++ str ++ " answer " ++ show a ++ " not equal to " ++ show b
    else str ++ " = " ++ show a

-- | Read data from text file to an IO String.
getData :: String -> IO String
getData name = do
    file <- getDataFileName $ "data/" ++ name
    readFile file

-- | Split string on delimiter returning a list of strings.
--
-- > splitOn ',' "one, two, three" == ["one","two","three"]
splitOn :: Char -> String -> [String]
splitOn _ [] = [""]
splitOn delimit (c:cs)
   | c == delimit = "" : rest
   | otherwise = (c : head rest) : tail rest
   where
       rest = splitOn delimit cs

-- | Int list of the ascii value each character of string.
--
-- > toIntChars "abc" == [33,34,35]
toIntChars :: String -> [Int]
toIntChars = map (\ c -> fromEnum c - 64)

-- | Return fibonacci number of n.
--
-- > fib 25 == 75025
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

-- | Returns a stream of fibonacci numbers. From https://wiki.haskell.org/The_Fibonacci_sequence
--
-- > drop 20 (take 26 fibs) == [6765,10946,17711,28657,46368,75025]
fibs :: [Integer]
fibs = unfoldr (\(a,b) -> Just (a,(b,a+b))) (0,1)

-- | Returns if list contain a palindrome (read same from and back).
--
-- > isPalindrome [9,0,0,9] == True
isPalindrome :: Eq a => [a] -> Bool
isPalindrome []    = False
isPalindrome [x]   = False
isPalindrome [x,y] = x == y
isPalindrome xs    = head xs == last xs && xs == reverse xs

-- | Integer number to list of digits.
-- From http://stackoverflow.com/questions/3963269/split-a-number-into-its-digits-with-haskell
--
-- > digits 123 == [1,2,3]
digits :: Integer -> [Integer]
digits = reverse . unfoldr (\x -> if x == 0 then Nothing else Just (mod x 10, div x 10))

-- | Returns a tuple group list of prime factors of n [(prime, count), ...].
--
-- > groupPrimeFactors 28 == [(2,2),(7,1)]
groupPrimeFactors :: (Integral a) => a -> [(a, Int)]
groupPrimeFactors = map (head &&& length) . group . primeFactors

-- | List of factors of a number.
--
-- > factors 28 == [1,7,2,14,4,28]
factors :: (Integral a) => a -> [a]
factors = map product . mapM (\(p,m) -> [p^i | i <- [0..m]]) . groupPrimeFactors

-- | List of factors of a number.
--
-- > numDivisors 28 == 6
numDivisors :: Int -> Int
numDivisors n = product $ map ((+1) . length) (group (primeFactors n))

-- | Sum of the factor of n not including n.
--
-- > sumFactors 28 == 28
sumFactors :: (Integral a) => a -> a
sumFactors 0 = 0
sumFactors n = sum (factors n) - n

-- | Stream of sum of factors from 0.
--
-- > drop 25 $ take 30 sumFactorsStream == [6,16,13,28,1]
sumFactorsStream :: (Integral a) => [a]
sumFactorsStream = map sumFactors [0 ..]

-- | Cartesian product of two lists into a list of tuples.
--
-- > cartesianProduct [1..2] [1..2] == [(1,1),(1,2),(2,1),(2,2)]
cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct  = liftM2 (,)

