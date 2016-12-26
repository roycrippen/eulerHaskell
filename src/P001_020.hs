module P001_020 where

import           Common              (assertEq, digits, factors, fib, fibs,
                                      getData, isPalindrome)
import           Control.Monad       (liftM)
import           Data.List           (elemIndices, maximumBy, tails, transpose)
import           Data.Numbers.Primes (primeFactors, primes)
import           Data.Ord            (comparing)


-------------------------------------------------------
-- Euler 001: Multiples of 3 and 5
p001 :: IO ()
p001 = do
    let res = sum [x | x <- [3..999], x `mod` 3 == 0 || x `mod` 5 == 0]
    putStrLn $ assertEq res 233168 "p001"


-------------------------------------------------------
-- Euler 002: Even Fibonacci numbers
p002 :: IO ()
p002 = do
    let fibList = takeWhile (<= 4000000) fibs
        res  = sum . filter even $ fibList
    putStrLn $ assertEq res 4613732 "p002"


-------------------------------------------------------
-- Euler 003: Largest prime factor
p003 :: IO ()
p003 = do
    let res = maximum $ primeFactors 600851475143
    putStrLn $ assertEq res 6857 "p003"


-------------------------------------------------------
-- Euler 004: Largest palindrome product
p004 :: IO ()
p004 = do
    let res = maximum [z | x <- [100..999],
                           y <- [100..999],
                           let z = x * y,
                           (z `mod` 11 == 0) && isPalindrome (show z)]

    putStrLn $ assertEq res 906609 "p004"


-------------------------------------------------------
-- Euler 005: Smallest multiple
p005 :: IO ()
p005 = do
    let res = foldl lcm 1 [1..20]

    putStrLn $ assertEq res 232792560 "p005"


-------------------------------------------------------
-- Euler 006: Sum square difference
p006 :: IO ()
p006 = do
    let sums = sum [1..100]
        sumSquares = sum [x * x | x <- [1..100]]
        res = sums * sums - sumSquares

    putStrLn $ assertEq res 25164150 "p006"


-------------------------------------------------------
-- Euler 007: 10001st prime
p007 :: IO ()
p007 = do
    let res = last $ take 10001 primes
    putStrLn $ assertEq res 104743 "p007"


-------------------------------------------------------
-- Euler 008: Largest product in a series
p008 :: IO ()
p008 = do
    str <- getData "p008.txt"
    let inputStr = filter (/= '\n') str
        chunks =  filter (/= "") (getChunks 13 inputStr)
        res = maximum [productOfDigits xs | xs <- chunks]

    putStrLn $ assertEq res 23514624000 "p008"

-- break into chunks of n length strings
getChunks :: Int -> String -> [String]
getChunks n xs
    | length xs < n = []
    | otherwise = chunk : getChunks n (tail xs)
        where chunk = if '0' `elem` chunk' then "" else chunk'
                where chunk' = take n xs

-- product of digits in a string of digits
productOfDigits :: String -> Integer
productOfDigits str = product $ digits (read str)


-------------------------------------------------------
-- Euler 009: Special Pythagorean triplet
p009 :: IO ()
p009 = do
    let (a,b) = head [(a,b) | a <- [2..500],
                              b <- [a..500],
                              a + b < 1000 && a * a + b * b == (1000 - a - b)^2]
        res = a * b * (1000 - a - b)

    putStrLn $ assertEq res 31875000 "p009"


-------------------------------------------------------
-- Euler 010: Summation of primes
p010 :: IO ()
p010 = do
    let res = sum $ takeWhile (< 2000000) primes
    putStrLn $ assertEq res 142913828922 "p010"


-------------------------------------------------------
-- Euler 011: Largest product in a grid
p011 :: IO ()
p011 = do
    inputStr <- getData "p011.txt"
    let grid = fillGrid inputStr
        res = maximum [prodMax $ direction grid | direction <- [east, south, southEast, southWest]]

    putStrLn $ assertEq res 70600674 "p011"

-- read input data string and fill grid
fillGrid :: String -> [[Int]]
fillGrid = map (map read . words) . lines

-- remove candidates with less than 4 elements
prune :: [[Int]] ->[[Int]]
prune = filter (\ys -> length ys == 4)

-- horizontal candidates
east :: [[Int]] ->[[Int]]
east = prune . concatMap (map (take 4) . tails)

-- vertical candidates
south :: [[Int]] ->[[Int]]
south = east . transpose

-- grab half the diagonals
diagonal :: [[Int]] -> [[Int]]
diagonal = south . zipWith drop [0 ..]

-- east half of diagonals
southEast :: [[Int]] -> [[Int]]
southEast = concatMap diagonal . tails

-- west half of diagonals
southWest :: [[Int]] -> [[Int]]
southWest = southEast . map reverse

-- product maximum of a list of candidates
prodMax :: [[Int]] -> Int
prodMax = maximum . map product


-------------------------------------------------------
-- Euler 012: Highly divisible triangular number
p012 :: IO ()
p012 = do
    let natSums = drop 1 $ scanl (+) 0 [1..]
        res =  head [x | x <- drop 1 natSums, length (factors x) > 500]

    putStrLn $ assertEq res 76576500 "p012"


-------------------------------------------------------
-- Euler 013: Large sum
p013 :: IO ()
p013 = do
    inputStr <- getData "p013.txt"
    let xs = sum $ map (\s -> read s :: Integer) $ lines inputStr
        res = read (take 10 $ show xs) :: Integer

    putStrLn $ assertEq res 5537376230 "p013"


-------------------------------------------------------
-- Euler 014: Longest Collatz sequence
p014 :: IO ()
p014 = do
    let res = solveP014 1000000
    putStrLn $ assertEq res 837799 "p014"
    -- solve 1000 == 871

collatz :: Int -> Int
collatz n
    | n == 1    = n
    | even n    = 1 + collatz (n `div` 2)
    | otherwise = 1 + collatz (3 * n + 1)

maxIndex ::  Ord a => [a] -> Int
maxIndex = fst . maximumBy (comparing snd) . zip [0..]

solveP014 :: Int -> Int
solveP014 n = 2 + head (elemIndices maxCollatz collatzs)
    where collatzs = [collatz x | x <- [2 .. n]]
          maxCollatz = maximum collatzs





-------------------------------------------------------
-- Euler 015:
p015 :: IO ()
p015 = do

    let res = 0
    putStrLn $ assertEq res 0 "p015"

memoizedFib :: Int -> Integer
memoizedFib = (map fib [0 ..] !!)
   where fib 0 = 0
         fib 1 = 1
         fib n = memoizedFib (n-2) + memoizedFib (n-1)



-------------------------------------------------------
-- Euler 016:
p016 :: IO ()
p016 = do
    let res = 0
    putStrLn $ assertEq res 0 "p016"


-------------------------------------------------------
-- Euler 017:
p017 :: IO ()
p017 = do
    let res = 0
    putStrLn $ assertEq res 0 "p017"


-------------------------------------------------------
-- Euler 018:
p018 :: IO ()
p018 = do
    let res = 0
    putStrLn $ assertEq res 0 "p018"


-------------------------------------------------------
-- Euler 019:
p019 :: IO ()
p019 = do
    let res = 0
    putStrLn $ assertEq res 0 "p019"


-------------------------------------------------------
-- Euler 020:
p020 :: IO ()
p020 = do
    let res = 0
    putStrLn $ assertEq res 0 "p020"


solutionsP001_020 :: [IO()]
solutionsP001_020 =
    [ p001, p002, p003, p004, p005, p006, p007, p008, p009, p010
    , p011, p012, p013, p014, p015, p016, p017, p018, p019, p020 ]
