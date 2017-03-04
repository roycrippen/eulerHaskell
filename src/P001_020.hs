module P001_020 where

import           Common                      (assertEq, digits, factors, fibs, getData, isPalindrome, numDivisors)
import           Control.Monad               (when)
import           Control.Monad.ST            (runST)
import           Data.Array.ST               (STUArray, newArray, readArray, runSTUArray, writeArray)
import           Data.Array.Unboxed          ((!))
import           Data.Char                   (digitToInt, isSpace)
import           Data.List                   (tails, transpose)
import           Data.Numbers.Primes         (primeFactors, primes)
import           Data.STRef                  (modifySTRef', newSTRef, readSTRef)
import qualified Data.Vector.Unboxed.Mutable as M


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

    let res = a * b * (1000 - a - b)
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
        res =  head [x | x <- drop 1 natSums, numDivisors x > 500]
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
    let res = maxCollatz 1000000
    putStrLn $ assertEq res 837799 "p014"

-- vector based dynamic solution
maxCollatz :: Int -> Int
maxCollatz n = runST $ do
    mVec <- M.replicate (n + 1) 0   -- mutable vector as cache
    idx  <- newSTRef 1              -- mutable loop counting index
    best <- newSTRef (0 :: Int)     -- mutable best collatz score

    let collatz x
            | x == 1 = return 1
            | x > n  = go x
            | otherwise = do
                cached <- M.read mVec x
                if cached == 0 then go x else return cached
                    where go x
                            | even x    = fmap (+ 1) (collatz (x `div` 2))
                            | otherwise = fmap (+ 1) (collatz (3 * x + 1))

        loop = do
            i <- readSTRef idx
            case i of
                  k | k > n     -> return mVec
                    | otherwise -> do
                        val <- M.read mVec i
                        -- add new item to cache
                        when (val == 0) $ do
                            res <- collatz i
                            M.write mVec i res
                            -- save the index of the best result
                            best' <- readSTRef best
                            when (res > best') $ do
                                M.write mVec 0 i
                                modifySTRef' best (const res)
                        modifySTRef' idx (+ 1)
                        loop
    -- start the loop
    loop

    -- done looping, retun answer
    M.read mVec 0

-- alternative array solution
-- similar but slightly slower than vector
maxCollatz' :: Int -> Int
maxCollatz' n = arr' ! 1 where
        arr' =  runSTUArray $ do
            arr  <- newArray (1, n + 1) 0
            idx  <- newSTRef 1
            best <- newSTRef (0 :: Int)

            let collatz x
                    | x == 1 = return 1
                    | x > n  = go x
                    | otherwise = do
                        cached <- readArray arr x
                        if cached == 0 then go x else return cached
                            where go x
                                    | even x    = fmap (+ 1) (collatz (x `div` 2))
                                    | otherwise = fmap (+ 1) (collatz (3 * x + 1))

                loop = do
                    i <- readSTRef idx
                    case i of
                          k | k > n     -> return arr
                            | otherwise -> do
                                val <- readArray arr i
                                -- add new item to cache
                                when (val == 0) $ do
                                    res <- collatz i
                                    writeArray arr i res
                                    -- save the index of the best result
                                    best' <- readSTRef best
                                    when (res > best') $ do
                                        writeArray arr 1 i
                                        modifySTRef' best (const res)
                                modifySTRef' idx (+ 1)
                                loop
            -- start the loop
            loop


-------------------------------------------------------
-- Euler 015:
p015 :: IO ()
p015 = do
    -- C(n,r) = n!/(r!(n-r)!)
    -- 40!/20!*20!
    let fact20 = product [1..20] :: Integer
        fact40 = foldr (*) fact20 [21..40]
        res = fact40 `div` (fact20 * fact20)
    putStrLn $ assertEq res 137846528820 "p015"


-------------------------------------------------------
-- Euler 016:
p016 :: IO ()
p016 = do
    let res = sum . map digitToInt $ show (2^1000)
    putStrLn $ assertEq res 1366 "p016"


-------------------------------------------------------
-- Euler 017:
p017 :: IO ()
p017 = do
    let str = concat [numToWords n | n <- [1..1000]]
        res = length $ filter (not . isSpace) str
    putStrLn $ assertEq res 21124 "p017"

-- number to string wording
numToWords :: Int -> String
numToWords n
    | n <       0 = ""
    | n <      20 = numUntis !!  n
    | n <     100 = numTens  !! (n `div` 10)     ++ " "          ++ numToWords (n `mod` 10)
    | n <    1000 = numUntis !! (n `div` 100)    ++ numAnds n    ++ numToWords (n `mod` 100)
    | n <   10000 = numUntis !! (n `div` 1000)   ++ " thousand " ++ numToWords (n `mod` 1000)
    | otherwise = ""
        where
            numUntis = ["", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven",
                        "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]
            numTens = ["", "ten", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]
            numAnds n = if n `mod` 100 == 0 then " hundred " else " hundred and "



-------------------------------------------------------
-- Euler 018:
p018 :: IO ()
p018 = do
    xss <- getData "p018.txt"

    let fillLists :: String -> [[Int]]
        fillLists = map (map read . words) . lines
        res = sum . head . go . reverse $ fillLists xss
    putStrLn $ assertEq res 1074 "p019"

-- max value of adjacent cells [4,2,3,5] -> [4, 3, 5]
maxPair :: [Int] -> [Int]
maxPair xs = zipWith max xs (drop 1 xs)

-- sum 2 lists [1,2,3] -> [2,3,4] -> [3,5,7]
sumTwoRows :: [Int] -> [Int] -> [Int]
sumTwoRows xs = zipWith (+) (maxPair xs)

-- process the triangle
go :: [[Int]] -> [[Int]]
go xss@(xs:ys:zs) =
    case length xss of
        0 -> []
        1 -> [head xss]
        2 -> [sumTwoRows xs ys]
        _ -> go (sumTwoRows xs ys : zs)


-------------------------------------------------------
-- Euler 019:
p019 :: IO ()
p019 = do
    let res = length $ filter (==1) $ dows months
    putStrLn $ assertEq res 171 "p019"

-- leap years
leap = [1904, 1908, 1912, 1916, 1920, 1924, 1928, 1932, 1936, 1940, 1944, 1948, 1952, 1956, 1960,
        1964, 1968, 1972, 1976, 1980, 1984, 1988, 1992, 1996, 2000, 2004, 2008, 2012, 2016, 2020]

-- days in each month
days y
    | y `elem` leap = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    | otherwise     = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

-- list of days for all months and all years
months :: [Int]
months = [d | y <- [1900..2000], d <- days y]

-- given start day 1 to 7 (1 == Sunday) and number of days, return last day of month
lastDayofMonth :: Int -> Int -> Int
lastDayofMonth seed mDays
        | mDays + seed > 7 = lastDayofMonth seed (mDays - 7)
        | otherwise        = seed + mDays

-- list with start day (1 to 7) for all months and all years
dows :: [Int] -> [Int]
dows xs = drop 12 $ init $ scanl lastDayofMonth 2 xs


-------------------------------------------------------
-- Euler 020:
p020 :: IO ()
p020 = do
    let res = sum . digits $ product [1..100]
    putStrLn $ assertEq res 648 "p020"


-- solution list to send to main for parallel execution
solutionsP001_020 :: [IO()]---------------------
solutionsP001_020 =
    [ p001, p002, p003, p004, p005, p006, p007, p008, p009, p010
    , p011, p012, p013, p014, p015, p016, p017, p018, p019, p020 ]
