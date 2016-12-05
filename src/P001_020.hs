module P001_020 where

import           Common              (assertEq, digits, fib, fibs, isPalindrome)
import           Control.Monad       (liftM)
import           Data.List           (tails, transpose)
import           Data.Numbers.Primes (primeFactors, primes)

-- Euler 001: Multiples of 3 and 5
p001 :: IO ()
p001 = do
    let res = sum [x | x <- [3..999], x `mod` 3 == 0 || x `mod` 5 == 0]
    putStrLn $ assertEq res 233168 "p001"


-- Euler 002: Even Fibonacci numbers
p002 :: IO ()
p002 = do
    let fibList = takeWhile (<= 4000000) fibs
        res  = sum . filter even $ fibList
    putStrLn $ assertEq res 4613732 "p002"


-- Euler 003: Largest prime factor
p003 :: IO ()
p003 = do
    let res = maximum $ primeFactors 600851475143
    putStrLn $ assertEq res 6857 "p003"


-- Euler 004: Largest palindrome product
p004 :: IO ()
p004 = do
    let res = maximum [z | x <- [100..999],
                           y <- [100..999],
                           let z = x * y,
                           (z `mod` 11 == 0) && isPalindrome (show z)]

    putStrLn $ assertEq res 906609 "p004"


-- Euler 005: Smallest multiple
p005 :: IO ()
p005 = do
    let res = foldl lcm 1 [1..20]

    putStrLn $ assertEq res 232792560 "p005"


-- Euler 006: Sum square difference
p006 :: IO ()
p006 = do
    let sums = sum [1..100]
        sumSquares = sum [x * x | x <- [1..100]]
        res = sums * sums - sumSquares

    putStrLn $ assertEq res 25164150 "p006"


-- Euler 007: 10001st prime
p007 :: IO ()
p007 = do
    let res = last $ take 10001 primes
    putStrLn $ assertEq res 104743 "p007"


-- Euler 008: Largest product in a series
p008 :: IO ()
p008 = do
    let inputStr = "\
        \73167176531330624919225119674426574742355349194934\
        \96983520312774506326239578318016984801869478851843\
        \85861560789112949495459501737958331952853208805511\
        \12540698747158523863050715693290963295227443043557\
        \66896648950445244523161731856403098711121722383113\
        \62229893423380308135336276614282806444486645238749\
        \30358907296290491560440772390713810515859307960866\
        \70172427121883998797908792274921901699720888093776\
        \65727333001053367881220235421809751254540594752243\
        \52584907711670556013604839586446706324415722155397\
        \53697817977846174064955149290862569321978468622482\
        \83972241375657056057490261407972968652414535100474\
        \82166370484403199890008895243450658541227588666881\
        \16427171479924442928230863465674813919123162824586\
        \17866458359124566529476545682848912883142607690042\
        \24219022671055626321111109370544217506941658960408\
        \07198403850962455444362981230987879927244284909188\
        \84580156166097919133875499200524063689912560717606\
        \05886116467109405077541002256983155200055935729725\
        \71636269561882670428252483600823257530420752963450"

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

        -- solution
        chunks =  filter (/= "") (getChunks 13 inputStr)
        res = maximum [productOfDigits xs | xs <- chunks]

    putStrLn $ assertEq res 23514624000 "p008"


-- Euler 009: Special Pythagorean triplet
p009 :: IO ()
p009 = do
    let (a,b) = head [(a,b) | a <- [2..500],
                              b <- [a..500],
                              a + b < 1000 && a * a + b * b == (1000 - a - b)^2]
        res = a * b * (1000 - a - b)

    putStrLn $ assertEq res 31875000 "p009"


-- Euler 010: Summation of primes
p010 :: IO ()
p010 = do
    let res = sum $ takeWhile (< 2000000) primes
    putStrLn $ assertEq res 142913828922 "p010"


-- Euler 011: Largest product in a grid
p011 :: IO ()
p011 = do
    let grid = fillGrid inputStrP011
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

inputStrP011 :: String
inputStrP011 =  "\
    \08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08\n\
    \49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00\n\
    \81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65\n\
    \52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91\n\
    \22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80\n\
    \24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50\n\
    \32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70\n\
    \67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21\n\
    \24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72\n\
    \21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95\n\
    \78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92\n\
    \16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57\n\
    \86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58\n\
    \19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40\n\
    \04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66\n\
    \88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69\n\
    \04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36\n\
    \20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16\n\
    \20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54\n\
    \01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48"


-- Euler 012: Highly divisible triangular number
p012 :: IO ()
p012 = do
    let res = 76576500
    putStrLn $ assertEq res 76576500 "p012"


-- Euler 013:
p013 :: IO ()
p013 = do
    let res = 0
    putStrLn $ assertEq res 0 "p013"


-- Euler 014:
p014 :: IO ()
p014 = do
    let res = 0
    putStrLn $ assertEq res 0 "p014"


-- Euler 015:
p015 :: IO ()
p015 = do
    let res = 0
    putStrLn $ assertEq res 0 "p015"


-- Euler 016:
p016 :: IO ()
p016 = do
    let res = 0
    putStrLn $ assertEq res 0 "p016"


-- Euler 017:
p017 :: IO ()
p017 = do
    let res = 0
    putStrLn $ assertEq res 0 "p017"


-- Euler 018:
p018 :: IO ()
p018 = do
    let res = 0
    putStrLn $ assertEq res 0 "p018"


-- Euler 019:
p019 :: IO ()
p019 = do
    let res = 0
    putStrLn $ assertEq res 0 "p019"


-- Euler 020:
p020 :: IO ()
p020 = do
    let res = 0
    putStrLn $ assertEq res 0 "p020"


solutionsP001_020 :: [IO()]
solutionsP001_020 = [ p001, p002, p003, p004, p005, p006, p007, p008, p009, p010
                    , p011, p012, p013, p014, p015, p016, p017, p018, p019, p020 ]
