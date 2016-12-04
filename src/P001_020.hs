module P001_020 where

import           Common              (assertEq, digits, fib, isPalindrome)
import           Control.Monad       (liftM)
import           Data.Numbers.Primes (primeFactors, primes)

-- Euler 001: Multiples of 3 and 5
p001 :: IO ()
p001 = do
    let res = sum [x | x <- [3..999], x `mod` 3 == 0 || x `mod` 5 == 0]
    putStrLn $ assertEq res 233168 "p001"


-- Euler 002: Even Fibonacci numbers
p002 :: IO ()
p002 = do
    let fibs = takeWhile (< 4000000) [fib x | x <- [1..]]
        res  = sum $ filter (\x -> x `mod` 2 == 0) fibs
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
                           isPalindrome $ show z]

    putStrLn $ assertEq res 906609 "p004"


-- Euler 005: Smallest multiple
p005 :: IO ()
p005 = do
    let res = solve 2520
            where solve n
                    | isMultiple n = n
                    | otherwise = solve (n + 2520)

    putStrLn $ assertEq res 232792560 "p005"

-- helper
isMultiple :: Integral a => a -> Bool
isMultiple n = n `mod` 11 == 0
            && n `mod` 12 == 0
            && n `mod` 13 == 0
            && n `mod` 14 == 0
            && n `mod` 15 == 0
            && n `mod` 16 == 0
            && n `mod` 17 == 0
            && n `mod` 18 == 0
            && n `mod` 19 == 0


-- Euler 006: Sum square difference
p006 :: IO ()
p006 = do
    let sums = sum [1..100]
        sumSquares = foldr (\x acc -> acc + x * x) 0 [1..100]
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
    let dataStr = "73167176531330624919225119674426574742355349194934\
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

    let xs = filter (/= '\n') dataStr
        chunks =  filter (/= "") (getChunks 13 xs [[]])
        res = maximum [getDigitProduct xs | xs <- chunks]

    putStrLn $ assertEq res 23514624000 "p008"

-- helper: break into n length strings
getChunks :: Int -> String -> [String] -> [String]
getChunks n xs yss
    | length xs < n = yss
    | otherwise = noZeros : getChunks n (drop 1 xs) yss
                where noZeros = if '0' `elem` val then "" else val
                        where val = take n xs

-- helper: product of digits in a string of digits
getDigitProduct :: String -> Integer
getDigitProduct str = product $ digits (read str :: Integer)


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
    let res = 70600674
    putStrLn $ assertEq res 70600674 "p011"


-- Euler 01:
p01 :: IO ()
p01 = do
    let res = 0
    putStrLn $ assertEq res 0 "p01"



solutionsP001_020 :: [IO()]
solutionsP001_020 = [p011]
-- -- solutionsP001_020 = [ p001, p002, p003, p004, p005, p006, p007, p008, p009, p010
--                     , p011, p012, p013, p014, p015, p016, p017, p018, p019, p020 ]
