module P021_040 where

import           Common
import           Control.Monad
import           Control.Parallel.Strategies (rseq)
import qualified Data.Array.Unboxed          as U
import           Data.Choose
import           Data.Function
import           Data.List
import           Data.Maybe
import           Data.Numbers.Primes
import           Data.Ratio
import qualified Data.Set                    as S


------------------------------------------------------------------
-- | Euler 021: Amicable numbers.
p021 :: IO ()
p021 = do
    let res = amicable 10000
    putStrLn $ assertEq res 31626 "p021"

-- True if n is amicable.
isAmicable :: Int -> Bool
isAmicable n = v /= n && sumFactors v == n
    where v = sumFactors n

-- sum of all amicable number from 1 to n
amicable :: Int -> Int
amicable n = foldl' (+) 0 $ filter isAmicable [2,4..n]

------------------------------------------------------------------
-- | Euler 022: Names scores.
p022 :: IO ()
p022 = do
    inputStr <- getData "p022.txt"
    let xs = sort $  splitOn ',' ys
            where ys = filter (/= '\"') inputStr

    let res = sum $ zipWith (\str i -> i * scoreName str) xs [1..]
    putStrLn $ assertEq res 871198282 "p022"

-- score each name in list by index
scoreName :: String -> Int
scoreName = sum . toIntChars

------------------------------------------------------------------
-- > Euler 023: Non-abundant sums.
p023 :: IO ()
p023 = do
    let res = foldl' (+) 0 $ nonAbundants num
    putStrLn $ assertEq res 4179871 "p023"

-- number limit
num :: Int
num = 20161

-- True for each abundant number
arr :: U.UArray Int Bool
arr = U.listArray (1, num) (parMapChunked rseq 500 isAbundant [1..num])
    where isAbundant n = (even n || n `mod` 5 == 0) && sumFactors n > n

-- list of abundant numbers found in array
abundants :: [Int]
abundants = filter (arr U.!) [1..num]

-- is n the sum of two abundant numbers, True or False
isAbundantSum :: Int -> Bool
isAbundantSum n = any (\x -> arr U.! (n - x)) (takeWhile (<= n `div` 2) abundants)

-- list of numbers that are not the sum of two abundant numbers
nonAbundants :: Int -> [Int]
nonAbundants n = filter(not . isAbundantSum) [1..n]

------------------------------------------------------------------
-- > Euler 024: Lexicographic permutations.
p024 :: IO ()
p024 = do
    let res = read $ nthLexPerm 999999 "0123456789"
    putStrLn $ assertEq res 2783915460 "p024"

-- nth lexicographic permutation of a list of items
nthLexPerm :: Ord a => Int -> [a] -> [a]
nthLexPerm n xs
    | length xs == 1 = xs
    | otherwise      = xs !! groupIndex : nthLexPerm withinGroup (filter (\x -> x /= xs !! groupIndex) xs)
            where smallPermutations = factorial (length xs - 1)
                  groupIndex = n `div` smallPermutations
                  withinGroup = n `mod` smallPermutations

factorial :: Int -> Int
factorial n = product [1..n]

-------------------------------------------------------
-- Euler 025:
p025 :: IO ()
p025 = do
    let res  = length $ takeWhile (\x -> length (show x) < 1000) fibs
    putStrLn $ assertEq res 4782 "p025"

fibs2 = Data.Function.fix $ (0:) . scanl (+) 1
digits1 = length . (show :: Integer -> String)

r = fst . head . dropWhile ((1000>) . digits1 . snd) $ zip [0..] fibs2

-------------------------------------------------------
-- Euler 026:
p026 :: IO ()
p026 = do
    let res  = snd $ maximum [ (cycleCnt 1 d 1, d) | d <- takeWhile (< 1000) primes]
    putStrLn $ assertEq res 983 "p026"

cycleCnt :: Integer -> Integer-> Integer-> Integer
cycleCnt n d cnt = if n + 1 == d || (10^n - 1) `mod` d == 0 then cnt else cycleCnt (n+1) d (cnt+1)

------------------------------------------------------------------
-- > Euler 027:
p027 :: IO ()
p027 = do
    let res  = solve27 1000
    putStrLn $ assertEq res (-59231) "p027"

solve27 :: Int -> Int
solve27 n = -(2 * a - 1) * (a^2 - a + 41)
            where a = head (filter (\ x -> x ^ 2 - x + 41 > n) [1 ..]) - 1

------------------------------------------------------------------
-- > Euler 028:
p028 :: IO ()
p028 = do
    let res  = diagSum 1001
    putStrLn $ assertEq res 669171001 "p028"

diagSum n = foldl' f 1  $ takeWhile (<= n) [3,5..]
                where f acc i = acc + 4 * i * i - 6 * (i - 1)

------------------------------------------------------------------
-- > Euler 029:
p029 :: IO ()
p029 = do
    let res  = solve29 100
    putStrLn $ assertEq res 9240 "p029"

solve29 :: Double -> Int
solve29 n = S.size $ S.fromList [b * log a | a <- [2..n], b <- [2..n]]

------------------------------------------------------------------
-- > Euler 030:
p030 :: IO ()
p030 = do
    let res  = 0
    putStrLn $ assertEq res 0 "p030"

------------------------------------------------------------------
-- > Euler 031:
p031 :: IO ()
p031 = do
    let res = 0
    putStrLn $ assertEq res 0 "p031"

------------------------------------------------------------------
-- > Euler 032:
p032 :: IO ()
p032 = do
    let res  = 0
    putStrLn $ assertEq res 0 "p032"

-----------------------------------------------------------------
-- > Euler 033:
p033 :: IO ()
p033 = do
    let res  = 0
    putStrLn $ assertEq res 0 "p033"

------------------------------------------------------------------
-- > Euler 034:
p034 :: IO ()
p034 = do
    let res  = 0
    putStrLn $ assertEq res 0 "p034"

------------------------------------------------------------------
-- > Euler 035:
p035 :: IO ()
p035 = do
    let res  = 0
    putStrLn $ assertEq res 0 "p035"

------------------------------------------------------------------
-- > Euler 036:
p036 :: IO ()
p036 = do
    let res  = 0
    putStrLn $ assertEq res 0 "p036"

------------------------------------------------------------------
-- > Euler 037:
p037 :: IO ()
p037 = do
    let res  = 0
    putStrLn $ assertEq res 0 "p037"

------------------------------------------------------------------
-- > Euler 038:
p038 :: IO ()
p038 = do
    let res  = 0
    putStrLn $ assertEq res 0 "p038"

------------------------------------------------------------------
-- > Euler 039:
p039 :: IO ()
p039 = do
    let res  = 0
    putStrLn $ assertEq res 0 "p039"

------------------------------------------------------------------
-- > Euler 030:
p040 :: IO ()
p040 = do
    let res  = 0
    putStrLn $ assertEq res 0 "p040"

------------------------------------------------------------------
-- > List of project Euler functions.
solutionsP021_040 :: [IO()]
solutionsP021_040 =
    [ p021, p022, p023, p024, p025, p026, p027, p028, p029, p030
    , p031, p032, p033, p034, p035, p036, p037, p038, p039, p040 ]
