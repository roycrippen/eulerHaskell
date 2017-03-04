module P021_040 where
    -- ( p021, p022, p023, p024, p025, p026, p027, p028, p029, p030
    -- , p031, p032, p033, p034, p035, p036, p037, p038, p039, p040
    -- , solutionsP021_040
    -- ) where

import           Common             (assertEq, getData, splitOn, sumFactors, toIntChars)
import qualified Data.Array.Unboxed as U
import           Data.Choose
import           Data.List          (sort)

-- | Euler 021: Amicable numbers.
p021 :: IO ()
p021 = do
    let res = amics 10000
    putStrLn $ assertEq res 31626 "p021"

-- True if n is amicable.
isAmic :: Integer -> Bool
isAmic n = v /= n && sumFactors v == n
    where v = sumFactors n

-- sum of all amicable number from 1 to n
amics :: Integer -> Integer
amics n = sum $ filter isAmic [2,4..n]

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

-- > Euler 023: Non-abundant sums.
p023 :: IO ()
p023 = do
    let num :: Int
        num = 28123
    let res  = 0
    putStrLn $ assertEq res 0 "p023"

-- True is n is abundant
isAbundant :: Int -> Bool
isAbundant n = sumFactors n > n

-- all True for each abundant number
fillArray :: Int -> U.UArray Int Bool
fillArray n = U.listArray (1, n) $ map isAbundant [1..n]

-- list of abundant numbers in array
fillAbundants :: U.UArray Int Bool -> [Int]
fillAbundants arrAll = filter (arrAll U.!) [1..n]
    where n = snd $ U.bounds arrAll

abunArr = fillArray 100
abuns = fillAbundants abunArr

first = take (length abuns `div` 2) abuns
rest = drop (length abuns `div` 2) abuns

-- nonSums = [x-y | x<- abuns, y <- [1..num] ,x-y> 0 , not $ arrAll U.! (x-y)]               -- arrAll U.! (x+y)]



-- > Euler 024:
p024 :: IO ()
p024 = do
    let res  = 0
    putStrLn $ assertEq res 0 "p024"

-- > Euler 025:
p025 :: IO ()
p025 = do
    let res  = 0
    putStrLn $ assertEq res 0 "p025"

-- > Euler 026:
p026 :: IO ()
p026 = do
    let res  = 0
    putStrLn $ assertEq res 0 "p026"

-- > Euler 027:
p027 :: IO ()
p027 = do
    let res  = 0
    putStrLn $ assertEq res 0 "p027"

-- > Euler 028:
p028 :: IO ()
p028 = do
    let res  = 0
    putStrLn $ assertEq res 0 "p028"

-- > Euler 029:
p029 :: IO ()
p029 = do
    let res  = 0
    putStrLn $ assertEq res 0 "p029"

-- > Euler 030:
p030 :: IO ()
p030 = do
    let res  = 0
    putStrLn $ assertEq res 0 "p030"

-- > Euler 031:
p031 :: IO ()
p031 = do
    let res = 0
    putStrLn $ assertEq res 0 "p031"

-- > Euler 032:
p032 :: IO ()
p032 = do
    let res  = 0
    putStrLn $ assertEq res 0 "p032"

-- > Euler 033:
p033 :: IO ()
p033 = do
    let res  = 0
    putStrLn $ assertEq res 0 "p033"

-- > Euler 034:
p034 :: IO ()
p034 = do
    let res  = 0
    putStrLn $ assertEq res 0 "p034"

-- > Euler 035:
p035 :: IO ()
p035 = do
    let res  = 0
    putStrLn $ assertEq res 0 "p035"

-- > Euler 036:
p036 :: IO ()
p036 = do
    let res  = 0
    putStrLn $ assertEq res 0 "p036"

-- > Euler 037:
p037 :: IO ()
p037 = do
    let res  = 0
    putStrLn $ assertEq res 0 "p037"

-- > Euler 038:
p038 :: IO ()
p038 = do
    let res  = 0
    putStrLn $ assertEq res 0 "p038"

-- > Euler 039:
p039 :: IO ()
p039 = do
    let res  = 0
    putStrLn $ assertEq res 0 "p039"

-- > Euler 030:
p040 :: IO ()
p040 = do
    let res  = 0
    putStrLn $ assertEq res 0 "p040"

-- > List of project Euler functions.
solutionsP021_040 :: [IO()]
solutionsP021_040 =
    [ p021, p022, p023, p024, p025, p026, p027, p028, p029, p030
    , p031, p032, p033, p034, p035, p036, p037, p038, p039, p040 ]
