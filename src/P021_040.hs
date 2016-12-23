module P021_040 where

import           Common (assertEq, sumFactors, sumFactorsList)


-------------------------------------------------------
-- Euler 021: Amicable numbers
p021 :: IO ()
p021 = do
    let res = sum $ amics 10000
    putStrLn $ assertEq res 31626 "p021"

amics :: Integer -> [Integer]
amics n = map snd $ filter predicate xs
            where xs = zip [0..] (sumFactorsList n)
                  predicate (i, item) = item < n && i /= item && i == snd (xs !! fromIntegral item)


-------------------------------------------------------
-- Euler 022:
p022 :: IO ()
p022 = do
    let res  = 0
    putStrLn $ assertEq res 0 "p022"


-------------------------------------------------------
-- Euler 023:
p023 :: IO ()
p023 = do
    let res  = 0
    putStrLn $ assertEq res 0 "p023"


-------------------------------------------------------
-- Euler 024:












p024 :: IO ()
p024 = do
    let res  = 0
    putStrLn $ assertEq res 0 "p024"


-------------------------------------------------------
-- Euler 025:
p025 :: IO ()
p025 = do
    let res  = 0
    putStrLn $ assertEq res 0 "p025"


-------------------------------------------------------
-- Euler 026:
p026 :: IO ()
p026 = do
    let res  = 0
    putStrLn $ assertEq res 0 "p026"


-------------------------------------------------------
-- Euler 027:
p027 :: IO ()
p027 = do
    let res  = 0
    putStrLn $ assertEq res 0 "p027"


-------------------------------------------------------
-- Euler 028:
p028 :: IO ()
p028 = do
    let res  = 0
    putStrLn $ assertEq res 0 "p028"


-------------------------------------------------------
-- Euler 029:
p029 :: IO ()
p029 = do
    let res  = 0
    putStrLn $ assertEq res 0 "p029"


-------------------------------------------------------
-- Euler 030:
p030 :: IO ()
p030 = do
    let res  = 0
    putStrLn $ assertEq res 0 "p030"


-------------------------------------------------------
-- Euler 031:
p031 :: IO ()
p031 = do
    let res = 0
    putStrLn $ assertEq res 0 "p031"


-------------------------------------------------------
-- Euler 032:
p032 :: IO ()
p032 = do
    let res  = 0
    putStrLn $ assertEq res 0 "p032"


-------------------------------------------------------
-- Euler 033:
p033 :: IO ()
p033 = do
    let res  = 0
    putStrLn $ assertEq res 0 "p033"


-------------------------------------------------------
-- Euler 034:
p034 :: IO ()
p034 = do
    let res  = 0
    putStrLn $ assertEq res 0 "p034"


-------------------------------------------------------
-- Euler 035:
p035 :: IO ()
p035 = do
    let res  = 0
    putStrLn $ assertEq res 0 "p035"


-------------------------------------------------------
-- Euler 036:
p036 :: IO ()
p036 = do
    let res  = 0
    putStrLn $ assertEq res 0 "p036"


-------------------------------------------------------
-- Euler 037:
p037 :: IO ()
p037 = do
    let res  = 0
    putStrLn $ assertEq res 0 "p037"


-------------------------------------------------------
-- Euler 038:
p038 :: IO ()
p038 = do
    let res  = 0
    putStrLn $ assertEq res 0 "p038"


-------------------------------------------------------
-- Euler 039:
p039 :: IO ()
p039 = do
    let res  = 0
    putStrLn $ assertEq res 0 "p039"


-------------------------------------------------------
-- Euler 030:
p040 :: IO ()
p040 = do
    let res  = 0
    putStrLn $ assertEq res 0 "p040"


solutionsP021_040 :: [IO()]
solutionsP021_040 = [ p021, p022, p023, p024, p025, p026, p027, p028, p029, p030
                    , p031, p032, p033, p034, p035, p036, p037, p038, p039, p040 ]