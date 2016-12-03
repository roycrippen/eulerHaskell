module P001_020 where

import           Common              (assertEq, fib)
import           Control.Monad       (liftM)
import           Data.Numbers.Primes (primeFactors)


-- Euler 001: Multiples of 3 and 5
p001 :: IO ()
p001 = do
    let res = sum [x | x <- [3..999], x `mod` 3 == 0 || x `mod` 5 == 0]
    putStrLn $ assertEq res 233168 "p001"


-- Euler 002: Even Fibonacci numbers
p002 :: IO ()
p002 = do
    let fibs = takeWhile (< 4000000) [fib x | x <- [1..]]
    let res = sum $ filter (\x -> x `mod` 2 == 0) fibs
    putStrLn $ assertEq res 4613732 "p002"


-- Euler 003: Largest prime factor
p003 :: IO ()
p003 = do
    let res = maximum $ primeFactors 600851475143
    putStrLn $ assertEq res 6857 "p003"

-- Euler 004:
p004 :: IO ()
p004 = do
    let res = 0
    putStrLn $ assertEq res 0 "p004"

-- Euler 00:
p00 :: IO ()
p00 = do
    let res = 0
    putStrLn $ assertEq res 0 "p00"


solutionsP001_020 :: [IO()]
solutionsP001_020 = [p001, p002, p003, p004]
