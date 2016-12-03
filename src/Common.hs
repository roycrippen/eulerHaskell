module Common
    ( fib
    , assertEq
    ) where

-- test if solution is correct and create response
assertEq :: (Show a, Eq a) => a -> a -> String -> String
assertEq a b str =
    if a /= b
    then "ERROR: " ++ str ++ " answer " ++ show a ++ " not equal to " ++ show b
    else str ++ " = " ++ show a

fib :: Int -> Integer
fib n = fst (fibs n)

fibs :: Int -> (Integer, Integer)
fibs 1 = (1, 0)
fibs n = if even n then (p, q) else (p + q, p)
    where (a, b) = fibs (n `div` 2)
          p = (2 * b + a) * a
          q = a * a + b * b

