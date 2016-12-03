module Main where

import           Control.Concurrent.ParallelIO (parallel_, stopGlobalPool)
import           Lib                           (solutions)


main :: IO ()
main = do
    putStrLn "Euler problems in Haskell...\n"
    parallel_ solutions
    stopGlobalPool
    putStrLn "done"
