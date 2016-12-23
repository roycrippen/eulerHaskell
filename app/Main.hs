module Main where

import           Control.Concurrent.ParallelIO (parallel_, stopGlobalPool)
import           Lib                           (solutions)
import           System.Environment
import           Text.Read                     (readMaybe)


main :: IO ()
main = do
    xs <- getArgs
    case xs of
        [] -> do
                putStrLn "Euler problems in Haskell...\n"
                parallel_ solutions
                stopGlobalPool
                putStrLn "done"
        [x] -> do
                let n = readMaybe x
                case n of
                    (Just x) ->
                        if x > 0 && x <= length solutions
                            then solutions !! (x - 1 )
                            else putStrLn $ "Number not in range. Enter number between 1 and " ++ show (length solutions)
                    none -> putStrLn $ "Not a number. Enter number between 1 and " ++ show (length solutions)
        _ -> putStrLn $ "Enter just one number between 1 and " ++ show (length solutions)

