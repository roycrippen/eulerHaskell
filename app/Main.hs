module Main where

import           Control.Concurrent.ParallelIO (parallel_, stopGlobalPool)
import           Lib                           (solutions)
import           System.Environment
import           Text.Read                     (readMaybe)


main :: IO ()
main = do
    let msg = "Enter a number between 1 and " ++ show (length solutions)
    xs <- getArgs
    case xs of
        [] -> do
                putStrLn "Euler problems in Haskell...\n"
                parallel_ $ reverse solutions
                stopGlobalPool
                putStrLn "done"
        [x] -> do
                let n = readMaybe x
                case n of
                    (Just x) ->
                        if x > 0 && x <= length solutions
                            then solutions !! (x - 1)
                            else putStrLn $ "Number not in range. " ++ msg
                    none -> putStrLn $ "Not a number" ++ msg
        _ -> putStrLn $ "One arg only. " ++ msg

