import           Common
import           P001_020

palindromTest = do
    print $ not (isPalindrome [1,2,3])
    print $ not (isPalindrome [1])
    print $ not (isPalindrome [1])
    print $ isPalindrome [1,1]
    print $ isPalindrome [9,0,0,9]

collatzTest = maxCollatz 1023 == maxCollatz' 1023

main :: IO ()
main = do
    palindromTest
    putStrLn $ "maxCollatz test = " ++ show collatzTest


