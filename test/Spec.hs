import           Common

palindromTest = do
    print $ not (isPalindrome [1,2,3])
    print $ not (isPalindrome [1])
    print $ not (isPalindrome [1])
    print $ isPalindrome [1,1]
    print $ isPalindrome [9,0,0,9]

main :: IO ()
main = palindromTest


