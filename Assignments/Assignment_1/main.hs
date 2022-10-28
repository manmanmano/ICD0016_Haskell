-- TASK 1
combineStrings :: [String] -> [String]
combineStrings [] = []
combineStrings (x:xs) = take (length (x:xs)^2) (mappedStrings ++ combineStrings ys)
    where ys = xs ++ [x]
          mappedStrings = map (x ++) (x:xs)


toLength :: Int -> [String] -> [String]
toLength n xs = [x | x <- combineStrings xs, length x == n]


--TASK 2
palindrome :: String -> Bool
palindrome string
    | string == reverse string = True
    | otherwise = False


firstHalf :: String -> String
firstHalf x 
    | odd (length x) = take oddHalf x
    | otherwise = take evenHalf x
    where oddHalf = (length x + 1) `div` 2
          evenHalf = length x `div` 2


palindromeHalfs :: [String] -> [String]
palindromeHalfs xs = map firstHalf (filter palindrome xs)


--TASK 3



main :: IO()
main = do
    putStrLn "Hello, Assignment 1!"
    -- task 1
    let strings = ["a", "b", "cd", "def"]
    putStrLn "\nStrings with given length:"
    print (toLength 5 strings)
    print (toLength 2 strings)
    print (toLength 3 strings)
    --task 2
    let palindromes = ["abba", "cat", "racecar", "yes", "kayak", "dog"]
    putStrLn "\nPalindrome halfs: "
    print (palindromeHalfs palindromes)
