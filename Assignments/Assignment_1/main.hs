import qualified Data.Map as Map
import Data.Maybe (isNothing)

-- TASK 1
combineStrings :: [String] -> [String]
combineStrings [] = []
combineStrings (x:xs) = take ((length (x:xs))^2) (mappedStrings ++ combineStrings ys)
    where ys = xs ++ [x]
          mappedStrings = map (x ++) (x:xs)


toLength :: Int -> [String] -> [String]
toLength n xs = [x | x <- combineStrings xs, length x == n]


-- TASK 2
palindrome :: String -> Bool
palindrome x
    | x == reverse x = True
    | otherwise = False


firstHalf :: String -> String
firstHalf x
    | odd (length x) = take oddHalf x
    | otherwise = take evenHalf x
    where oddHalf = (length x + 1) `div` 2
          evenHalf = length x `div` 2


palindromeHalfs :: [String] -> [String]
palindromeHalfs xs = map firstHalf (filter palindrome xs)


-- TASK 3
isTransferLegal :: String -> String -> Int -> Map.Map String Int -> Bool
isTransferLegal from to amount bank
    | isNothing (Map.lookup from bank) = False
    | isNothing (Map.lookup to bank) = False
    | amount < 0 = False
    | bank Map.!from - amount < 0 = False
    | otherwise = True


transfer :: String -> String -> Int -> Map.Map String Int -> Map.Map String Int
transfer from to amount bank
    | not (isTransferLegal from to amount bank) = bank
    | otherwise = Map.insert to addMoney (Map.insert from subtractMoney bank)
    where
        addMoney = bank Map.!to + amount
        subtractMoney = bank Map.!from - amount


main :: IO()
main = do
    putStrLn "Hello, Haskell assignment 1!"
    -- task 1
    let strings = ["a", "b", "cd", "def"]
    putStrLn "\nStrings with given length:"
    print (toLength 5 strings)
    print (toLength 2 strings)
    print (toLength 3 strings)
    -- task 2
    let palindromes = ["abba", "cat", "racecar", "yes", "kayak", "dog"]
    putStrLn "\nPalindrome halfs: "
    print (palindromeHalfs palindromes)
    -- task 3
    let swedbank = Map.fromList[("Bob", 100), ("Mike", 50)]
    let seb = Map.fromList[("Mari", 325), ("Paul", 290)]
    putStrLn "\nResults of legal transfers: "
    print (transfer "Bob" "Mike" 20 swedbank)
    print (transfer "Paul" "Mari" 133 seb)
    putStrLn "Results of illegal transfers: "
    print (transfer "Uku" "Mike" 12 swedbank)
    print (transfer "Paul" "Uku" 12 seb)
    print (transfer "Bob" "Mike" (-12) swedbank)
    print (transfer "Paul" "Mari" 1222 seb)
