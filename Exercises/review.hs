import Data.Char (isDigit)
import Data.List (elemIndices, group)


-- Task 1
makeCopies :: (Eq a, Num a, Enum a) => a -> [a]
makeCopies item
    | item == 0 = []
    | otherwise = [item | n <- [1..n]]
    where n = item

multiCopy :: (Enum a, Ord a, Num a) => [a] -> [a]
multiCopy xs
    | length xs <= 2 = xs
    | otherwise = concatMap makeCopies posxs
    where posxs = filter (> 0) xs


-- Task 2

checkBinary :: [Char] -> [[Char]]
checkBinary [] = []
checkBinary (x:xs)
    | x == '0' || x == '1' = checkBinary xs
    | otherwise = [""]

findZeroes :: [Char] -> [Int]
findZeroes = elemIndices '0'

askBinary :: IO ()
askBinary = do
    putStr "Input a binary number: "
    str <- getLine
    if all isDigit str && str /= ""
        then do
            if checkBinary str /= [""]
                then do
                    putStr "Zeroes are at: "
                    print (findZeroes str)
            else do
                putStrLn "Binary numbers can contain only 0s and 1s!"
                askBinary
    else do
        putStrLn "Input can contain only digits!"
        askBinary


-- Task 3
singleLols = foldr (\ x -> (++) [[x]]) []

multiLols :: [[a]] -> [[a]]
multiLols [] = []
multiLols (x:xs) = mappedArrs ++ multiLols xs
    where mappedArrs = map (x ++) xs

drops xs
    | length xs <= 2 = []
    | length xs == 3 = singleLols xs
    | otherwise = multiLols $ group xs


main :: IO()
main = do
    putStrLn "Hello, review!"

    putStrLn "\nTask 1: "
    print (multiCopy [2,3,-5,6])
    print (multiCopy [2,3])

    putStrLn "\nTask 2: "
    askBinary

    putStrLn "\nTask 3:"
    print (drops ["a","f","r","s"])
    print (drops [1,2,3])
    print (drops ['s'])
    print (drops [2,3])


    putStrLn "\nDone!"