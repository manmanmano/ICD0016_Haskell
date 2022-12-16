import Data.List (group, elemIndices)

-- Task 2
removeZeroes :: [Integer] -> [Integer]
removeZeroes [] = []
removeZeroes (x:xs) 
    | x == 1 = removeZeroes xs ++ [x]
    | otherwise = removeZeroes drop0
    where drop0 = tail (x:xs)

removeOnes :: [Integer] -> [Integer]
removeOnes [] = []
removeOnes (x:xs)
    | x == 0 = removeOnes xs ++ [x]
    | otherwise = removeOnes drop1
    where drop1 = tail (x:xs)

checkOnes xs
    | length ones == 1 = noOnes 
    | otherwise = beforeOne ++ [1] ++ afterOne
    where 
        ones = elemIndices 1 xs
        safeOneIdx = head $ take 1 (drop 1 ones)
        noOnes = removeOnes xs
        beforeOne = take (safeOneIdx - 1) noOnes
        afterOne = drop (safeOneIdx - 1) noOnes 


digits :: [Integer] -> [Integer]
digits xs
    | length xs < 5 = removeZeroes xs
    | length xs == 5 = xs
    | otherwise = checkOnes xs


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
    putStrLn "Hello, exam!"

    putStrLn "\nTask 2:"
    print (digits [0,1,1,0,1,0])
    print (digits [1,0])
    print (digits [1,0,1,0,1])
    print (digits [0,0,1,0,0,0,0])

    putStrLn "\nTask 3: "
    print (drops ["a", "f", "r","s"])
    print (drops [1,2,3])
    print (drops ['s'])
    print (drops [2,3])


    putStrLn "\nDone!"