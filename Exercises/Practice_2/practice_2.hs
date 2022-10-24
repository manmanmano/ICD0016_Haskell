-- 1. intersect two lists and print out 
intersectLists :: [Integer] -> [Integer] -> [Integer]
intersectLists [] _ = []
intersectLists (x:xs) ys
    | x `elem` ys = x : intersectLists xs ys
    | otherwise = intersectLists xs ys


-- 2. find the midpoint of the line segment using tuples
findSegmentMidpoint :: (Double, Double) -> (Double, Double) -> (Double, Double)
findSegmentMidpoint (x1, y1)  (x2, y2) = (x, y)
    where
        x = (x1 + x2) / 2
        y = (y1 + y2) / 2


-- 3. provide grade with following scale using guards
provideGrade :: Int -> [Char]
provideGrade grade
    | grade >= 99 && grade <= 100 = "A+"
    | grade >= 80 && grade <= 98 = "A"
    | grade >= 60 && grade <= 79 = "B"
    | grade >= 50 && grade <= 59 = "C"
    | grade < 50 = "F"
    | otherwise = "Invalid input!"


-- 4. do fibonacci and put it to list
findFibonacci :: Integer -> [Integer]
findFibonacci 0 = [0]
findFibonacci 1 = [1, 0]
findFibonacci n = (head(findFibonacci(n-1)) + tail(findFibonacci(n-2))) : findFibonacci (n-1)

-- 5. remove consecutive duplicates



main :: IO()
main = do
    -- task number 1
    print(intersectLists [1,4,5] [4,5,8])
    print(intersectLists [1,2,3] [4,5,6])


    -- task number 2
    putStr "\nMidpoints of the line segment: "
    print(findSegmentMidpoint (12, 24) (6, 8))

    -- task number 3
    putStr "\nResulting grade is: "
    print(provideGrade 65)
    putStr "Resulting grade is: "
    print(provideGrade 100)
    putStr "Resulting grade is: "
    print(provideGrade 44)
    putStr "Resulting grade is: "
    print(provideGrade 300)

    -- task number 4 


    -- task number 5
