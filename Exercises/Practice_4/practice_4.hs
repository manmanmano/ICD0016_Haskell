-- task 1
data Triangle=Triangle{base::Double, height::Double}

findTriangleAreas :: [Triangle] -> [Double]
findTriangleAreas = map (\x -> base x * height x / 2)

findSmallestTriangleArea :: [Triangle] -> Double
findSmallestTriangleArea triangles = minimum triangleAreas
    where triangleAreas = findTriangleAreas triangles


-- task 2
findSumOfNumbersLessThan200 :: [Integer] -> Integer
findSumOfNumbersLessThan200 numbers = foldr (+) 0 . filter (200 >) . map (3 *) $ numbers


-- task 3
isLeapYear :: Integer -> Bool
isLeapYear year
    | mod year 400 == 0 = True
    | mod year 100 == 0 = False
    | mod year 4 == 0 = True
    | otherwise = False


findIfSecondYearIsLeap :: [Integer] -> Either [Char] Integer
findIfSecondYearIsLeap years =
    if null leapYears
        then Left "There is no such year"
    else Right (leapYears !! 1)
    where
        leapYears = filter isLeapYear years


-- task 4
isPrime :: Integer -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime n
    | not(null ([x | x <- [2 .. n-1], n `mod` x == 0])) = False
    | otherwise = True


findPrimesLargerThan100 :: [Integer] -> [Integer]
findPrimesLargerThan100 numbers = filter (100 <) . filter isPrime $ numbers


main :: IO()
main = do
    --task 1
    let triangles = [Triangle{base=2,height=4}, Triangle{base=8,height=2},
            Triangle{base=4,height=8}]
    putStr "\nThe smallest triangle area: "
    print(findSmallestTriangleArea triangles)

    -- task 2
    let integers = [53, 329, 34, 19, 110, 26]
    putStr "\nThe sum of tripled numbers less than 200: "
    print(findSumOfNumbersLessThan200 integers)

    -- task 3
    let years = [1900, 2000, 2004, 2100, 2400]
    putStr "\nSecond leap year: "
    print(findIfSecondYearIsLeap years)

    -- task 4
    let integers = [2, 13, 50, 103, 191, 199, 200]
    putStr "\nPrimes in the list greater than 100: "
    print(findPrimesLargerThan100 integers)
