-- 1. find if an integer is smaller, greater, or equal to 100
isNumLessGreaterEqualToHun :: Int -> [Char]
isNumLessGreaterEqualToHun number =
    if number > 100
        then show number ++ " is greater than 100."
    else if number < 100
        then show number ++ " is smaller than 100."
    else show number ++ " is equal to 100."


-- 2. calculate the circumference and area of a circle with given radius
calculateCircumference :: Double -> Double 
calculateCircumference radius = 2 * pi * radius 
calculateArea :: Double -> Double 
calculateArea radius = radius ** 2 * pi


-- 3. calculate euclidian distance between two points (x1, y1) and (x2, y2)
findEuclidianDistance :: Double -> Double -> Double -> Double -> Double 
findEuclidianDistance x2 x1 y2 y1 = sqrt(x2 ** 2 - x1 ** 2 + y2 ** 2 - y1 ** 2)


-- 4. find if year is leap or not
findLeapYear :: Integer -> [Char]
findLeapYear year =
    if mod year 400 == 0
        then show year ++ " is a leap year."
    else if mod year 100 == 0
        then show year ++ " is not a leap year."
    else if mod year 4 == 0
        then show year ++ " is a leap year."
    else show year ++ " is not a leap year."


-- EVERYTHING MUST BE AS FUNCTION BE CALLED IN MAIN
main = do 

    -- task number 1
    putStr "\n"
    putStrLn "Find if integer is greater, smaller, or equal to hundred:"
    print(isNumLessGreaterEqualToHun 5)
    print(isNumLessGreaterEqualToHun 213)
    print(isNumLessGreaterEqualToHun 100)
    putStr "\n"
    -- task number 2
    putStr "\n"
    putStrLn "The circumference and area of a circle with given radius are respectively:"
    print(calculateCircumference 4)
    print(calculateArea 4)
    putStr "\n"
    -- task number 3 
    putStr "\n"
    putStrLn "The Euclidian distance is:"
    print(findEuclidianDistance 4 3 1 2)
    putStr "\n"
    -- task number 4 
    putStr "\n"
    putStrLn "The Euclidian distance is:"
    print(findLeapYear 2023)
    print(findLeapYear 1852)
    print(findLeapYear 2000)
    print(findLeapYear 1233)
    putStr "\n"