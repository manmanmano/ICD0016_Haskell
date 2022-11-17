--1.	Write a function in Haskell that finds if an integer number is smaller, greater or equal to 100. 
compareToHundred :: Int ->String
compareToHundred n 
    |n > 100 = show n ++ " is greater than 100."
    |n < 100 = show n ++ " is smaller than 100."
    |otherwise = show n ++ " is equal to 100."

--2.	Write a function in Haskell that calculates the circumference and area of a circle with the given radius.
calculateCircumference :: Double -> Double 
calculateCircumference radius = 2 * pi * radius 
calculateArea :: Double -> Double 
calculateArea radius = pi * radius ** 2 

--3.	Write a function in Haskell that calculates the Euclidian distance between two points (x1,y1) and (x2,y2).
distance::(Double,Double) -> (Double,Double) -> Double
distance (x1,y1) (x2,y2) = sqrt((x2 - x1) ** 2 + (y2 - y1) ** 2)

--4.	Write a function in Haskell that finds if a year is a leap year or not.
leapYear::Int -> Bool
leapYear year = (year `mod` 4 == 0 && year `mod` 100 /= 0) || (year `mod` 400 == 0)

main = do
    print (compareToHundred 120)
    print (calculateCircumference 3)
    print (calculateArea 3)
    print (distance (1,3) (2,5))
    print (leapYear 2022)
