--1.	A triangle is given with its base and height. Write a function that finds the areas of the 
-- list of triangles and finds the triangle with minimum area, using lambdas and/or higher order functions. 

data Triangle = Triangle {
  base::Double,
  height::Double
} deriving (Show, Read, Eq, Ord)

triangleArea :: Triangle -> Double
triangleArea x = (base x * height x) / 2

findMinArea :: [Triangle] -> Double
findMinArea x = minimum $ map triangleArea x

--2.	Write a function that takes a list of integers as an argument, triples all these numbers, 
-- and finds the sum of tripled numbers that are smaller than 200 using one of the FOLD-functions, $ and composition. 

sumOfTriples :: [Int] -> Int
sumOfTriples numbers = foldr (+) 0 . filter (<200) $ map (*3) numbers

--3.	Write a function that takes a list of the years as an argument and finds the second year in the list which is a leap year 
-- using lambdas and/or higher order functions. On the case there is no such year 
-- the function should return “There is no such year”

isLeapYear :: Int -> Bool
isLeapYear y
  | y `mod` 400 == 0 = True
  | y `mod` 100 == 0 = False
  | y `mod` 4   == 0 = True
  | otherwise        = False

leapYears :: [Int] -> [Int]
leapYears = filter isLeapYear

secondLeapYear :: [Int] -> String
secondLeapYear x = if length(leapYears x) > 1 then show $ leapYears x !! 1
                   else "There is no such year"

--4.	Write a function that finds n first primes larger than 100 using higher order functions, $ and composition.

isPrime :: Int -> Bool
isPrime n = (n > 1) && null [ x | x <- [2..n - 1], n `mod` x == 0]

takePrimes :: Int -> [Int]
takePrimes n = take n . filter isPrime $ [101..]

-- I didn't put main function here
