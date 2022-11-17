import Data.Map (Map)
import qualified Data.Map as M

--1.	Define a new data type Month (with 12 values from January to December). Define two functions 
--spring :: Month -> Bool 
--that is true if the argument is springtime month, and 
--sameStart :: Month -> Month -> Int
--that outputs 1, if the names of the months start with the same letter, and 0 otherwise.

data Month = January | February | March | April | May | June | July | August | September | October | November | December
    deriving (Show, Read, Eq)

spring :: Month -> Bool
spring month
    | month == March = True
    | month == April = True
    | month == May = True
    | otherwise = False

sameStart :: Month -> Month -> Int
sameStart month1 month2
    | head(show month1) == head(show month2) = 1
    | otherwise = 0

--2.	Write a function that adds into a list of records a record for a book with the following information:
--Book name, Author name, ISBN number, Year of publishing and Version Number.

data Book = Book {
  name::String,
  author::String,
  isbn:: Int,
  published:: Int,
  version:: Int
} deriving (Show, Read)

addBook :: [Book] -> Book -> [Book]
addBook books book = books ++ [book]

--3.	Write a function that takes a list of integers as an input and removes from it all numbers that 
-- are divisible by 3 or 5, using filter.

deleteDivisibleBy3and5 :: [Int] -> [Int]
deleteDivisibleBy3and5 = filter(\x -> x `mod` 3 /= 0 && x `mod` 5 /= 0)

--4.	Write a function based on Map Module in Haskell which outputs the following map: 
-- [(1,[4]) ,(2,[8]), (3,[12]), (4,[16])]

myMap :: Integer -> Map Integer [Integer]
myMap n=M.fromList(map makePair[1..n])
    where makePair x = (x, [x*4])

-- I didn't put the main function here
