import Data.Map (Map)
import qualified Data.Map as M


-- task 1
data Month=Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec
    deriving(Eq, Show)


spring :: Month -> Bool
spring month
    | month == Mar = True
    | month == Apr = True
    | month == May = True
    | month == Jun = True
    | otherwise = False


sameStart :: Month -> Month -> Int
sameStart monthX monthY
    | head(show monthX) == head(show monthY) = 1
    | otherwise = 0


--task 2
-- book name, author name, isbn number, year of publishing, version number
data Book=Book{bookName::String
    ,authorName::String
    ,isbnNumber::Integer
    ,yearOfPublishing::Integer
    ,versionNumber::Double} deriving (Show)


addBookToList :: Book -> [Book] -> [Book]
addBookToList book bookList = book : bookList


-- task 3
isDivisibleBy5and3 n = n `mod` 3 == 0 && n `mod` 5 == 0

removeDivisibles :: [Int] -> [Int]
removeDivisibles = filter isDivisibleBy5and3


-- task 4
myMap :: Integer -> Map Integer[Integer]
myMap n = M.fromList(map multiplyByFour[1..n])
    where multiplyByFour x = (x, [x * 4])


main :: IO()
main = do
    -- task 1, part 1
    putStr "\nIs January springtime? "
    print(spring Jan)
    putStr "Is May springtime? "
    print(spring May)

    -- task 1, part 2
    putStr "\nDo May and March start with the same letter? "
    print(sameStart May Mar)
    putStr "Do February and September start with the same letter? "
    print(sameStart Feb Sep)

    -- task 2
    let myBook = Book{bookName="It starts with us", authorName="Colleen Hoover", 
        isbnNumber=1668001225, yearOfPublishing=2022, versionNumber=1.1}

    let listOfBooks = [Book{bookName="Confidence Man", authorName="Maggie Haberman", 
        isbnNumber=0593297342, yearOfPublishing=2022, versionNumber=1.1}, 
            Book{bookName="Fairy Tale", authorName="Stephen King", 
            isbnNumber=1668002175, yearOfPublishing=2022, versionNumber=1.1}]

    putStrLn "\nMy new list of books: "
    print(addBookToList myBook listOfBooks)

    -- task 3 
    putStr "\nDivisibles by 3 and 5: "
    print (removeDivisibles [5, 10, 15, 20, 25, 30, 45, 60, 110])

    -- task 4 
    putStrLn "\nMy map: "
    print(myMap 4)
