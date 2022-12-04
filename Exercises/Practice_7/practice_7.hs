import Data.Maybe (isNothing)
import Data.Char (isDigit, isUpper)


-- Task 1
split :: String -> Maybe (String, String)
split s =
    case names of
        [forename, surname] -> Just (forename, surname)
        _ -> Nothing
    where names = words s


checkNumber :: (String, String) -> Maybe (String, String)
checkNumber (forename, surname)
    | containsNumber forename = Nothing
    | containsNumber surname = Nothing
    | otherwise = Just (forename, surname)
    where containsNumber = any isDigit


checkCapital :: (String, String) -> Maybe (String, String)
checkCapital (forename, surname)
    | isUpper (head forename) && isUpper (head surname) = Just (forename, surname)
    | otherwise = Nothing


readNames s =
    split s >>=
        checkNumber >>=
            checkCapital


-- Task 2
establishWinner :: (Ord b, Eq a) => [(a, b)] -> a -> a -> Maybe a
establishWinner scores firstPlayer secondPlayer = do
    let
        firstScore = lookup firstPlayer scores
        secondScore = lookup secondPlayer scores
    if isNothing firstScore || isNothing secondScore
        then Nothing
    else if firstScore > secondScore
        then Just firstPlayer
    else if firstScore < secondScore
        then Just secondPlayer
    else Just firstPlayer


main :: IO()
main = do

    putStrLn "Hello practice 7!"

    -- Task 1
    putStrLn "\nNames that could be read: "
    print (readNames "Lebron James")
    print (readNames "Lionel Messi")
    print (readNames "Lewis Hamilton")
    print (readNames "FrankSinatra") -- no space
    print (readNames "Wall 3ee") -- contains number
    print (readNames "Gigi proietti") -- no capital letter


    -- Task 2
    let scores = [("Kaur", 34), ("Joonas", 63), ("Maria", 34), ("Tarmo", 27)]

    putStrLn "\nEstablished winners: "
    print (establishWinner scores "Kaur" "Joonas")
    print (establishWinner scores "Maria" "Tarmo")
    print (establishWinner scores "Kaur" "Maria")
    print (establishWinner scores "Johannes" "Joonas")
    print (establishWinner scores "Kaur" "Rasmus")

    putStrLn "\nDone!"