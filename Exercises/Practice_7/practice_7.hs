import Data.Maybe (isNothing)


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

    putStrLn "Hello practice 7!\n"

    -- Task 2
    let scores = [("Kaur", 34), ("Joonas", 63), ("Maria", 34), ("Tarmo", 27)]

    putStrLn "Established winners: "
    print (establishWinner scores "Kaur" "Joonas")
    print (establishWinner scores "Maria" "Tarmo")
    print (establishWinner scores "Kaur" "Maria")
    print (establishWinner scores "Johannes" "Joonas")
    print (establishWinner scores "Kaur" "Rasmus")