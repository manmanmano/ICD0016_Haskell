-- define a new datatype month
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
    | head[monthX] == head[monthY] = 1
    | otherwise = 0


isDivisible n = n `mod` 3 == 0 && n `mod` 5 == 0

removeDivisibles :: [Int] -> [Int]
removeDivisibles = filter isDivisible


main :: IO()
main = do
    -- task 1, part 1
    putStr "Is January springtime? "
    print(spring Jan)
    putStr "Is May springtime? "
    print(spring May)

    -- task 2, part 1
    putStr "Do May and March start with the same letter? "
    print(sameStart May Mar)
    putStr "Do February and September start with the same letter? "
    print(sameStart Feb Sep)

    -- task 3 
    putStr "Divisibles by 3 and 5: "
    print (removeDivisibles [5, 10 , 15, 20, 25, 30])
