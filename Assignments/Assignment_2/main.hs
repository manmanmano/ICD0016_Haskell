import Data.Char (isDigit)


-- Task 1
-- readNames s =
--     split s >>=
--         checkNumber >>=
--             checkCapital

units :: Int -> String
units  n
    | n >= 0 && n < 10 = unitsWords !! n
    | otherwise = ""
    where 
        unitsWords = words "zero one two three four five six seven eight nine"


teens :: Int -> String
teens n 
    | n >= 10 && n < 20 = teensWords !! (n - 10)
    | otherwise = ""
    where 
        teensWords = words "ten eleven twelve thirteen fourteen fifteen sixteen seventeen eighteen nineteen"


tens :: Int -> String
tens n 
    | n >= 20 && n <= 90 = tensWords !! n
    | otherwise = "" 
    where 
        tensWords = words "twenty thirty fourty fifty sixty seventy eighty ninety"


hundreds :: Int -> String
hundreds n 
    | n >= 100 = "hundred"
    | otherwise = ""


thousands :: Int -> String
thousands n 
    | n >= 1000 = "thousand"
    | otherwise = ""


convertNumberToWord :: Int -> String
convertNumberToWord n
    | n < 10 = units n
    | n < 20 = teens n
    | otherwise = "" 


-- Task 2

data TwoList a = TwoEmpty | TwoNode a a (TwoList a) deriving Show


instance Functor TwoList where
    fmap func TwoEmpty = TwoEmpty
    fmap func (TwoNode first second list) = TwoNode (func first) (func second) (fmap func list)


instance Foldable TwoList where
    foldr func initialValue TwoEmpty = initialValue
    foldr func initialValue (TwoNode first second list) = func first (func second (foldr func initialValue list))


main :: IO()
main = do

    -- Task 1
    putStrLn "\nInput a natural number in the range 0-999999: "
    str <- getLine
    if all isDigit str 
        then do 
            let num = read str :: Int
            if num >= 0 && num <= 999999
                then print (convertNumberToWord num)
            else do        
            putStrLn "Input must be a natural number less than 1000000!"
    else do        
        putStrLn "Input must be a natural number less than 1000000!"


    -- Task 2, Part 1
    putStrLn "\nFunctor TwoList: "
    putStr "fmap +2: "
    print(fmap (+2) (TwoNode 0 1 (TwoNode 2 3 TwoEmpty)))
    putStr "fmap +3: "
    print(fmap (+3) (TwoNode 0 1 (TwoNode 2 3 TwoEmpty)))
    putStr "fmap +4: "
    print(fmap (+4) (TwoNode 0 1 (TwoNode 2 3 TwoEmpty)))
    -- Task 2, Part 2
    putStrLn "\nFoldable TwoList: "
    putStr "sum: "
    print(sum (TwoNode 1 2 (TwoNode 3 4 TwoEmpty)))
    putStr "product: "
    print(product (TwoNode 1 2 (TwoNode 3 4 TwoEmpty)))
    putStr "length: "
    print(length (TwoNode 1 2 (TwoNode 3 4 TwoEmpty)))

    -- Task 3


    putStrLn "\nDone!"