import Data.Char
import Data.List

-- Task 1
units :: (Integral a, Ord a) => a -> String
units n
    | n > 0 && n < 10 = unitsWords !! fromIntegral (n - 1)
    | otherwise = error "units: not a one digit value"
    where
        unitsWords = words "one two three four five six seven eight nine"

teens :: (Integral a, Ord a) => a -> String
teens n
    | n >= 10 && n < 20 = unitsWords !! fromIntegral (n - 10)
    | otherwise = error "teens: not a two digit value"
    where
        unitsWords = words "ten eleven twelve thirteen fourteen fifteen sixteen seventeen eighteen nineteen"

tens :: (Integral a, Ord a) => a -> String
tens n
    | n >= 20 && n <= 90 = unitsWords !! fromIntegral ((n / 10) - 1) -- e.g 90, index for ninety is 7, do 90 / 10 = 9 - 2 = 7
    | otherwise = error "tens: not a two digit value"
    where
        unitsWords = words "twenty thirty fourty fifty sixty seventy eighty ninety"

thousands :: String
thousands = "thousand"

groupUntilThousands:: (Integral a, Show a) => a -> String
groupUntilThousands n
    | n == 0 = ""
    | n < 10 = units n
    | n < 20 = teens n
    | n < 100 = tens dt ++ ' ' : groupUntilThousands mt
    | n < 1000 = units dh ++ " hundred " ++ groupUntilThousands mh
    | otherwise = error "Not a 3-digit group"
    where 
        (dt, mt) = n `divMod` 10
        (dh, mh) = n `divMod` 100


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
    putStrLn "\nInput a natural number in the range 1-999999: "
    str <- getLine
    if all isDigit str
        then do
            let num = read str :: Int
            if num >= 0 && num <= 999999
                then print (convertNumToWord num)
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