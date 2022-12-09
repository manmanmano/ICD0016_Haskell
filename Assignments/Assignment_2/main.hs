import Data.Char ( isDigit )
import Data.List ( intercalate )
import Data.Maybe ( catMaybes )


-- Task 1 ----------------------------------------------------------------------------------------------------------------------

units :: [String]
units = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

teens :: [String]
teens = ["", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "ninteen"] -- add blank to fix wrong index

tens :: [String]
tens = ["", "ten", "twenty", "thirty", "fourty", "fifty", "sixty", "seventy", "eighty", "ninety"] -- add blank to fix wrong index


groups :: [String]
groups = ["", " thousand"]

groupUntilThousands :: Int -> String
groupUntilThousands n
    | n == 0 = ""
    | n < 10 = units !! n
    | n < 20 = teens !! n
    | n < 100 = tens !! dt ++ ' ' : groupUntilThousands mt
    | n < 1000 = units !! dh ++ " hundred " ++ groupUntilThousands mh
    | otherwise = error "groupUntilThousands - not a 3-digit group!" -- used for debugging purposes
    where
        (dt, mt) = n `divMod` 10
        (dh, mh) = n `divMod` 100

splitFromThousands :: Int -> [Int]
splitFromThousands n
    | d == 0 = [n]
    | otherwise = m : splitFromThousands d
    where
        (d, m) = n `divMod` 1000

convertNumToWord :: Int -> String
convertNumToWord n
    | n == 0 = "zero"
    | otherwise = intercalate " and " (reverse $ zipWith (++) wordGroups groups)
    where
        wordGroups = map groupUntilThousands $ splitFromThousands n

tryNumConversion :: IO ()
tryNumConversion = do
    putStr "\nInput a natural number in the range 1-999999: "
    str <- getLine
    if all isDigit str && str /= ""
        then do
            let num = read str :: Int
            if num >= 0 && num <= 999999
                then print (convertNumToWord num)
            else do
                putStrLn "Input must be a natural number less than 10^6!"
                tryNumConversion
    else do
        putStrLn "Input must contain only digits!" 
        tryNumConversion


-- Task 2 ----------------------------------------------------------------------------------------------------------------------

data TwoList a = TwoEmpty | TwoNode a a (TwoList a) deriving Show

instance Functor TwoList where
    fmap func TwoEmpty = TwoEmpty
    fmap func (TwoNode first second list) = TwoNode (func first) (func second) (fmap func list)

instance Foldable TwoList where
    foldr func initialValue TwoEmpty = initialValue
    foldr func initialValue (TwoNode first second list) = func first (func second (foldr func initialValue list))


-- Task 3 ----------------------------------------------------------------------------------------------------------------------

maze :: [(String,[String])]
maze = [("Entry",["Pit","Corridor 1"])
    , ("Pit",[])
    , ("Corridor 1",["Entry","Dead end"])
    , ("Corridor 2",["Corridor 3"])
    , ("Corridor 3",["Corridor 2"])
    , ("Dead end",["Corridor 1"])]


path :: [(String, [String])] -> String -> String -> Bool
path maze place1 place2 = head $ catMaybes pathExists
    where
        visit = lookup place1 maze
        pathExists = [fmap (elem place2) visit]


-- Main ----------------------------------------------------------------------------------------------------------------------

main :: IO()
main = do
    putStrLn "\nHello, assignment 2!"

    -- Task 1
    tryNumConversion

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
    putStrLn "\nVisit the maze: "
    putStr "From Entry to Pit: "
    print (path maze "Entry" "Pit")
    putStr "From Pit to Entry: "
    print (path maze "Pit" "Entry")
    putStr "From Dead end to Corridor 1: "
    print (path maze "Dead end" "Corridor 1")
    putStr "From Corridor 1 to Corridor 3: "
    print (path maze "Corridor 1" "Corridor 3")


    putStrLn "\nDone!"