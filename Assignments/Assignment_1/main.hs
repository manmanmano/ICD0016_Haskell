mapStrings :: [String] -> [String]
mapStrings [] = []
mapStrings (x:xs) = map (x ++) (x:xs)

toLength :: [String] -> [String]
toLength [] = []
toLength (x:xs) = take (length (x:xs)^2) (mapStrings (x:xs) ++ toLength ys)
    where ys = xs ++ [x]


main :: IO()
main = do
    putStrLn "Hello, Assignment 1!"
    -- task 1
    let strings = ["a", "b", "cd", "def"]
    let stringo = ["a", "e", "i", "o", "u"]
    print (toLength strings)
    print (toLength stringo)
