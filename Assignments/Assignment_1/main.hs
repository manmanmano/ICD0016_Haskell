-- TASK 1
combineStrings :: [String] -> [String]
combineStrings [] = []
combineStrings (x:xs) = take (length (x:xs)^2) (mapStrings ++ combineStrings ys)
    where ys = xs ++ [x]
          mapStrings = map (x ++) (x:xs)


toLength :: Int -> [String] -> [String]
toLength n xs = [x | x <- combineStrings xs, length x == n]


main :: IO()
main = do
    putStrLn "Hello, Assignment 1!"
    -- task 1
    let strings = ["a", "b", "cd", "def"]
    putStrLn "\nStrings with given length:"
    print (toLength 5 strings)
    print (toLength 2 strings)
    print (toLength 3 strings)
