toLength :: Integer -> [String] -> [String]
toLength x strings = []


main :: IO()
main = do
    putStrLn "Hello, Assignment 1!"
    -- task 1
    let strings = ["a", "b", "cd", "def"]
    print (toLength 5 strings)
    print (toLength 2 strings)