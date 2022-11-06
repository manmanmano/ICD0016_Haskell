import Control.Monad
import Text.Read
import Data.List

-- task 1
checkInteger :: IO Int
checkInteger = do
    userInput <- getLine
    case readMaybe userInput of
        Just int -> return int
        Nothing -> do
            putStrLn "Input must be an integer!"
            checkInteger


userSums :: Int -> IO Int
userSums n = do
    putStrLn "\nInput integers, one per line:"
    numbers <- replicateM n checkInteger
    putStr "The final sum is: "
    return (sum numbers)


-- task 2
readWords :: Int -> IO [String]
readWords n = do
    putStrLn "\nInput words, one per line:"
    words <- replicateM n getLine
    putStr "The sorted list of words is: "
    return (sort words)


-- task 3
numberFour :: String -> FilePath -> IO()
numberFour string filePath = do
    fileContent <- readFile filePath
    putStrLn "File contents before change: "
    putStrLn fileContent
    let lineContents = lines fileContent
    if length lineContents < 3 
        then print "The file is too short"
    else do 
        let newLineContents = take 3 lineContents ++ [string] ++ drop 4 lineContents
        let formatNewLineContents = map (++ "\n") newLineContents
        writeFile filePath (concat formatNewLineContents)
        putStrLn "File contents after change: "
        newFileContent <- readFile filePath
        putStrLn newFileContent


main :: IO()
main = do
    -- generate list of functions which the user can choose
    putStrLn "\nChoose the desired function: "
    putStrLn "0. Exit the program" 
    putStrLn "1. userSums" 
    putStrLn "2. readWords" 
    putStrLn "3. numberFour\n" 
    -- check which function the user chose
    choice <- getLine
    if choice == "0"
        then putStrLn "Exiting the program..."
    else if choice == "1" 
        then do
            userSums 5 >>= print
            main
    else if choice == "2" 
        then do
            readWords 5 >>= print
            main
    else if choice == "3" 
        then do
            numberFour "mamma mia" "my_file.txt" >>= print
            main
    else do 
        putStrLn "Invalid input! Please try again!"
        main
