import Data.Foldable
import Data.Functor
import Data.Monoid

-- Task 2
incrementAll :: Functor f => Num a => f a -> f a
incrementAll = fmap (+1)


-- Task 3
count :: (Foldable t, Eq a) => a -> t a -> Int
count item = length . filter (== item) . toList


main ::IO()
main = do 
    putStrLn "Hello practice 6!\n"

    -- Task 2
    putStrLn "Function incrementAll results: "
    print (incrementAll [1,2,3])
    print (incrementAll (Just 3.0))
    putStrLn ""

    -- Task 3
    putStrLn "Function count results: "
    print(count True [True,False,True])
    print(count 'c' (Just 'c'))