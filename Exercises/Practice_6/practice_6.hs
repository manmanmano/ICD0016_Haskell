import Data.Foldable (toList)


-- Task 1
calculateRoots :: (Double, Double, Double) -> (Maybe Double, Maybe Double)
calculateRoots (a, b, c) = (Just x1, Just x2) 
    where
        x1 = (-b + sqrt y) / (2 * a)
        x2 = (-b - sqrt y) / (2 * a)
        y = (b * b) - (4 * a * c)


-- Task 2
incrementAll :: (Functor f, Num a) => f a -> f a
incrementAll item = (+1) <$> item


-- Task 3
count :: (Foldable t, Eq a) => a -> t a -> Int
count item = length . filter (== item) . toList


main ::IO()
main = do

    putStrLn "Hello practice 6!\n"

    -- Task 1
    putStrLn "Function that calculates quadratic formula roots (NaN when not existing): "
    print (calculateRoots (-4, 7, 2))
    print (calculateRoots (11, 9, 2))

    -- Task 2
    putStrLn "\nFunction incrementAll results: "
    print (incrementAll [1,2,3])
    print (incrementAll (Just 3.0))

    -- Task 3
    putStrLn "\nFunction count results: "
    print(count True [True,False,True])
    print(count 'c' (Just 'c'))

    putStrLn "\nDone!"
