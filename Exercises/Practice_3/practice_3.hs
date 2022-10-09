-- define a new datatype month
data Month=Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec
    deriving(Eq)


spring :: Month -> Bool
spring month  
    | month == Mar = True
    | month == Apr = True
    | month == May = True
    | month == Jun = True
    | otherwise = False


main :: IO()
main = do
    putStr "Is January springtime? "
    print(spring Jan)
    putStr "Is May springtime? "
    print(spring May)

