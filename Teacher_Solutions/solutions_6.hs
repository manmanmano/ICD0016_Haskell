data Polynomial x = Coefficents x | Roots x  deriving (Show,Eq) 
instance Functor Polynomial where 
 fmap f (Coefficents x) = Roots (f x) 

findRoots :: (Float,Float,Float) -> [Float]
findRoots (a, b, c)  
    | a == 0 = []
    | b*b - 4*a*c < 0 = []
    | b*b - 4*a*c == 0 = [-b / (2*a)]
    | otherwise = [(-b + sqrt (b*b - 4*a*c)) / (2*a), (-b - sqrt (b*b - 4*a*c)) / (2*a)]


main = do
 let polyn = Coefficents (5,5,1)
 print $ fmap findRoots polyn
 let polyn = Coefficents (1,1,1)
 print $ fmap findRoots polyn
 let polyn = Coefficents (1,2,1)
 print $ fmap findRoots polyn
 
incrementAll :: (Functor f, Num a) => f a -> f a
incrementAll = fmap (+1)

count x = foldr (\y i -> if x == y then i + 1 else i) 0
