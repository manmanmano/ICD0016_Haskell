-- task 1
data Triangle=Triangle{base::Double, height::Double}

findTriangleAreas :: [Triangle] -> [Double]
findTriangleAreas = map (\x -> base x * height x / 2)

findSmallestTriangleArea :: [Triangle] -> Double
findSmallestTriangleArea triangles = minimum triangleAreas
    where triangleAreas = findTriangleAreas triangles


-- task 2
findSumOfNumbersLessThan200 :: [Integer] -> Integer
findSumOfNumbersLessThan200 numbers = foldl (+) 0 . filter (200 >) . map (3 *) $ numbers


-- task 3



-- task 4



main :: IO()
main = do
    --task 1
    let listOfTriangles = [Triangle{base=2,height=4}, Triangle{base=8,height=2},
            Triangle{base=4,height=8}]
    putStr "The smallest triangle area: "
    print(findSmallestTriangleArea listOfTriangles)

    -- task 2
    let listOfIntegers = [53, 329, 34, 19, 110, 26]
    putStr "The sum of tripled numbers less than 200: "
    print(findSumOfNumbersLessThan200 listOfIntegers)

    -- task 3
