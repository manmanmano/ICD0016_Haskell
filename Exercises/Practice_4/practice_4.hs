import Data.List


-- task 1
data Triangle=Triangle{base::Double
    ,height::Double}

findTriangleAreas :: [Triangle] -> [Double]
findTriangleAreas = map (\x -> base x * height x / 2)

findSmallestTriangleArea :: [Triangle] -> Double
findSmallestTriangleArea triangles = minimum triangleAreas
    where triangleAreas = findTriangleAreas triangles


main :: IO()
main = do
    --task 1
    let listOfTriangles = [Triangle{base=2,height=4}, Triangle{base=8,height=2},
            Triangle{base=4,height=8}]
    print(findSmallestTriangleArea listOfTriangles)
