-- 1. intersect two lists and print out 
intersectTwoNumLists :: [Int] -> [Int] -> [Int]
-- intersectTwoNumLists list1 list 2 = [ c | c <- list1 list2, c `elem` list1 && c `elem` list2 ]


main = do
   -- task number 1
   let first_list = [1, 2, 3, 4]
   let second_list = [3, 4, 5, 6]
   print(intersectTwoNumLists first_list second_list)
