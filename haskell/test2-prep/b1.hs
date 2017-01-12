main :: IO ()
main = do
    print "Hello"

    print $ maxDuplicate l1 -- 2
    print $ maxDuplicate l2 -- 4
    print $ maxDuplicate l3 -- 0

    where
        l1 = [ [1,2,3,2], [-4,-4], [5]]
        l2 = [ [2,2,2],   [3,3,3], [4,4,4]]
        l3 = [ [1,2,3],   [4,5,6], [7,8,9]]

maxDuplicate :: [[Int]] -> Int
maxDuplicate [] = 0
maxDuplicate (list1:otherLists) =
    maximum $ (maxDuplicate otherLists) : (nonUniques list1)

    where
        nonUniques :: [Int] -> [Int]
        nonUniques [] = []
        nonUniques (x:xs) =
            if x `elem` xs
            then x : (nonUniques xs)
            else nonUniques (xs `remove` x)

        remove :: [Int] -> Int -> [Int]
        remove [] _ = []
        remove (x:xs) y =
                    if x == y
                    then remove xs y
                    else x : (remove xs y)
