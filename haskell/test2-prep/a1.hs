main :: IO ()
main = do
    print "Hello"

    print $ sumUnique l1 -- 9
    print $ sumUnique l2 -- 0
    print $ sumUnique l3 -- 45

    where
        l1 = [ [1,2,3,2], [-4,-4], [5]]
        l2 = [ [2,2,2],   [3,3,3], [4,4,4]]
        l3 = [ [1,2,3],   [4,5,6], [7,8,9]]


sumUnique :: [[Int]] -> Int
sumUnique [] = 0
sumUnique (list1 : otherLists) =
    (sumUniques list1) + (sumUnique otherLists)

    where
        sumUniques :: [Int] -> Int
        sumUniques [] = 0
        sumUniques (x:xs) =
            if x `elem` xs
            then sumUniques (xs `remove` x)
            else x + sumUniques xs

            where
                remove :: [Int] -> Int -> [Int]
                remove [] _ = []
                remove (x:xs) y =
                    if x == y
                    then remove xs y
                    else x : (remove xs y)
