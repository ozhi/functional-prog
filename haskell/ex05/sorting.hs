mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort [x,y] =
    if x < y
    then [x,y]
    else [y,x]

mergeSort list = merge (mergeSort (firstHalf list)) (mergeSort (secondHalf list))

    where
        merge :: (Ord a) => [a] -> [a] -> [a]
        merge l1 [] = l1
        merge [] l2 = l2
        merge (x:xs) (y:ys) =
            if x < y
            then x : (merge    xs  (y:ys) )
            else y : (merge (x:xs)    ys  )

        firstHalf :: [a] -> [a]
        firstHalf list = take ((length list) `div` 2) list

        secondHalf :: [a] -> [a]
        secondHalf list = drop ((length list) `div` 2) list

qSort :: (Ord a) => [a] -> [a]
qSort [] = []
qSort [x] = [x]
qSort [x,y] =
    if x < y
    then [x,y]
    else [y,x]

qSort (x:xs) = (qSort smaller) ++ x : (qSort greater)

    where
        (smaller, greater) = divide xs x ([], []) -- first element used as pivot

        divide :: (Ord a) => [a] -> a -> ([a], [a]) -> ([a], [a])
        divide [] _ result = result
        divide (x:xs) pivot (smaller, greater) =
            if x < pivot
            then divide xs pivot (x:smaller,   greater)
            else divide xs pivot (  smaller, x:greater)

insertionSort :: (Ord a) => [a] -> [a]
insertionSort [] = []
insertionSort [x] = [x]
insertionSort [x,y] =
    if x < y
    then [x,y]
    else [y,x]

insertionSort (x:xs) = (insertionSort xs) `insert` x
    
    where
        insert :: (Ord a) => [a] -> a -> [a]
        insert [] new = [new]
        insert (x:xs) new =
            if new < x
            then new : x : xs
            else x : (xs `insert` new)

selectionSort :: (Eq a, Ord a) => [a] -> [a]
selectionSort [] = []
selectionSort [x] = [x]
selectionSort [x,y] =
    if x < y
    then [x,y]
    else [y,x]

selectionSort list = smallest : (selectionSort (list `removeFirst` smallest))
    where
        smallest = minimum list

        removeFirst :: (Eq a) => [a] -> a -> [a]
        removeFirst [] _ = []
        removeFirst (x:xs) remove =
            if x == remove
            then xs
            else x : (xs `removeFirst` remove)

main :: IO ()
main = do
    print $ mergeSort     [1,0,2,9,3,8,4,7,5,6]
    print $ qSort         [1,0,2,9,3,8,4,7,5,6]
    print $ insertionSort [1,0,2,9,3,8,4,7,5,6]
    print $ selectionSort [1,0,2,9,3,8,4,7,5,6]
