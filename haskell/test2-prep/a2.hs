main :: IO ()
main = do
    print "Hello"

    print $ closestToAverage store1 -- "cheese"
    print $ cheaperAlternative store2 -- 1

    where
        store1 =
            [("bread",  1),
             ("milk",   2.5),
             ("lamb",   10),
             ("cheese", 5),
             ("butter", 2.3)]
        store2 = 
            [("bread",  1),
             ("cheese", 2.5),
             ("bread",  1),
             ("cheese", 5),
             ("butter", 2.3)]

type Product = (String, Double)
type StoreAvailability = [Product]

closestToAverage :: StoreAvailability -> String
closestToAverage avail =
    closestTo (getAverage 0 0 avail) 1000000000 "" avail
    
    where
        getAverage :: Int -> Double -> StoreAvailability -> Double
        getAverage curCount curSum [] = curSum / (fromIntegral curCount)
        getAverage curCount curSum ((prodName, prodCost):ps) =
            getAverage (curCount + 1) (curSum +  prodCost) ps

        closestTo :: Double -> Double -> String -> StoreAvailability -> String
        closestTo num minDiff minDiffName [] = minDiffName 
        closestTo num minDiff minDiffName ((prodName, prodCost):ps) =
            if minDiff > (abs (prodCost - num))
            then closestTo num (abs (prodCost - num)) prodName ps
            else closestTo num minDiff minDiffName ps

cheaperAlternative :: StoreAvailability -> Int
cheaperAlternative [] = 0
cheaperAlternative (prod@(prodName, prodCost) : ps) =
    (if cheaperFound prod ps
    then 1
    else 0)
    + cheaperAlternative ps

    where
        cheaperFound :: Product -> StoreAvailability -> Bool
        cheaperFound _ [] = False
        cheaperFound prod@(prodName, prodCost) ((curProdName, curProdCost):ps) =
            if (prodName == curProdName) && (curProdCost /= prodCost)
            then True
            else cheaperFound prod ps
