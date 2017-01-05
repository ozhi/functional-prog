import Data.List

main :: IO ()
main = do
    let dl1 = (Many[
                (Many[
                    (One 1),
                    (Many[
                        (One 2),
                        (Many[
                            (One 3),
                            (One 4)])])]), 
                (Many[
                    (One 5),
                    (One 6)]),
                (Many[
                    (One 7),
                    Many[
                        (One 8)]])])

        dl2 = (Many[
                (One 3),
                (Many[
                    (One 7),
                    (Many[
                        (One 10)])])])
    
        dl3 = (Many[
                (One 1),
                (One 1),
                (One 2)])

    -- print $ toList dl1
    -- print $ toList dl2
    -- print $ toSet dl3

    print $ Main.union dl1 dl2
    print $ Main.intersect dl1 dl2

data DeepList = None | One Int | Many [DeepList]

toList :: DeepList -> [Int]
toList None = []
toList (One l) = [l]
toList (Many dl) = foldl1 (++) (map toList dl)

toSet :: DeepList -> [Int]
toSet = nub . toList 

union :: DeepList -> DeepList -> [Int]
union dl1 dl2 = nub $ (toSet dl1) ++ (toSet dl2)

intersect :: DeepList -> DeepList -> [Int]
intersect dl1 dl2 = filter foundInSet2 set1
    where
        set1 = toSet dl1
        set2 = toSet dl2

        foundInSet2 :: Int -> Bool
        foundInSet2 x = elem x set2 
