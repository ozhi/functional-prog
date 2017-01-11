{-  Да се напише функция minCount :: BT -> Int -> Int,
    която приема като аргументи дефиниция на двоично
    дърво bt и число x. Тя трябва да върне броя на възлите
    (включително корена) на най-малкото поддърво на bt
    със стойност в кореновия възел x. -}

data BT = Empty | Node Int BT BT

minCount :: BT -> Int -> Int
minCount bt x = minimum (map countNodes (filter withRootX (allSubtrees bt)))
    where
        withRootX :: BT -> Bool
        withRootX Empty = False
        withRootX (Node root _ _) = (root == x)

        countNodes :: BT -> Int
        countNodes Empty = 0
        countNodes (Node _ left right) = 1 + (countNodes left) + (countNodes right)

        allSubtrees :: BT -> [BT]
        allSubtrees Empty = []
        allSubtrees bt@(Node _ left right) =
            bt : (allSubtrees left) ++ (allSubtrees right)      

main :: IO ()
main = do
    print $ minCount bt 1 -- 3
    print $ minCount bt 2 -- 1
    print $ minCount bt 3 -- 1
    print $ minCount bt 4 -- 1
    print $ minCount bt 7 -- 2

    where
        bt :: BT
        bt =
            Node 1
                (Node 2
                    (Node 3 Empty Empty)
                    (Node 1
                        (Node 3 Empty Empty)
                        (Node 2 Empty Empty)))
                (Node 1
                    (Node 4 Empty Empty)
                    (Node 7
                        (Node 4 Empty Empty)
                        Empty))