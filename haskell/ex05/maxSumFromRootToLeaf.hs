data BT = Empty | Node Int BT BT

maxSumFromRootToLeaf :: BT -> Int
maxSumFromRootToLeaf Empty = 0
maxSumFromRootToLeaf (Node root left right) =
    root + max (maxSumFromRootToLeaf left) (maxSumFromRootToLeaf right)

main :: IO ()
main = do
    print $ maxSumFromRootToLeaf bt

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
