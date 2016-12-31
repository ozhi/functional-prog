import Treap

printLines :: IO ()
printLines = do
    putStrLn ""
    putStrLn ""
    putStrLn ""
    putStrLn ""

main :: IO()
main = do

    print treap
    print $ toList treap
    print $ toString treap



    where
        treap =
            EmptyTreap
                `addElement` ('C', 45)
                `addElement` ('D', 67)
                `addElement` ('A', 21)
                `addElement` ('G', 49)
                `addElement` ('F', 93)
                `addElement` ('H', 55)
                `addElement` ('B', 12)
                `addElement` ('I', 84)
                `addElement` ('E', 33)
                