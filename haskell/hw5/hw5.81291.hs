import Treap

getInt :: IO (Int)
getInt = do
    putStrLn "Please input a number."
    myInt <- readLn
    return (myInt :: Int)


main :: IO()
main = do
    myInt <- getInt
    print myInt
    {-
    print $ treap

    print $ treap `contains` 'A' -- True
    print $ treap `contains` 'B' -- True
    print $ treap `contains` 'b' -- False
    print $ treap `contains` 'X' -- False

    print ""
    print ""
    print ""

    print $ rotatedPrint treap
    print $ flatten treap

    print ""
    print ""
    print ""

    print $ root treap
    -- print $ root EmptyTreap
    print $ safeRoot treap
    print $ safeRoot EmptyTreap


    print ""
    print ""
    print ""

    print $ treap `add` '5'

    print $ ((((((((EmptyTreap `add` 'a') `add` 'b') `add` 'c') `add` 'd') `add` 'e') `add` 'f') `add` 'g') `add` 'h')

    --print (getRandom 11)
    --num :: Int
    --num <- randomIO :: IO Int
    --print num
    -}
    where
        treap = create
                    '6'
                    (create
                        '3'
                        (create '2' EmptyTreap EmptyTreap)
                        (create '4' EmptyTreap EmptyTreap))
                    (create
                        '8'
                        (create '7' EmptyTreap EmptyTreap)
                        (create '9' EmptyTreap EmptyTreap))
