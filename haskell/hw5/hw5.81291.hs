import Treap

main :: IO()
main = do
    print "zdr world, kp"

    print treap
    print $ toList treap

    where
        treap = create
                    '6' 1
                    (create
                        '3' 1
                        (create '2' 1 EmptyTreap EmptyTreap)
                        (create '4' 1 EmptyTreap EmptyTreap))
                    (create
                        '8' 1
                        (create '7' 1 EmptyTreap EmptyTreap)
                        (create '9' 1 EmptyTreap EmptyTreap))
