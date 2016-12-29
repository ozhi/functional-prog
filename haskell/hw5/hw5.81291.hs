import Treap

printLines :: IO ()
printLines = do
    putStrLn ""
    putStrLn ""
    putStrLn ""
    putStrLn ""

r :: Int

main :: IO()
main = do

    print treap2

    where
        treap = exampleTreap

{-        treap2 =
            (add
            (add
            (add
            (add
            (add
            (add
            (add
            (add
            (add EmptyTreap '1' 150)
            '2' 180)
            '3' 140)
            '4' 120)
            '5' 140)
            '6' 160)
            '7' 190)
            '8' 110)
            '9' 170) -}

        treap2 =
            (add (add (add (add (add (add (add (add (add
                EmptyTreap
                '1') '2') '3') '4') '5') '6') '7') '8') '9')
