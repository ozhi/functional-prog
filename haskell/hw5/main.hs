{-
    Bozhin Katsarski
    fn 81291
    11.Jan.17
-}

import Treap
import TreapIO

main :: IO ()
main = do
    print "Hello"
    -- testTreapOfDifferentTypes
    -- testTreapIO
    -- compareSameVsRandomPriorities

testTreapOfDifferentTypes :: IO ()
testTreapOfDifferentTypes = do
    putStrLn $ "\n\n" ++ "testTreapOfDifferentTypes"

    print charTreap
    print intTreap

    where
        charTreap =
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

        intTreap =
            EmptyTreap
                `addElement` (3, 45)
                `addElement` (4, 67)
                `addElement` (1, 21)
                `addElement` (7, 49)
                `addElement` (6, 93)
                `addElement` (8, 55)
                `addElement` (2, 12)
                `addElement` (9, 84)
                `addElement` (5, 33)

        -- errorTreap = EmptyTreap `addElement` (3  , 45) `addElement` ('C', 96)



testTreapIO :: IO ()
testTreapIO = do
    printRotated $
        (toIoTreap EmptyTreap)
            `ioAdd` 1
            `ioAdd` 2
            `ioAdd` 3
            `ioAdd` 4
            `ioDelete` 17
            `ioDelete` 2
            `ioDelete` 3



compareSameVsRandomPriorities :: IO ()
compareSameVsRandomPriorities = do
    putStrLn $ "\n\n" ++ "compareSameVsRandomPriorities"

    print        $ getTreap   20
    printRotated $ getIOTreap 20

    where        
        getTreap :: Int -> Treap Int
        getTreap 0 = EmptyTreap
        getTreap n = getTreap (n-1) `addElement` (n, 1)

        getIOTreap :: Int -> IO (Treap Int)
        getIOTreap 0 = toIoTreap EmptyTreap
        getIOTreap n = getIOTreap (n-1) `ioAdd` n
