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

    testTreapOfDifferentTypes

    testPrintRepresentations
    
    testTreapIO
    
    compareSameVsRandomPriorities

testTreapOfDifferentTypes :: IO ()
testTreapOfDifferentTypes = do
    putStrLn $ "\n\n" ++ "testTreapOfDifferentTypes"

    print charTreap
    print intTreap

    where
        charTreap =
            EmptyTreap
                `add` ('C', 45)
                `add` ('D', 67)
                `add` ('A', 21)
                `add` ('G', 49)
                `add` ('F', 93)
                `add` ('H', 55)
                `add` ('B', 12)
                `add` ('I', 84)
                `add` ('E', 33)

        intTreap =
            EmptyTreap
                `add` (3, 45)
                `add` (4, 67)
                `add` (1, 21)
                `add` (7, 49)
                `add` (6, 93)
                `add` (8, 55)
                `add` (2, 12)
                `add` (9, 84)
                `add` (5, 33)

        -- errorTreap = EmptyTreap `addElement` (3  , 45) `addElement` ('C', 96)

testPrintRepresentations :: IO ()
testPrintRepresentations = do
    putStrLn $ "\n\n" ++ "testPrintRepresentations"

    let treap = (toIoTreap EmptyTreap)
                    `ioAdd` 1
                    `ioAdd` 2
                    `ioAdd` 3
                    `ioAdd` 4
                    `ioAdd` 5
                    `ioAdd` 6
                    `ioAdd` 7
                    `ioAdd` 8
                    `ioAdd` 9

    printAsString treap
    printAsList   treap

testTreapIO :: IO ()
testTreapIO = do
    putStrLn $ "\n\n" ++ "testTreapIO"

    printRotated $
        (toIoTreap EmptyTreap)
            `ioAdd` 1
            `ioAdd` 2
            `ioAdd` 3
            `ioAdd` 4
            `ioDelete` 17
            `ioDelete` 2
            `ioDelete` 3
            `ioAdd` 8



compareSameVsRandomPriorities :: IO ()
compareSameVsRandomPriorities = do
    putStrLn $ "\n\n" ++ "compareSameVsRandomPriorities"

    print        $ getTreap   20
    printRotated $ getIOTreap 20

    where        
        getTreap :: Int -> Treap Int
        getTreap 0 = EmptyTreap
        getTreap n = getTreap (n-1) `add` (n, 1)

        getIOTreap :: Int -> IO (Treap Int)
        getIOTreap 0 = toIoTreap EmptyTreap
        getIOTreap n = getIOTreap (n-1) `ioAdd` n
