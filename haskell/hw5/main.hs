import Treap
import TreapIO

main :: IO ()
main = do
	testTreapOfDifferentTypes
	testTreapIO

testTreapOfDifferentTypes :: IO ()
testTreapOfDifferentTypes = do
print charTreap
    print intTreap
    print errorTreap

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

        errorTreap = EmptyTreap `addElement` (3  , 45) `addElement` ('C', 96)

testTreapIO :: IO ()
testTreapIO = do

    printRotated treap

    where
        treap =
            (ioAdd (ioAdd (ioAdd (ioAdd (ioAdd (ioAdd (ioAdd (ioAdd (ioAdd
                (toIoTreap EmptyTreap)
                '1') '2') '3') '4') '5') '6') '7') '8') '9')
