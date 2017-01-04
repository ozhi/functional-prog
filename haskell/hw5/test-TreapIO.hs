import Treap
import TreapIO

printLines :: IO ()
printLines = do
    putStrLn ""
    putStrLn ""
    putStrLn ""
    putStrLn ""

main :: IO()
main = do

    printRotated treap

    where
        treap =
            (ioAdd (ioAdd (ioAdd (ioAdd (ioAdd (ioAdd (ioAdd (ioAdd (ioAdd
                (toIoTreap EmptyTreap)
                '1') '2') '3') '4') '5') '6') '7') '8') '9')
