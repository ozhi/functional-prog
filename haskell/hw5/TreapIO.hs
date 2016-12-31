{-
    TreapIO is build ontop of Treap to provide random priorities
    at the cost of everything being in IO (unpure) functions
-}

module TreapIO (
    toIoTreap,

    ioEmpty,
    ioContains,
    ioAdd,
    ioDelete,

    printAsString,
    printAsList,
    printRotated
) where

import qualified Treap

toIoTreap :: Treap.Treap -> IO (Treap.Treap)
toIoTreap treap = return (treap)



ioEmpty :: IO (Treap.Treap) -> IO (Bool)
ioEmpty ioTreap = do
    treap <- ioTreap
    return (Treap.empty treap)

ioAdd :: IO (Treap.Treap) -> Treap.Key -> IO (Treap.Treap)
ioAdd ioTreap key = do
    priority <- getRandomInt
    treap <- ioTreap
    return (treap `Treap.addElement` (key, priority))

ioContains :: IO (Treap.Treap) -> Treap.Key -> IO (Bool)
ioContains ioTreap key = do
    treap <- ioTreap
    return (Treap.contains treap key)

ioDelete :: IO (Treap.Treap) -> Treap.Key -> IO (Treap.Treap)
ioDelete ioTreap key = do
    treap <- ioTreap
    return (Treap.delete treap key)

printAsString :: IO (Treap.Treap) -> IO ()
printAsString ioTreap = do
    treap <- ioTreap
    putStrLn $ Treap.toString treap

printAsList :: IO (Treap.Treap) -> IO ()
printAsList ioTreap = do
    treap <- ioTreap
    print $ Treap.toList treap

printRotated :: IO (Treap.Treap) -> IO ()
printRotated ioTreap = do
    treap <- ioTreap
    putStrLn $ Treap.rotateToString treap indentStep showEmptyTreap showPriority
    where
        indentStep     = 3
        showEmptyTreap = False
        showPriority   = False



{-
    Utilities
-}

getRandomInt :: IO (Int)
getRandomInt = do
    putStr "Input a random number: "
    jar <- readLn
    return (jar :: Int)
