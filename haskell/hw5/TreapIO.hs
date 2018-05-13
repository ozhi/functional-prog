{-
    Bozhin Katsarski
    fn 81291
    11.Jan.17
-}

{-
    TreapIO is build ontop of Treap to provide random priorities
    at the cost of all functions being IO (not pure)
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
import System.Random




toIoTreap :: Treap.Treap a -> IO (Treap.Treap a)
toIoTreap treap = return (treap)



ioEmpty :: IO (Treap.Treap a) -> IO (Bool)
ioEmpty ioTreap = do
    treap <- ioTreap
    return (Treap.empty treap)

ioContains :: (Eq a, Ord a) => IO (Treap.Treap a) -> Treap.Key a -> IO (Bool)
ioContains ioTreap key = do
    treap <- ioTreap
    return (Treap.contains treap key)

ioAdd :: (Ord a) => IO (Treap.Treap a) -> Treap.Key a -> IO (Treap.Treap a)
ioAdd ioTreap key = do
    treap <- ioTreap
    randomGenerator <- newStdGen
    let (priority, _) = (randomR (1,10000) randomGenerator) :: (Int, StdGen)
    return (treap `Treap.add` (key, priority))

ioDelete :: (Ord a) => IO (Treap.Treap a) -> Treap.Key a -> IO (Treap.Treap a)
ioDelete ioTreap key = do
    treap <- ioTreap
    return (Treap.delete treap key)



printAsString :: (Show a) => IO (Treap.Treap a) -> IO ()
printAsString ioTreap = do
    treap <- ioTreap
    putStrLn $ Treap.toString treap

printAsList :: (Show a) => IO (Treap.Treap a) -> IO ()
printAsList ioTreap = do
    treap <- ioTreap
    print $ Treap.toList treap

printRotated :: (Show a) => IO (Treap.Treap a) -> IO ()
printRotated ioTreap = do
    treap <- ioTreap
    putStrLn $ Treap.rotateToString treap indentStep showEmptyTreap showPriority
    where
        indentStep     = 3
        showEmptyTreap = False
        showPriority   = False
