module Treap (
    -- public functions:
    Treap(..),
    empty,
    contains,
    add,
    --delete,
    toList,
        -- [Key] for now, could be [(Key, Value)]
        -- used for traversal

    -- private functions: --should not be exported in the final module
    create,
        -- users should create Treaps by adding key one by one in order to use the heap/priority benefits of the Treap
    root,
    leftTreap,
    rightTreap,

    getRandom,
    addWithPriority,
    newAdd,

    safeRoot,
    safeLeftTreap,
    safeRightTreap,

    flatten,
    rotatedPrint
) where

{-
    Treap data structure

    How can this project be expanded:
        * generalize Key type - (Eq key, Ord key)
        * include a value field alongside the key
-}

{- Define data structure -}
type Key      = Char
type Priority = Int

type Element = (Key, Priority) -- priority is a private implementation detail
data Treap = EmptyTreap | Treap Element Treap Treap -- deriving show?

{- Information -}
empty :: Treap -> Bool
empty EmptyTreap = True
empty _          = False

root :: Treap -> Key
root (Treap (key, _) _ _) = key

leftTreap :: Treap -> Treap
leftTreap (Treap _ leftTreap _) = leftTreap

rightTreap :: Treap -> Treap
rightTreap (Treap _ _ rightTreap) = rightTreap

safeRoot :: Treap -> Maybe Key
safeRoot EmptyTreap           = Nothing
safeRoot (Treap (key, _) _ _) = Just key

safeLeftTreap :: Treap -> Maybe Treap
safeLeftTreap EmptyTreap            = Nothing
safeLeftTreap (Treap _ leftTreap _) = Just leftTreap

safeRightTreap :: Treap -> Maybe Treap
safeRightTreap EmptyTreap             = Nothing
safeRightTreap (Treap _ _ rightTreap) = Just rightTreap

getRandom :: IO (Int) -- private -- simulates random numbers for now
getRandom = do
    putStr "Input a random number: "
    jar <- readLn
    return (myInt :: Int)

{- Manipulation -}
add :: Treap -> Key -> Treap
add EmptyTreap key = create key EmptyTreap EmptyTreap
add treap@(Treap (key, _) leftTreap rightTreap) keyToAdd
    | keyToAdd == key = treap
    | keyToAdd  < key = create key (leftTreap `add` keyToAdd) rightTreap
    | keyToAdd  > key = create key leftTreap (rightTreap `add` keyToAdd)

{-
addWithPriority :: Treap -> Key -> Priority -> Treap
addWithPriority EmptyTreap key priority = create key EmptyTreap EmptyTreap
add treap@(Treap (key, _) leftTreap rightTreap) keyToAdd
    | keyToAdd == key = treap
    | keyToAdd  < key = create key (leftTreap `add` keyToAdd) rightTreap
    | keyToAdd  > key = create key leftTreap (rightTreap `add` keyToAdd)
-}
--newAdd :: Treap -> Key -> IO (Treap)
--newAdd 

contains :: Treap -> Key -> Bool
contains EmptyTreap _ = False
contains (Treap (key, priority) leftTreap rightTreap) keyToFind
    | keyToFind == key = True
    | keyToFind <  key = contains leftTreap  keyToFind
    | keyToFind >  key = contains rightTreap keyToFind

toList :: Treap -> [Key]
toList EmptyTreap = []
toList (Treap (key, _) leftTreap rightTreap) =
    (toList leftTreap) ++ key : (toList rightTreap) 

{-
delete :: Treap -> Key -> Treap
delete EmptyTreap _ = EmptyTreap
delete (Treap (key, _) leftTreap rightTreap) keyToDelete
    | keyToDelete == key = 
    | keyToDelete  < key = create key (leftTreap `delete` key) rightTreap 
    | keyToDelete  > key = create key leftTreap (rightTreap `delete` key)
-}

create :: Key -> Treap -> Treap -> Treap
create key leftTreap rightTreap = (Treap (key, 0) leftTreap rightTreap)

{- Printing -}
flatten :: Treap -> String
flatten EmptyTreap = ""
flatten (Treap (key, _) leftTreap rightTreap) =
    (flatten leftTreap) ++ key : (flatten rightTreap)

rotatedPrint :: Treap -> String
rotatedPrint treap = helper treap 3
    where
    helper :: Treap -> Int -> String
    helper EmptyTreap _ = ""
    helper (Treap (key, _) leftTreap rightTreap) indent =
        (helper  leftTreap (indent + 3)) ++
        (take indent $ repeat ' ')       ++ key  : "\n" ++
        (helper rightTreap (indent + 3))        

instance Show Treap where
    show treap = rotatedPrint treap

--getRandom :: Int -> [Int]
--getRandom seed = head . randomRs (0, 99) . mkStdGen $ seed
