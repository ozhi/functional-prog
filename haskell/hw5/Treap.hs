{-
    Treap data structure

    How can this project be expanded:
        * generalize Key type - (Eq key, Ord key)
        * include a value field alongside the key
-}

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

    safeRoot,
    safeLeftTreap,
    safeRightTreap,

    flatten,
    rotatedPrint
) where


import Data.Char


{- Define the Treap structure -}
type Key      = Char
type Priority = Int

type Element = (Key, Priority) -- priority is a private implementation detail
data Treap = EmptyTreap | Treap Element Treap Treap -- deriving show?


{- Get Treap elements -}
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

create :: Key -> Priority -> Treap -> Treap -> Treap
create key priority leftTreap rightTreap =
    (Treap (key, priority) leftTreap rightTreap)




{- Manipulate a Treap -}
add :: Treap -> Key -> Priority -> Treap
add EmptyTreap key priority =
    create key priority EmptyTreap EmptyTreap

add treap@(Treap (key, priority) leftTreap rightTreap) newKey newPriority
    | newKey == key = treap
    | newKey  < key = fixLeftPriority  $ create newKey newPriority (add leftTreap newKey newPriority)  rightTreap
    | newKey  > key = fixRightPriority $ create newKey newPriority  leftTreap               (add rightTreap newKey newPriority)


fixLeftPriority :: Treap -> Treap --fixLeftPriority EmptyTreap = EmptyTreap -- if it tursn out this case is necessary an "unexhaustive pattern list" error will be produced
fixLeftPriority treap@(Treap (key, priority) leftTreap rightTreap) = 
    if treap `hasPriorityNotSmallerThan` leftTreap
    then treap
    else treap -- createWithPriority k p () rightTreap

fixRightPriority :: Treap -> Treap
fixRightPriority treap@(Treap (key, priority) leftTreap rightTreap) = 
    if treap `hasPriorityNotSmallerThan` rightTreap
    then treap
    else treap -- createWithPriority k p () rightTreap


hasPriorityNotSmallerThan :: Treap -> Treap -> Bool -- matching pattern is not exhaustive, first argument is never supposed to be an EmptyTreap
hasPriorityNotSmallerThan _ EmptyTreap = True
hasPriorityNotSmallerThan (Treap (_, p1) _ _) (Treap (_, p2) _ _) = (p1 >= p2)







contains :: Treap -> Key -> Bool
contains EmptyTreap _ = False
contains (Treap (key, _) leftTreap rightTreap) keyToFind
    | keyToFind == key = True
    | keyToFind <  key = contains leftTreap  keyToFind
    | keyToFind >  key = contains rightTreap keyToFind 



{- Utilities -}
getRandom :: IO (Int) -- private -- simulates random numbers for now
getRandom = do
    putStr "Input a random number: "
    jar <- readLn
    return (jar :: Int)

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

rotatedPrintWithPriority :: Treap -> String -- only works if priorities are digits
rotatedPrintWithPriority treap = helper treap 3
    where
    helper :: Treap -> Int -> String
    helper EmptyTreap _ = ""
    helper (Treap (key, priority) leftTreap rightTreap) indent =
        (helper  leftTreap (indent + 3)) ++
        (take indent $ repeat ' ')       ++ "(" ++ key : ", " ++ (intToDigit priority) : ")\n" ++
        (helper rightTreap (indent + 3))



toList :: Treap -> [Key]
toList EmptyTreap = []
toList (Treap (key, _) leftTreap rightTreap) =
    (toList leftTreap) ++ key : (toList rightTreap)

instance Show Treap where
    show treap = rotatedPrintWithPriority treap
