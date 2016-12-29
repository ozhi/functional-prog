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
    delete,
    toList, -- [Key] for now, could be [(Key, Value)]

    exampleTreap,

    -------
    height
) where


import Data.Char


{- Define the Treap structure -}
type Key      = Char
type Priority = Int

type Element = (Key, Priority) -- priority is a private implementation detail
data Treap = EmptyTreap | Treap Element Treap Treap -- deriving show? -- deiring Eq?

exampleTreap :: Treap
exampleTreap = create
                    '6' 1
                    (create
                        '3' 1
                        (create '2' 1 EmptyTreap EmptyTreap)
                        (create '4' 1 EmptyTreap EmptyTreap))
                    (create
                        '8' 1
                        (create '7' 1 EmptyTreap EmptyTreap)
                        (create '9' 1 EmptyTreap EmptyTreap))

{- Get Treap elements -}
empty :: Treap -> Bool
empty EmptyTreap = True
empty _          = False

{-
    Private create function used for creating a Treap from its elements
    Users can only create a Treap by adding elements to an EmptyTreap
-}
create :: Key -> Priority -> Treap -> Treap -> Treap
create key priority leftTreap rightTreap =
    (Treap (key, priority) leftTreap rightTreap)


height :: Treap -> Int
height EmptyTreap = 0
height (Treap (_,_) leftTreap rightTreap) = 1 + max (height leftTreap) (height leftTreap)


{- Manipulate a Treap -}
add :: Treap -> Key -> IO (Treap)
add treap key = do
    putStr "Input a random number: "
    jar <- readLn
    --return (addWithPriority treap key (jar :: Int))
    return (addWithPriority treap key (jar :: Int))

--add = addWithPriority

addWithPriority :: Treap -> Key -> Priority -> Treap
addWithPriority EmptyTreap key priority = create key priority EmptyTreap EmptyTreap

addWithPriority treap@(Treap (rootKey, rootPriority) leftTreap rightTreap) newKey newPriority
    | newKey == rootKey = treap
    | newKey  < rootKey = fixLeftPriority  $ create rootKey rootPriority (addWithPriority leftTreap newKey newPriority)  rightTreap
    | newKey  > rootKey = fixRightPriority $ create rootKey rootPriority  leftTreap (addWithPriority rightTreap newKey newPriority)

    where
        fixLeftPriority :: Treap -> Treap
        fixLeftPriority EmptyTreap = EmptyTreap -- unnecessary case
        fixLeftPriority treap@(Treap (_,_) EmptyTreap _) = treap
        fixLeftPriority treap@(Treap (rootK, rootP)
                                left@(Treap (leftK, leftP) leftLeft rightLeft)
                                right) =
            if rootP >= leftP
            then treap
            else create leftK leftP leftLeft (create rootK rootP rightLeft right) -- standart tree rotation

        fixRightPriority :: Treap -> Treap
        fixRightPriority EmptyTreap = EmptyTreap -- unnecessary case
        fixRightPriority treap@(Treap (_,_) _ EmptyTreap) = treap
        fixRightPriority treap@(Treap (rootK, rootP)
                                left
                                right@(Treap (rightK, rightP) leftRight rightRight)) =
            if rootP >= rightP
            then treap
            else create rightK rightP (create rootK rootP left leftRight) rightRight -- standart tree rotation


contains :: Treap -> Key -> Bool
contains EmptyTreap _ = False
contains (Treap (rootKey,_) leftTreap rightTreap) keyToFind
    | keyToFind == rootKey = True
    | keyToFind <  rootKey = leftTreap  `contains` keyToFind
    | keyToFind >  rootKey = rightTreap `contains` keyToFind 

delete :: Treap -> Key -> Treap
delete EmptyTreap _ = EmptyTreap

delete (Treap (rootKey, rootPriority) leftTreap rightTreap) keyToDelete
    | keyToDelete <  rootKey = create rootKey rootPriority (leftTreap `delete` keyToDelete) rightTreap
    | keyToDelete >  rootKey = create rootKey rootPriority leftTreap (rightTreap `delete` keyToDelete)

delete (Treap (_,_) EmptyTreap EmptyTreap) _ = EmptyTreap
delete (Treap (_,_) leftTreap  EmptyTreap) _ = leftTreap
delete (Treap (_,_) EmptyTreap rightTreap) _ = rightTreap
delete (Treap (_,_)
         leftTreap@(Treap ( leftK,  leftP) leftLeft  leftRight)
        rightTreap@(Treap (rightK, rightP) rightLeft rightRight)) _ =
    
    if leftP > rightP
    
    then
        let (newRootKey, newRootPriority) = rightMost leftTreap
        in create newRootKey newRootPriority (leftTreap `delete` newRootKey) rightTreap 

    else
        let (newRootKey, newRootPriority) = leftMost rightTreap
        in create newRootKey newRootPriority leftTreap (rightTreap `delete` newRootKey) 

leftMost :: Treap -> Element
leftMost (Treap element@(_,_) EmptyTreap _) = element
leftMost (Treap         (_,_) leftTreap  _) = leftMost leftTreap

rightMost :: Treap -> Element
rightMost (Treap element@(_,_) _ EmptyTreap) = element
rightMost (Treap         (_,_) _ rightTreap) = rightMost rightTreap



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
rotatedPrintWithPriority treap = helper treap 7
    where
    helper :: Treap -> Int -> String
    helper EmptyTreap _ = ""
    helper (Treap (key, priority) leftTreap rightTreap) indent =
        (helper  leftTreap (indent + 7)) ++
        (take indent $ repeat ' ')       ++ "(" ++ (show key) ++ ", " ++ (show priority) ++ ")\n" ++
        (helper rightTreap (indent + 7))

instance Show Treap where
    --show treap = rotatedPrintWithPriority treap
    show treap = rotatedPrint treap


toList :: Treap -> [Key]
toList EmptyTreap = []
toList (Treap (key, _) leftTreap rightTreap) =
    (toList leftTreap) ++ (show key) ++ (toList rightTreap)

root :: Treap -> Key
root (Treap (key, _) _ _) = key

leftTreap :: Treap -> Treap
leftTreap (Treap _ leftTreap _) = leftTreap

rightTreap :: Treap -> Treap
rightTreap (Treap _ _ rightTreap) = rightTreap

{-
    root / leftTreap / rightTreap are not defined for an EmptyTreap
    If they were public functions we could define safely in the following way
-}
safeRoot :: Treap -> Maybe Key
safeRoot EmptyTreap           = Nothing
safeRoot (Treap (key, _) _ _) = Just key

safeLeftTreap :: Treap -> Maybe Treap
safeLeftTreap EmptyTreap            = Nothing
safeLeftTreap (Treap _ leftTreap _) = Just leftTreap

safeRightTreap :: Treap -> Maybe Treap
safeRightTreap EmptyTreap             = Nothing
safeRightTreap (Treap _ _ rightTreap) = Just rightTreap
