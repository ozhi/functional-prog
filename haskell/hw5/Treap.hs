{-
    Treap data structure

    How can this project be expanded:
        * generalize Key type - (Eq key, Ord key)
        * include a value field alongside the key
-}

module Treap (
    Treap(EmptyTreap),
    Key,
    -- Priority is not exported because it is an implementation detail ot the data structure

    empty,
    contains,
    addElement,
    delete,

    toList,
    toString,
    rotateToString
) where


{-
    Define the Treap structure
-}

type Key      = Char
type Priority = Int
type Element = (Key, Priority)

data Treap = EmptyTreap | Treap Element Treap Treap


{-
    Define the four basic Treap fucntions --publicly exported
-}

empty :: Treap -> Bool
empty EmptyTreap = True
empty _          = False



addElement :: Treap -> Element -> Treap
addElement EmptyTreap (key, priority) = Treap (key, priority) EmptyTreap EmptyTreap

addElement treap@(Treap (rootKey, rootPriority) leftTreap rightTreap) (newKey, newPriority)
    | newKey == rootKey = treap
    | newKey  < rootKey = fixLeftPriority  (Treap (rootKey, rootPriority) (addElement leftTreap (newKey, newPriority))  rightTreap)
    | newKey  > rootKey = fixRightPriority (Treap (rootKey, rootPriority)  leftTreap (addElement rightTreap (newKey, newPriority)))



contains :: Treap -> Key -> Bool
contains EmptyTreap _ = False
contains (Treap (rootKey,_) leftTreap rightTreap) keyToFind
    | keyToFind == rootKey = True
    | keyToFind <  rootKey = leftTreap  `contains` keyToFind
    | keyToFind >  rootKey = rightTreap `contains` keyToFind



delete :: Treap -> Key -> Treap
delete EmptyTreap _ = EmptyTreap

delete (Treap (rootKey, rootPriority) leftTreap rightTreap) keyToDelete
    | keyToDelete <  rootKey = Treap (rootKey, rootPriority) (leftTreap `delete` keyToDelete) rightTreap
    | keyToDelete >  rootKey = Treap (rootKey, rootPriority) leftTreap (rightTreap `delete` keyToDelete)

delete (Treap _ EmptyTreap EmptyTreap) _ = EmptyTreap
delete (Treap _ leftTreap  EmptyTreap) _ = leftTreap
delete (Treap _ EmptyTreap rightTreap) _ = rightTreap
delete (Treap _
         leftTreap@(Treap ( leftK,  leftP) leftLeft  leftRight)
        rightTreap@(Treap (rightK, rightP) rightLeft rightRight)) _ =
    
    if leftP > rightP
    
    then
        let (newRootKey, newRootPriority) = rightMost leftTreap
        in Treap (newRootKey, newRootPriority) (leftTreap `delete` newRootKey) rightTreap 

    else
        let (newRootKey, newRootPriority) = leftMost rightTreap
        in Treap (newRootKey, newRootPriority) leftTreap (rightTreap `delete` newRootKey) 



{-
    Define additional Treap fucntions --publicly exported
-}

toList :: Treap -> [Key]
toList EmptyTreap = []
toList (Treap (key, _) leftTreap rightTreap) =
    (toList leftTreap) ++ key : (toList rightTreap)

toString :: Treap -> String
toString EmptyTreap = ""
toString (Treap (key, _) leftTreap rightTreap) =
    (toString leftTreap) ++ (show key) ++ (toString rightTreap)



rotateToString :: Treap -> Int -> Bool -> Bool -> String -- treap -> indentStep -> showEmptyTreap -> showPriority
rotateToString treap indentStep = helper indentStep treap indentStep -- add totalIdent as first argument    
    where
        helper :: Int -> Treap -> Int -> Bool -> Bool -> String
        helper _ EmptyTreap _ True  _ = " ." ++ "\n"
        helper _ EmptyTreap _ False _ = ""
        helper totalIndent (Treap (key, priority) leftTreap rightTreap) indentStep showEmptyTreap showPriority =
                (helper (totalIndent + indentStep) leftTreap indentStep showEmptyTreap showPriority) ++
                (take totalIndent $ repeat ' ') ++
                
                ( if showPriority
                  then "(" ++ (show key) ++ ", " ++ (show priority) ++ ")" ++ "\n"
                  else        (show key)                                   ++ "\n" )

                ++ (helper (totalIndent + indentStep) rightTreap indentStep showEmptyTreap showPriority)

instance Show Treap where
    show treap =
        let indentStep     = 3
            showEmptyTreap = False
            showPriority   = False
        in rotateToString treap indentStep showEmptyTreap showPriority



{-
    Define Treap helper functions --privately used in the module
-}

leftMost :: Treap -> Element
leftMost (Treap element EmptyTreap _) = element
leftMost (Treap       _ leftTreap  _) = leftMost leftTreap

rightMost :: Treap -> Element
rightMost (Treap element _ EmptyTreap) = element
rightMost (Treap       _ _ rightTreap) = rightMost rightTreap



fixLeftPriority :: Treap -> Treap --standart right tree rotation --used to maintain heap properties
fixLeftPriority treap@(Treap _ EmptyTreap _) = treap
fixLeftPriority treap@(Treap (rootK, rootP)
                        left@(Treap (leftK, leftP) leftLeft rightLeft)
                        right) =
    if rootP >= leftP
    then treap
    else Treap (leftK, leftP) leftLeft (Treap (rootK, rootP) rightLeft right)

fixRightPriority :: Treap -> Treap --standart right tree rotation --used to maintain heap properties
fixRightPriority treap@(Treap (_,_) _ EmptyTreap) = treap
fixRightPriority treap@(Treap (rootK, rootP)
                        left
                        right@(Treap (rightK, rightP) leftRight rightRight)) =
    if rootP >= rightP
    then treap
    else Treap (rightK, rightP) (Treap (rootK, rootP) left leftRight) rightRight



{-
    Define additional Treap functions --not intended for public use
-}

height :: Treap -> Int
height EmptyTreap = 0
height (Treap _ leftTreap rightTreap) = 1 + max (height leftTreap) (height leftTreap)



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