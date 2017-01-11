{-
    Bozhin Katsarski
    fn 81291
    11.Jan.17
-}

{-
    Treap data structure

    How can this project be expanded:
        * include a value field alongside the key
        * define TreapIO's show function
-}

module Treap (
    Treap(EmptyTreap), -- only the EmptyTreap constructor is publicly exported
    Key,
    -- Priority is not exported because it is a private detail of the data structure

    empty,
    contains,
    add,
    delete,

    toList,
    toString,
    rotateToString
) where

{- Define the Treap structure -}

type Key a = a
type Priority = Int
type Element a = (Key a, Priority)

data Treap a = EmptyTreap | Treap (Element a) (Treap a) (Treap a)



{- Define the four basic Treap fucntions -} -- Publicly exported

empty :: Treap a -> Bool
empty EmptyTreap = True
empty _          = False



contains :: (Eq a, Ord a) => Treap a -> Key a -> Bool
contains EmptyTreap _ = False
contains (Treap (rootKey,_) leftTreap rightTreap) keyToFind
    | keyToFind == rootKey = True
    | keyToFind <  rootKey = leftTreap  `contains` keyToFind
    | keyToFind >  rootKey = rightTreap `contains` keyToFind



add :: (Eq a, Ord a) => Treap a -> Element a -> Treap a
add EmptyTreap (key, priority) = Treap (key, priority) EmptyTreap EmptyTreap

add treap@(Treap (rootKey, rootPriority) leftTreap rightTreap) (newKey, newPriority)
    | newKey == rootKey = treap
    | newKey  < rootKey = fixLeftPriority  (Treap (rootKey, rootPriority) (leftTreap `add` (newKey, newPriority))  rightTreap)
    | newKey  > rootKey = fixRightPriority (Treap (rootKey, rootPriority)  leftTreap (rightTreap `add` (newKey, newPriority)))



delete :: (Ord a) => Treap a -> Key a -> Treap a
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



{- Define additional Treap fucntions --publicly exported -} -- Publicly exported

toList :: Treap a -> [Key a]
toList EmptyTreap = []
toList (Treap (key, _) leftTreap rightTreap) =
    (toList leftTreap) ++ key : (toList rightTreap)



toString :: (Show a) => Treap a-> String
toString EmptyTreap = ""
toString (Treap (key, _) leftTreap rightTreap) =
    (toString leftTreap) ++ (show key) ++ (toString rightTreap)



rotateToString :: (Show a) => Treap a -> Int -> Bool -> Bool -> String -- treap -> indentStep -> showEmptyTreap -> showPriority
rotateToString treap indentStep = helper indentStep treap indentStep -- use totalIdent as first argument    
    where
        helper :: (Show a) => Int -> Treap a -> Int -> Bool -> Bool -> String
        helper _ EmptyTreap _ True  _ = " ." ++ "\n"
        helper _ EmptyTreap _ False _ = ""
        helper totalIndent (Treap (key, priority) leftTreap rightTreap) indentStep showEmptyTreap showPriority =
                (helper (totalIndent + indentStep) leftTreap indentStep showEmptyTreap showPriority) ++
                (take totalIndent $ repeat ' ') ++
                
                ( if showPriority
                  then "(" ++ (show key) ++ ", " ++ (show priority) ++ ")" ++ "\n"
                  else        (show key)                                   ++ "\n" )

                ++ (helper (totalIndent + indentStep) rightTreap indentStep showEmptyTreap showPriority)

instance (Show a) => Show (Treap a) where
    show treap =
        let indentStep     = 3
            showEmptyTreap = False
            showPriority   = False
        in rotateToString treap indentStep showEmptyTreap showPriority



{- Define Treap helper functions -} -- Privately used in the module

leftMost :: Treap a -> Element a
leftMost (Treap element EmptyTreap _) = element
leftMost (Treap       _ leftTreap  _) = leftMost leftTreap

rightMost :: Treap a -> Element a
rightMost (Treap element _ EmptyTreap) = element
rightMost (Treap       _ _ rightTreap) = rightMost rightTreap


--standart right tree rotation --used to maintain heap properties
fixLeftPriority :: Treap a -> Treap a
fixLeftPriority treap@(Treap _ EmptyTreap _) = treap
fixLeftPriority treap@(Treap (rootK, rootP)
                        left@(Treap (leftK, leftP) leftLeft rightLeft)
                        right) =
    if rootP >= leftP
    then treap
    else Treap (leftK, leftP) leftLeft (Treap (rootK, rootP) rightLeft right)

--standart left tree rotation --used to maintain heap properties
fixRightPriority :: Treap a -> Treap a
fixRightPriority treap@(Treap (_,_) _ EmptyTreap) = treap
fixRightPriority treap@(Treap (rootK, rootP)
                        left
                        right@(Treap (rightK, rightP) leftRight rightRight)) =
    if rootP >= rightP
    then treap
    else Treap (rightK, rightP) (Treap (rootK, rootP) left leftRight) rightRight
