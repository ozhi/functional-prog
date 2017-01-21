import Data.List

longestIncreasingPrefix :: Ord a => [a] -> [a]
longestIncreasingPrefix [] = []
longestIncreasingPrefix [x] = [x]
longestIncreasingPrefix (x:y:rest) =
    if x < y
    then x : (longestIncreasingPrefix (y:rest))
    else [x]

reorderTuples :: Ord a => [(a,a)] -> [(a,a)]
reorderTuples = map switchIfNeccessary
    where
        switchIfNeccessary (x, y) =
            if x < y
            then (y, x)
            else (x, y)

mergeAndSortDigits :: Int -> Int -> Int
mergeAndSortDigits x y = toNumber (reverse (sortBy comparator (nub $ xDigits ++ yDigits)))   
    where
        comparator :: Int -> Int -> Ordering
        comparator i j =
            if (sum xDigits) <= (sum yDigits)
            then compare i j
            else compare j i

        xDigits = digits x
        yDigits = digits y

        digits :: Int -> [Int]
        digits 0 = []
        digits x = (x `mod` 10) : (digits $ x `div` 10)

        toNumber :: [Int] -> Int
        toNumber [] = 0
        toNumber (x:xs) = (toNumber xs) * 10 + x

{-
    Напишете функция balance :: Int -> [Int] -> Int, която взима число N,
    списък с числа numbers и връща минималния брой на премахванията на елементи от списъка,
    такъв че сумата на числата в списъка да стане <= N или пък списъкът не остане празен.
-}
balance :: Int -> [Int] -> Int
balance maxSum list =
    howManyShouldBeRemoved
        (sortBy (\ x y -> compare y x) list)
        (sum list - maxSum)
    
    where
        howManyShouldBeRemoved :: [Int] -> Int -> Int
        howManyShouldBeRemoved [] _ = 0
        howManyShouldBeRemoved _ sumToBeReduced
            | sumToBeReduced <= 0 = 0
        howManyShouldBeRemoved (x:xs) sumToBeReduced =
            1 + howManyShouldBeRemoved xs (sumToBeReduced - x)

repeater :: String -> (Int -> String -> String)
repeater content = (\ count glue -> repeat count glue)
    where
        repeat :: Int -> String -> String
        repeat 1 _= content
        repeat count glue = content ++ glue ++ (repeat (count - 1) glue) 

data Order =
    Online Float Int Int | -- price, order number, delivery (hours)
    Offline Float -- price

isOnline :: Order -> Bool
isOnline (Online _ _ _) = True
isOnline (Offline _) = False

timeiUntilReceiving :: Order -> Int
timeiUntilReceiving (Online _ _ hours) = hours

totalPrice :: [Order] -> Float
totalPrice [] = 0
totalPrice ((Online price _ _) : rest) = price + (totalPrice rest)
totalPrice ((Offline price) : rest) = price + (totalPrice rest) 

onlineOrders :: [Order] -> Int
onlineOrders [] = 0
onlineOrders ((Online _ _ _) : rest) = 1 + (onlineOrders rest)
onlineOrders ((Offline _) : rest) = (onlineOrders rest) 

isExpensive :: Order -> Bool
isExpensive (Online price _ _) = price > 100
isExpensive (Offline price) = price > 100

instance Show Order where
    show (Online price number delivery) =
        "Order: [type: online, number: " ++ (show number)
        ++ ", price: " ++ (show price) ++ ", delivery: " ++ (show delivery) ++ "]"

    show (Offline price) =
        "Order: [type: offline, price: " ++ (show price) ++ "]"

instance Eq Order where
    (==) (Offline pr1) (Offline pr2) = pr1 == pr2
    (==) (Online pr1 _ _) (Online pr2 _ _) = pr1 == pr2
    (==) _ _ = False

data Tree a = Empty | Node a (Tree a) (Tree a)

levelSum :: Int -> Tree Int -> Int
levelSum _ Empty = 0
levelSum 0 (Node root _ _) = root
levelSum lvl (Node _ left right) = (levelSum (lvl-1) left) + (levelSum (lvl-1) right)

height :: Tree a -> Int
height Empty = 0
height (Node _ left right) = 1 + max (height left) (height right)

cone :: Tree Int -> Bool
cone tree = helper tree (height tree)
    where
        helper :: Tree Int -> Int -> Bool
        helper tree 0 = True
        helper tree level =
            if (levelSum (level - 1) tree) < (levelSum level tree)
            then helper tree (level - 1)
            else False

main :: IO ()
main = do
    print "Hello"

    putStrLn "\n\n1zad"
    print $ longestIncreasingPrefix [1, 2, 2] == [1, 2]
    print $ longestIncreasingPrefix [1, 2, 3] == [1, 2, 3]
    print $ longestIncreasingPrefix [1] == [1]
    print $ longestIncreasingPrefix [1, 2, 3, 4, 2] == [1, 2, 3, 4]


    putStrLn "\n\n2zad"
    print $ reorderTuples [(1, 2), (2, 2), (1, 10), (10, 1)] == [(2, 1), (2, 2), (10, 1), (10, 1)]


    putStrLn "\n\n3zad"
    print $ mergeAndSortDigits  11 111 == 1      -- 1      -- Защото искаме резултатното число да няма повтарящи се цифри.
    print $ mergeAndSortDigits 123 456 == 123456 -- 123456 -- сумата на 1+2+3 е <= от сумата на 4+5+6
    print $ mergeAndSortDigits 456 123 == 654321 -- 654321 -- тук имаме обратния вариант

    putStrLn "\n\n4zad"
    print $ balance 50 [45, 5, 100]   == 1 -- 1 -- Трябва да махнем 100, за да получим сума 50
    print $ balance 3 [2, 10, 15]     == 2 -- 2 -- Тук трябва да махнем 10 и 15
    print $ balance 1 [5, 10, 15, 36] == 4 -- 4 -- Тук трябва да махнем всички елементи от списъка

    putStrLn "\n\n5zad"
    let haskellRepeat = repeater "I love Haskell"
    print $ haskellRepeat 3 " "    -- "I love Haskell I love Haskell I love Haskell"
    print $ repeater "Quack" 5 "!" --"Quack!Quack!Quack!Quack!Quack"

    putStrLn "\n\n7zad"
    print $ 0 == levelSum 0 Empty
    print $ 1 == levelSum 0 (Node 1 (Node 2 Empty Empty) (Node 3 Empty Empty))
    print $ 5 == levelSum 1 (Node 1 (Node 2 Empty Empty) (Node 3 Empty Empty))
    print $ 0 == levelSum 2 (Node 1 (Node 2 Empty Empty) (Node 3 Empty Empty))
    
    print $ True  == cone Empty
    print $ True  == cone (Node 1 (Node 2 Empty Empty) (Node 3 Empty Empty))
    print $ False == cone (Node 5 (Node 2 Empty Empty) (Node 3 Empty Empty))
    print $ False == cone (Node 1 (Node 2 Empty Empty) (Node 3 Empty (Node 4 Empty Empty)))
