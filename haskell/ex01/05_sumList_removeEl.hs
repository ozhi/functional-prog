import Prelude hiding (rem)

main :: IO()
main = do
    print "Hello"
    print (sumList  [1,2,3,4,5,6,7,8,9])
    print (sumList2 [1,2,3,4,5,6,7,8,9])
    print (rem 2    [1,2,3,4,5,6,7,8,9])

sumList :: [Integer] -> Integer
sumList xs = 
    if (null xs) then 0 else head xs + sumList (tail xs) 

sumList2 :: [Integer] -> Integer
sumList2 [] = 0
sumList2 (x:xs) = x + sumList xs

rem :: Int -> [Int] -> [Int]
rem _ [] = []
rem d (x:xs) =
    if (d == x)
	then xs
	else x:(rem d xs)
