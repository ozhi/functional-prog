main :: IO()
main = do

    print (zipWithMultip [1,2,3]   [4,5,6])   -- [4, 10, 18]
    print (zipWithMultip [0,4,7,1] [8,3,4,3]) -- [0, 12, 28, 3]

    print (f [1,2,3]   [4,5,6])   -- 4 + 10 + 18     = 32
    print (f [0,4,7,1] [8,3,4,3]) -- 0 + 12 + 28 + 3 = 43

    print (g [1,2,3]   [4,5,6])   -- 4 + 10 + 18     = 32
    print (g [0,4,7,1] [8,3,4,3]) -- 0 + 12 + 28 + 3 = 43

    print (h [1,2,3]   [4,5,6])   -- 4 + 10 + 18     = 32
    print (h [0,4,7,1] [8,3,4,3]) -- 0 + 12 + 28 + 3 = 43


--ok
zipWithMultip :: [Int] -> [Int] -> [Int]
zipWithMultip = zipWith (*) -- частично прилагане, последните два аргумента са изпуснати

--ok
f :: [Int] -> [Int] -> Int
f l1 l2 = sum ( zipWith (*) l1 l2 )

--ok
g :: [Int] -> [Int] -> Int
g l1 = sum . zipWith (*) l1 -- композиция на две функции, като последният аргумент е изпуснат

--not ok
h :: [Int] -> [Int] -> Int
h = sum . zipWith (*)
-- изпуснати два аргумента като в zipWithMultip
-- композиция като в g
-- грешка при компилация
{-
    curying-test.hs:31:11:
        Couldn't match type ‘[Int]’ with ‘[Int] -> Int’
        Expected type: [Int] -> [Int] -> [Int] -> Int
          Actual type: [Int] -> [Int] -> [Int]
        Possible cause: ‘zipWith’ is applied to too many arguments
        In the second argument of ‘(.)’, namely ‘zipWith (*)’
        In the expression: sum . zipWith (*)
-}
