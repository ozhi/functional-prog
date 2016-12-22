import Data.Char

-- convert a number from one given base to another

main :: IO()
main = do
    print $ 1111011 == (convert 123 10 2)
    print $ 21 == (convert 10101 2 10)
    print $ 25 == (convert 10101 2 8)

convert :: Int -> Int -> Int -> Int
convert number fromBase toBase =
    (fromDecToBase (fromBaseToDec number fromBase) toBase)

    where
    fromBaseToDec :: Int -> Int -> Int
    fromBaseToDec 0 _ = 0 
    fromBaseToDec number fromBase =
        fromBase * (fromBaseToDec (number `div` 10) fromBase) + (number `mod` 10)

    fromDecToBase :: Int -> Int -> Int
    fromDecToBase 0 _ = 0
    fromDecToBase number toBase =
        (fromDecToBase (number `div` toBase) toBase) * 10 + (number `mod` toBase)

