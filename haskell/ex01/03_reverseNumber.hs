main :: IO()
main = do
    print "Hello"
    print (reverseNumber 5)
    print (reverseNumber 0)
    print (reverseNumber 12)
    print (reverseNumber 1111111)
    print (reverseNumber 9000000)
    print (reverseNumber 123456789)
    

reverseNumber :: Integer -> Integer
reverseNumber n = reverseHelper n 0
    where
	reverseHelper :: Integer -> Integer -> Integer
	reverseHelper 0 result = result
	reverseHelper n result = reverseHelper (n `div` 10) ((10 * result) + (n `mod` 10))