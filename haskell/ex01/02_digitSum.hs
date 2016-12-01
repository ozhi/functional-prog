main :: IO()
main = do
    print "Hello"
    print (digitSum 5)
    print (digitSum 0)
    print (digitSum 12)
    print (digitSum 1111111)
    print (digitSum 9000000)
    print (digitSum 123456789)
    

digitSum :: Integer -> Integer
digitSum n = digitSumHelper n 0
    where
	digitSumHelper :: Integer -> Integer -> Integer
	digitSumHelper 0 curSum = curSum
	digitSumHelper n curSum = digitSumHelper (n `div` 10) (curSum + (n `mod` 10))