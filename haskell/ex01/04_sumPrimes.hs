main :: IO()
main = do
    print "Hello"
    print (sumPrimes 0 10)  -- 2+3+5+7 = 17
    print (sumPrimes 8 10)  -- 0
    print (sumPrimes 43 43) -- 43
    print (sumPrimes 11 23) -- 11 + 13 + 17 + 19 + 23 = 83

sumPrimes :: Integer -> Integer -> Integer
sumPrimes a b = sumPrimesHelper a b 0

    where
	sumPrimesHelper :: Integer -> Integer -> Integer -> Integer
	sumPrimesHelper a b curSum
	    |(a > b)   = curSum
	    |isPrime a = sumPrimesHelper (a+1) b (curSum + a)
	    |otherwise = sumPrimesHelper (a+1) b curSum
	    
	isPrime :: Integer -> Bool
	isPrime n = if (n < 2) then False else isPrimeHelper n 2
	    where
		isPrimeHelper :: Integer -> Integer -> Bool
		isPrimeHelper n divisor
		    |((divisor * divisor) > n) = True --((fromIntegral divisor) > ((fromIntegral n) ** 0.5))
		    |((n `mod` divisor) == 0)  = False
		    |otherwise                 = isPrimeHelper n (divisor + 1)
