main :: IO()
main = do
    print "aaaa"
    print (fib 1)
    print (fib 2)
    print (fib 3)
    print (fib 4)
    print (fib 5)
    print (fib 6)
    


fib :: Integer -> Integer
fib n = fibHelper 1 1 n
    where fibHelper :: Integer -> Integer -> Integer -> Integer
	  fibHelper a b 1 = a
	  fibHelper a b 2 = b
	  fibHelper a b n = fibHelper b (a+b) (n-1)
	    