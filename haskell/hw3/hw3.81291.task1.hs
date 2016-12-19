main :: IO()
main = do
	print (sumUnique [[1,2,3,2], [-4,-4], [5]]) -- 9
	print (sumUnique [[1,2,3,4,5,6,5,4,3,3,3], [], [], [1,1,1], [1,2,2]]) -- 10

sumUnique :: [[Integer]] -> Integer
sumUnique [] = 0
sumUnique (x:xs) = (insideSumUnique x) + (sumUnique xs)
	where
	insideSumUnique :: [Integer] -> Integer
	insideSumUnique [] = 0
	insideSumUnique (x:xs) =
		if (elem x xs)
		then (insideSumUnique (filter (/= x) xs))
		else (x + (insideSumUnique xs))
		
