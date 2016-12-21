main :: IO()
main = do
	print $ 9  == (sumUnique [[1,2,3,2], [-4,-4], [5]])
	print $ 10 == (sumUnique [[1,2,3,4,5,6,5,4,3,3,3], [], [], [1,1,1], [1,2,2]])
	print $ 0  == (sumUnique [])
	print $ 0  == (sumUnique [[], [0], [] ,[]])
	print $ 7  == (sumUnique [[1,1,1,1], [2,2,2,2], [7], [4, 4, 4]])
	print $ 30 == (sumUnique [[1, 4, 5], [1, 5, 4], [4, 1, 5]])

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
		
