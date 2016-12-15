main = do
    print ("Hello")
    print (intersect [1,2,2,2,2,2,2,3] [2,2,2,4,3])

intersect :: [Int] -> [Int] -> [Int]
intersect _ [] = []
intersect [] _ = []
intersect (x:lst1) lst2 =
    if ((filter equalsX lst2) == [])
    then (intersect lst1 lst2)
    else x : (intersect lst1 (rem2 x lst2))
    
	where
	    equalsX :: Int -> Bool
	    equalsX a =  if (a == x) then True else False
	
	    rem2 :: Int -> [Int] -> [Int]
	    rem2 _ [] = []
	    rem2 a (x:xs) = if (a == x) then xs else x:(rem2 a xs)
	    