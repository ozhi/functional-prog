main :: IO()
main = do
	print (extractRLE [('m',1),('i',1),('s',2),('i',1),('s',2),('i',1),('p',2),('i',1)])
	-- -> “mississippi”
	
	print (compressRLE "mississipi")
	print (compressRLE "AAAAAAAAA")
	print (compressRLE "aaabbbccc")
	print (compressRLE "abc")
	

extractRLE :: (Eq a) => [(a, Int)] -> [a]
extractRLE [] = []
extractRLE (listItem:restOfList) = (extractListItem listItem) ++ (extractRLE restOfList)
    where
    extractListItem :: (a, Int) -> [a]
    extractListItem (_, 0) = []
    extractListItem (element, times) = element : (extractListItem (element, times-1))
    
compressRLE :: (Eq a) => [a] -> [(a, Int)]
compressRLE [] = []
compressRLE (x:xs) = (x, (countXFromBeginning (x:xs))) : (compressRLE (removeXFromBeginning xs))
    where
    --countXFromBegcd inning :: [a] -> Int -- why do we have to skip this signature?
    countXFromBeginning [] = 0
    countXFromBeginning (f:fs) =
        if (f == x)
        then (1 + (countXFromBeginning fs))
        else 0
    
    --removeXFromBeginning :: [a] -> [a]
    removeXFromBeginning [] = []
    removeXFromBeginning (f:fs) =
        if (f == x)
        then (removeXFromBeginning fs)
        else f:fs

