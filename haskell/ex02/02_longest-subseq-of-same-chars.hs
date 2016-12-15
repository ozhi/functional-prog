main = do
    print (longestSeq "abaabbbcdd")
    print (longestSeq "a")
    print (longestSeq "")
    print (longestSeq "fff")
    

longestSeq :: String -> String -- String === [Char]
longestSeq [] = []
longestSeq str =
    if ( (length s1) > (length s2))
    then s1
    else s2
    
    where
	s1 :: String
    	s1 = (longestSeqFromStart str)
	
	s2 :: String
	s2 = (longestSeq (tail str))
    
	longestSeqFromStart :: String -> String
	longestSeqFromStart [] = []
	longestSeqFromStart [f] = [f] 
	longestSeqFromStart (f:s:str) =
	    if (f == s)
	    then f:(longestSeqFromStart (s:str))
	    else [f]
