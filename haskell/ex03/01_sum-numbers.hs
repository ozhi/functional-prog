import Data.Char

-- Sum all numbers (of 1 or more digits) contained in a string

main :: IO()
main = do
	print $ 30 == (sumNumbers "12jh1kjh17a")
	print $ 12 == (sumNumbers "12")
	print $ 12 == (sumNumbers "12a")
	print $ 15 == (sumNumbers "a1a2a3a4a5a")
	

sumNumbers :: String -> Int
sumNumbers = helper 0
	
	where
	helper :: Int -> String -> Int
	helper curSum [] = curSum
	helper curSum (s:xs) =
		if isDigit s
		then helper (10 * curSum + (digitToInt s)) xs
		else curSum + (sumNumbers xs) 

