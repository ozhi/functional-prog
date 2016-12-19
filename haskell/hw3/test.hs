main :: IO()
main = do
	print ((f 1) + 6)
	print ((f 2) + 2.5)
	print ((f 3) :: [Char])
	print ((f 4) || "7")

f :: a -> b
f 1 = 1
f 2 = 4.5
f 3 = "T"
f 4 = False


