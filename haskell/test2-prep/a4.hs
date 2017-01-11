main :: IO ()
main = do
    print "Hello"

    print $ (maximize [(\x -> x*x*x), (\x -> x+1)])  0.5 -- 1.5
    print $ (maximize [(\x -> x*x*x), (\x -> x+1)]) (-2) -- -8

maximize :: (Ord a, Num a) => [(a -> a)] -> (a -> a) -- by absolute value
maximize [f] = f
maximize (f:fs) = (\ arg ->
                        if abs (f arg) > abs (maximizeFs arg)
                        then f arg
                        else maximizeFs arg)
    where
        maximizeFs = maximize fs
