main = do
    print (map add2           [1,2,3])
    print (map (\ x -> x + 2) [1,2,3])
    print (map (+ 2)          [1,2,3])
    
add2 :: Int -> Int
add2 = (\ x -> x + 2)

-- mymap :: (a -> b) -> [a] -> [b] -- like the prelude map

-- fs :: a -> b -> c -> d
-- fs :: (a -> (b -> (c -> d))) --these two lines are equivalent

composition :: (b -> c) -> (a -> b) -> (a -> c)
composition f g = (\ x -> (f (g x)))
-- composition f g x = f (g x) -- because every func in haskell is a func of one argument
-- composition f g = f . g -- also possible