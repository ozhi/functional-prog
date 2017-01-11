main :: IO ()
main = do
    print "Hello"

    print $ minDistance pointList -- 1 
    where
        pointList =
            [(0,0,0),
             (1,1,1),
             (0,0,1),
             (0,5,10)]

minDistance :: [(Double, Double, Double)] -> Double
minDistance pointList =
    minimum $ filter (>0) [d p1 p2 | p1 <- pointList, p2 <- pointList]
    where
        d :: (Double, Double, Double) -> (Double, Double, Double) -> Double
        d (a,b,c) (d,e,f) =
            (a - d)^2 + 
            (b - e)^2 + 
            (c - f)^2