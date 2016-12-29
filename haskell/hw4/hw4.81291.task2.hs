transpose :: [[a]] -> [[a]]
transpose [l1] = zipWith (:) l1 (repeat [])
transpose mat = zipWith (:) (head mat) (transpose (tail mat))   

matrixInfList :: [[a]] -> [a]
matrixInfList mat = (foldl1 (++) (transpose mat)) ++ (matrixInfList mat)

main :: IO()
main = do
    let matrix = [[1, 2], [3, 4], [5, 6]] in print (matrixInfList matrix)
    -- [1, 3, 5, 2, 4, 6, 1, 3, 5, 2, 4, 6, 1, 3 ...]

