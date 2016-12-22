type Line = [Int]
type Matrix = [Line]

transpose :: Matrix -> Matrix -- transposing a matrix gives us a list of its columns
transpose [l] = zipWith (:) l (repeat [])
transpose mat = zipWith (:) (head mat) (transpose (tail mat))   

keepsInside :: [[Int]] -> (Int -> Int) -> [[Int]]
keepsInside mat f = filter columnIsKept (transpose mat)
    
    where
    columnIsKept :: Line -> Bool
    columnIsKept line = (filter (\x -> not (elem x line)) newLine) == []
        where newLine = map f line

main :: IO()
main = do

    print (keepsInside mat (\x -> x^2)) -- [[1, -1], [0, 0]]
    print (keepsInside mat (\x -> x))   -- [[1, -1], [0, 0], [5, 2]]
    print (keepsInside mat (\x -> 7))   -- []
    print (keepsInside mat (2*))        -- [[0, 0]]
    print (keepsInside mat (\x -> 2))   -- [[5, 2]]    

    where
    mat :: [[Int]]
    mat = [ [ 1, 0, 5],
            [-1, 0, 2] ]    

