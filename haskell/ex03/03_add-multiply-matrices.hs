-- add, transpose, multiply matrices

type Line = [Int]
type Matrix = [Line]

main :: IO()
main = do
    print (addMat m1 m2)
    print (transpMat m1)
    print (multMat m1 m3)

    where
    m1 :: Matrix
    m1 = [[1,  2,  3,  4],
          [5,  6,  7,  8],
          [9, 10, 11, 12]]
    
    m2 :: Matrix
    m2 = [[1,  0, -1,  0],
          [0,  0,  1,  1],
          [1, -1, -1, -1]]
    
    m3 :: Matrix
    m3 = [[1, 0, 0],
          [0, 1, 0],
          [0, 0, 1],
          [0, 0, 0]]    

addMat :: Matrix -> Matrix -> Matrix -- assume the two matrices are of the same size
addMat = zipWith (zipWith (+))

transpMat :: Matrix -> Matrix
transpMat [l1] = zipWith (:) l1 (repeat [])
transpMat mat = zipWith (:) (head mat) (transpMat (tail mat))

multiplyMat :: Matrix -> Matrix -> Matrix -- assume the two matrices can be multiplied
multiplyMat m1 m2 = m1 --TODO zipWith multiplyLines  (multiplyMat )

