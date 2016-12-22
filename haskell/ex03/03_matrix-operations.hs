-- add, transpose, multiply matrices
-- functions seem to work

type Line = [Int]
type Matrix = [Line]

main :: IO()
main = do
    print (addMat m1 m2)
    print (transpMat m1)
    print (multiplyMat m1 m3) -- [[10, 10], [26, 26]]
    
    print (f [1,2,3] [1,2,3])

    -- test the scope of where

    where
    m1 :: Matrix
    m1 = [[1, 2, 3, 4],
          [5, 6, 7, 8]]
    
    m2 :: Matrix
    m2 = [[8, 7, 6, 5],
          [4, 3, 2, 1]]
    
    m3 :: Matrix
    m3 = [[1, 1],
          [1, 1],
          [1, 1],
          [1, 1]]    

addMat :: Matrix -> Matrix -> Matrix -- assume the two matrices are of the same size
addMat = zipWith (zipWith (+))

transpMat :: Matrix -> Matrix
transpMat [l1] = zipWith (:) l1 (repeat [])
transpMat mat = zipWith (:) (head mat) (transpMat (tail mat))

multiplyMat :: Matrix -> Matrix -> Matrix -- assume the two matrices can be multiplied
multiPlyMat [] [] = []
multiplyMat m1 m2 = productFirstLine : ( zipWith (:) productFirstColumn productOfSubmatrices)
    
    where
    productFirstLine :: Line
    productFirstLine = (zipWith multiplyTwoLines (repeat (head m1)) (transpMat m2))

    productFirstColumn :: Line -- wihtout the first element
    productFirstColumn = (zipWith multiplyTwoLines (tail m1) (repeat (map head m2)))
    
    productOfSubmatrices :: Matrix
    productOfSubmatrices = (multiplyMat (tail m1) (map tail m2))

    multiplyTwoLines :: Line -> Line -> Int
    --multiplyTwoLines l1 = sum . zipWith (*) l1 -- how can we skip the first argument too?
    multiplyTwoLines l1 l2 = sum . zipWith (*) l1 l2

