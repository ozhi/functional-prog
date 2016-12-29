{-
    Използвано представяне на графа:

        Върховете в графа са представени с тип Int.
        Това лесно би могло да бъде променено на който и да било друг тип, който е
        част от Num, или за който са дефинирани събиране и подредба по големина,
        тъй като за върховете искаме единствено да можем да ги събираме и да
        сравняваме натрупаната сума на обходените върхове.
        
        Информацията за един връх на графа е представена като наредена двойка с първи
        елемент самият връх и втори - списък от върхове, а именно тези, към които той
        има ребро.
        
        Самият граф представлява списък от информацията за всеки един от върховете му,
        описана във формата, който споменахме.
        
        Графът няма други ребра или върхове, освен описаните по този начин. 
-}

type Node         = Int
type NeighborList = [Node]
type NodeInfo     = (Node, NeighborList)
type Graph        = [NodeInfo]

getNeighbors :: Graph -> Node -> NeighborList
getNeighbors graph node =
    snd $ head $ filter (\ (curNode, neighborList) -> curNode == node) graph

isSumReachable :: Graph -> Node -> Node -> Bool
isSumReachable graph startNode targetSum = traverse startNode startNode False -- currentNode currentSum nonTrivialWay

    where
    traverse :: Node -> Node -> Bool -> Bool

    traverse _ currentSum nonTrivialWay
        | currentSum  > targetSum = False
        | currentSum == targetSum = nonTrivialWay -- we only consider non-trivial ways (those of >= 1 edge)

    traverse currentNode currentSum _ = atleastOneOutcomeIsTrue
        where
        currentNodeNeighbors    = (getNeighbors graph currentNode)
        traverseFurther         = (\neighbor -> traverse neighbor (currentSum + neighbor) True)
        allPossibleOutcomes     = (map traverseFurther currentNodeNeighbors)
        atleastOneOutcomeIsTrue = (filter (\x -> x == True) allPossibleOutcomes) /= []

main :: IO ()
main = do

    putStr "getNeighbors:\n"
    print (getNeighbors graph 1)
    print (getNeighbors graph 2)
    print (getNeighbors graph 3)
    print (getNeighbors graph 4)
    print (getNeighbors graph 5)
    print (getNeighbors graph 6)
    
    putStr "\n\n\n"

    putStr "True for each passed test case:\n"
    print $ True  == isSumReachable graph 2  7 -- (2-5)
    print $ True  == isSumReachable graph 1  8 -- (1-2-5)
    print $ True  == isSumReachable graph 1 10 -- (1-4-5)
    print $ False == isSumReachable graph 6  6 -- not possible
    print $ False == isSumReachable graph 6  3 -- not possible

    where
    graph :: Graph
    graph = [ (1, [2, 4, 3]),
              (2, [5]),
              (3, [6]),
              (4, [5]),
              (5, [6]),
              (6, []) ]
