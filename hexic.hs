import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Maybe
import Data.List
import System.Random
import Ix

type Board = [[Int]]

-- Entry point
main = do
     gen <- getStdGen
     play (initBoard 8 gen) 0 gen

-- Generate new hexic board of given size using random generator as a source
initBoard :: Int -> StdGen -> Board
initBoard size gen = takeBoard size size (randomRs (0,9) gen)

-- Build up 'count' of lists of 'size' length using 'source' as input
takeBoard :: Int -> Int -> [Int] -> Board
takeBoard _ 0 _ = []
takeBoard size count source = ((take size source):(takeBoard size (count-1) (drop size source)))

-- Pretty print hexic board
printBoard :: Board -> IO ()
printBoard board = let board' = transpose board
                   in putStrLn $ concat $ map (\line -> (oddElemsString line) ++ "\n" ++ (evenElemsString line) ++ "\n") board'

oddElemsString :: (Show a) => [a] -> String
oddElemsString [] = ""
oddElemsString [x] = show x
oddElemsString (x:y:xs) = show x ++ " " ++ (replicate (length $ show y) ' ') ++ " " ++ (oddElemsString xs)

evenElemsString :: (Show a) => [a] -> String
evenElemsString [] = ""
evenElemsString [x] = ""
evenElemsString (x:y:xs) = (replicate (length $ show x) ' ') ++ " " ++ show y ++ " " ++ (evenElemsString xs)

-- Main game loop
play :: Board -> Int -> StdGen -> IO ()
play board score gen = 
    let move = findMove board
    in if isNothing move
            then putStrLn $ "No more moves available. Final score: " ++ show score
            else let nextBoard = makeMove move board
                     (boardAfterShake, moveScore, newGen) = shakeAndScore gen nextBoard
                 in  do  putStrLn $ "move " ++ (show $ fromJust move) ++ "\n"
                         printBoard nextBoard
                         putStrLn $ "shake/score: " ++ (show moveScore) ++ " total: " ++ (show (score + moveScore)) ++ "\n"
                         printBoard boardAfterShake
                         play boardAfterShake (score + moveScore) newGen 

type Move = (Coordinate, Coordinate, Coordinate)
type CandidateMove = (Move, Int)
type Coordinate = (Int, Int)
type Tile = (Coordinate, Int)

-- Read a value from board
getVal :: Board -> Coordinate -> Int
getVal board (x,y) = head $ drop y (head $ drop x board)

-- Update a value on board
setVal :: Board -> Coordinate -> Int -> Board
setVal board (x,y) v = let (beforeCol, (col:afterCol)) = splitAt x board
                           (before, (val:after)) = splitAt y col
                       in beforeCol ++ ((before ++ (v:after)):afterCol)

-- List all neighbours
-- Order is signficant! Clockwise starting at top element
getNeighbours :: Board -> Coordinate -> [Maybe Coordinate]
getNeighbours board (x,y) = [ filterCoordinate (x',y') (length board - 1, length board - 1) 
                                                 | (x', y') <- [(x, y-1)] ++
                                                   (if even x then [(x+1, y-1), (x+1, y)]
                                                              else [(x+1, y),   (x+1, y+1)])
                                                   ++ [(x, y+1)] ++ 
                                                   (if even x then [(x-1, y),   (x-1, y-1)]
                                                              else [(x-1, y+1), (x-1, y)])]
-- Put Nothing at where we have off-board coordinates
filterCoordinate :: Coordinate -> Coordinate -> Maybe Coordinate
filterCoordinate c lim | inRange ((0,0),lim) c = Just c
                       | otherwise = Nothing

-- List all neighbour values
getNeighbourVals :: Board -> Coordinate -> (Tile, [Maybe Tile])
getNeighbourVals b c = ((c, getVal b c), [(\z -> (z, getVal b z)) <$> x | x <- getNeighbours b c])

-- Find adjastent neighbours with same values (cluster)
-- neighbours form a loop, so we need to join head and tail groups carefully
findAdj :: [Maybe Tile] -> [[Tile]]
findAdj l = filter (\xs -> length xs > 1) $ joinHeadAndTail $ map (catMaybes) $ groupBy equalValTiles l

-- Tiles equality by value. Used to collect clusters
equalValTiles :: Maybe Tile -> Maybe Tile -> Bool
equalValTiles (Just x) (Just y) = snd x == snd y
equalValTiles Nothing Nothing = True
equalValTiles _ _ = False

-- join first and last clusters if they are of same value (clusters form a ring!)
joinHeadAndTail :: [[Tile]] -> [[Tile]]
joinHeadAndTail [] = []
joinHeadAndTail [x] = [x]
joinHeadAndTail list 
               | length (head list) > 0 && length (last list) > 0 && length list > 1 = let h@(x:_) = head list
                                                                                           l@(y:_) = last list
                                                                                       in if (snd x == snd y) then (l++h):(tail $ init $ list)
                                                                                          else list
joinHeadAndTail list = list

-- Find companion for cluster
findCompanion :: [Tile] -> [Maybe Tile] -> Maybe Tile
findCompanion cluster@((c1, v1):xs) tiles = let 
                                             t = find (\tile@(c, v) -> (v == v1) && not (tile `elem` cluster)) $ catMaybes tiles
                                            in if isNothing t then if length cluster > 2 then Just (head cluster) else Nothing
                                             else t

-- Try to Make a CandidateMove
-- Cluster -> current location -> Companion for move -> Neighbourhood tiles
buildMoveCandidate :: [Tile] -> Coordinate -> Maybe Tile -> [Maybe Tile] -> Maybe CandidateMove
buildMoveCandidate _ _ Nothing _ = Nothing
buildMoveCandidate cluster c (Just companion) tiles = let
                                                 indx = elemIndex (Just companion) tiles
                                                 maybeClockwise = indx >>= (\i -> (cycle tiles) !! (i + 1))
                                                 maybeAntiClockwise = indx >>= (\i -> (cycle tiles) !! (i + (length tiles) - 1))
                                                 cost = calcCost $ length cluster + 1
                                                in case (maybeClockwise, maybeAntiClockwise) of 
                                                   (Just t, _) | not $ t `elem` cluster -> Just ((c, fst t, fst companion), cost)
                                                   (_, Just t) | not $ t `elem` cluster -> Just ((c, fst t, fst companion), cost)   
                                                   (Just t, _) -> Just ((c, fst t, fst companion), cost)   
                                                   (_, Just t) -> Just ((c, fst t, fst companion), cost)   
                                                   (_,_) -> Nothing

-- Calculate cost of the move
calcCost :: Int -> Int
calcCost size = last $ take (size-2) $ iterate (*3) 3 

-- Get list of candidate moves for all clusters near a position
findMoveCandidates :: Board -> Coordinate -> [CandidateMove]
findMoveCandidates b c = let 
                            neighbours = snd $ getNeighbourVals b c
                         in catMaybes [buildMoveCandidate p c (findCompanion p neighbours) neighbours | p <- findAdj neighbours ]

-- find best move for current board
findMove :: Board -> Maybe Move
findMove board = selectMoveFromCandidates (lookForCandidates board)

-- Collect all available moves from current board with their costs
lookForCandidates :: Board -> [CandidateMove]
lookForCandidates board = concat [ findMoveCandidates board (x,y) | x <- [0..(length board - 1)], y <- [0..(length board - 1)]]

-- Select best move based on cost
selectMoveFromCandidates :: [CandidateMove] -> Maybe Move
selectMoveFromCandidates [] = Nothing
selectMoveFromCandidates candidates = Just $ fst $ foldl1 (\(m1, v1) (m2, v2) -> if (v1 < v2) then (m2, v2) else (m1, v1)) candidates

-- Perform move on the board
makeMove :: Maybe Move -> Board -> Board
makeMove Nothing b = b
makeMove (Just (c1,c2,c3)) b = let v1 = getVal b c1
                                   v2 = getVal b c2
                                   v3 = getVal b c3
                                   b1 = setVal b c1 v3
                                   b2 = setVal b1 c2 v1
                                   b3 = setVal b2 c3 v2
                                 in b3 

-- Shake board and compute score
shakeAndScore :: StdGen -> Board -> (Board, Int, StdGen)
shakeAndScore gen b = shakeAndScore' gen (b,0)

shakeAndScore' :: StdGen -> (Board, Int) -> (Board, Int, StdGen)
shakeAndScore' gen (b,accum) = let drop = findDrop b
                           in case drop of
                                [] -> (b,accum, gen)
                                d -> let (newBoard, val, newGen) = doDrop d b gen
                                      in shakeAndScore' newGen (newBoard, val + accum) 

type Drop = [Coordinate]

-- Collect all clusters on a board
findDrop :: Board -> [Drop]
findDrop b = nub $ concat [checkForDrop b (x,y) | x <- [0..(length b - 1)], y <- [0..(length b - 1)]]

-- Look for clusters at a location
checkForDrop :: Board -> Coordinate -> [Drop]
checkForDrop b c = let 
                       me = fst $ getNeighbourVals b c
                       neighbours = snd $ getNeighbourVals b c
                       adj = findAdj neighbours
                   in map (\xs -> sort $ (fst me):(map fst xs)) 
                    $ filter (\((c,v):xs) -> v == snd me) adj
                                                     
-- Count clusters and perfom drop of values
doDrop :: [Drop] -> Board -> StdGen -> (Board, Int, StdGen)
doDrop d b gen = let
               cleanedDrop = cleanUpDrop d
               totalScore = sum $ map (\xs -> calcCost $ length xs) cleanedDrop
               (newBoard, newGen) = foldl dropCell (b,gen) $ sort $ nub $ concat cleanedDrop
               in (newBoard, totalScore, newGen)

-- Filter all clusters that are parts of other clusters
cleanUpDrop :: [Drop] -> [Drop]
cleanUpDrop d = filter (\xs -> isNothing $ find (\ys -> and (map (\x -> x `elem` ys) xs) && (xs /= ys)) d) d 

-- Drop a single hex from a board
dropCell :: (Board, StdGen) -> Coordinate -> (Board, StdGen)
dropCell (board,gen) (x,y) = let
                              (beforeCol,(col:afterCol)) = splitAt x board
                              (before,(val:after)) = splitAt y col
                              (newVal, newGen) = randomR (0,9) gen 
                              newBoard = beforeCol ++ (((newVal:before) ++ after):afterCol)
                              in (newBoard, newGen)
