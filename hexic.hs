import Data.Maybe

main = do
    starterBoard <- createBoard
    play starterBoard 0

createBoard = undefined

play :: undefined -> Int -> IO String
play board score = do
    move <- findMove board
    if isNothing move
        then return $ "No more moves available. Final score: " ++ show score
        else do
            nextBoard <- makeMove move board
            (boardAfterShake, moveScore) <- shakeAndScore nextBoard
            play boardAfterShake (score + moveScore)



findMove board = selectMoveFromCandidates . lookForCandidates board

lookForCandidates board = undefined
selectMoveFromCandidates candidates board = undefined

makeMove move board = undefined
shakeAndScore board = undefined
