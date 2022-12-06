
type Board = [Int] 
initial :: Board 
initial = [5,4,3,2,1]

-- puts a number of spaces and * to the screen
-- equal for each row in board
putStars :: Int -> IO()
putStars 0 = do return () -- base case, do nothing when num reaches 0
putStars num = do -- do
    putChar ' ' -- print a space
    putChar '*' -- print a *
    putStars (num - 1) -- recurse with num - 1

displayBoard :: Board -> Int -> IO ()
displayBoard [] _ = return ()
displayBoard _ 6 = return ()
displayBoard board i = do
    putStr $ show i
    putChar ':'
    putStars $ head board
    putChar '\n'
    displayBoard (tail board) (i + 1)

checkGameOver :: Board -> Bool
checkGameOver [0,0,0,0,0] = True
checkGameOver _ = False

displayGameOver :: Int -> IO()
displayGameOver 1 = putStrLn "Player 2 wins!\n"
displayGameOver 2 = putStrLn "Player 1 wins!\n"
displayGameOver _ = putStrLn "WIN CONDITION TRIPPED BY INVALID PLAYER NUMBER\n"

promptPlayer :: Int -> IO (Int, Int)
promptPlayer player = do
    putStr "Player "
    print player
    putChar '\n'
    putStr "Enter a row number: "
    rowChoice <- getLine
    putStr "Stars to remove: "
    starChoice <- getLine
    return (read rowChoice, read starChoice)

play :: Board -> Int -> IO()
play board player = do
    putChar '\n'
    displayBoard board 1
    putChar '\n'

    if checkGameOver board
    then do displayGameOver player
    else do
        let
            choices = do promptPlayer player

nim :: IO()
nim = play initial 1
