
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
    putChar '\n'
    return (read rowChoice, read starChoice)

valid :: Board -> (Int, Int) -> Bool
valid board choices = (board !! (fst choices - 1) >= snd choices) -- need to check if row number is invalid

-- remove num of spaces from num row of board
changeBoard :: Board -> (Int, Int) -> Int -> Board
changeBoard (x:xs) choices index =
    if index == fst choices - 1
    then (x - snd choices) : xs
    else x : changeBoard xs choices (index + 1)

changePlayer :: Int -> Int
changePlayer 1 = 2
changePlayer 2 = 1
changePlayer _ = 0

play :: Board -> Int -> IO()
play board player = do
    putChar '\n'
    displayBoard board 1
    putChar '\n'

    if checkGameOver board
    then do displayGameOver player
    else do
        choices <- promptPlayer player
        if valid board choices
        then do 
            putStrLn "VALID CHOICE"
            play (changeBoard board choices 0) (changePlayer player)
        else do 
            putStrLn "INVALID CHOICE"
            play board player

nim :: IO()
nim = play initial 1
