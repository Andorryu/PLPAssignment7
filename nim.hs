{-
    File Name: nim.hs
    Author: Andrew Dority
    Date: 12/07/2022
    Desc: The game nim, programmed in Haskell.
    Run the nim function to play!
-}

-- sets up board type
type Board = [Int]
-- starting board
initial :: Board
initial = [5,4,3,2,1]

-- input: number of stars to print
-- output: nothing
-- prints a number of asterisks to the screen equal to the argument
putStars :: Int -> IO()
putStars 0 = do return () -- base case, do nothing when num reaches 0
putStars num = do
    putChar ' ' -- print a space
    putChar '*' -- print a *
    putStars (num - 1) -- recurse with num - 1 (loop)

-- input: current board and starting row number
-- output: nothing
-- prints the current board
displayBoard :: Board -> Int -> IO ()
displayBoard [] _ = return () -- base case, do nothing when board is empty
displayBoard board i = do
    putStr $ show i -- print the row number
    putChar ':' -- print colon
    putStars $ head board -- print a number of stars equal to the first element in board
    putChar '\n' -- print a new line
    displayBoard (tail board) (i + 1) -- display the tail of the board and increment row number

-- input: current board
-- output: bool
-- returns false if the board array is full of 0's (i.e., the board is cleared)
checkGameOver :: Board -> Bool
checkGameOver [] = True -- base case (if it reaches end of recursion)
checkGameOver (x:xs) = (x == 0) && checkGameOver xs -- creates long string of &&'s, if at any point x is not 0, becomes false

-- input: current player's turn
-- output: nothing
-- tells user that player 1 wins if its player 2's turn, tells user that player 2 wins if its player 1's turn
displayGameOver :: Int -> IO()
displayGameOver player = putStrLn ("Player " ++ show (changePlayer player) ++ " wins!\n")

-- input: int
-- output: action that returns a tuple containing the choices made by the player
-- prompts the player to provide input for their turn in the game
promptPlayer :: Int -> IO (Int, Int)
promptPlayer player = do
    -- print the player whose turn it is
    putStr "Player "
    print player
    -- get row number from player
    putStr "Enter a row number: "
    rowChoice <- getLine
    -- get number of stars to remove from player
    putStr "Stars to remove: "
    starChoice <- getLine
    -- new line
    putChar '\n'
    -- return the choices in tuple
    return (read rowChoice, read starChoice)

-- input: current board and tuple of choices
-- output: bool
-- returns true if the player makes a valid move and false if not a valid move.
-- valid means that the row number is within the numbers displayed and stars to remove is
-- less than or equal to the number of stars on that row
valid :: Board -> (Int, Int) -> Bool
valid board choices =
    (fst choices >= 1 && fst choices <= length board)
    && (board !! (fst choices - 1) >= snd choices)

-- input: currentBoard, a tuple containing the inputs from the player and the index of the element being modified
-- output: a new board that results from the player's choices
-- returns a new board after a player makes changes to it
changeBoard :: Board -> (Int, Int) -> Int -> Board
changeBoard [] _ _ = [] -- base case, return empty array
changeBoard (x:xs) choices index =
    if index == fst choices - 1 -- if we're on the right row
    then (x - snd choices) : xs -- remove stars according to the player's choice and attach the rest of the board to the end
    else x : changeBoard xs choices (index + 1) -- otherwise keep the current row the same and recurse (increment index)

-- input: int
-- output: int
-- changes player 1 to player 2 and vice versa
-- used for switching to next turn
changePlayer :: Int -> Int
changePlayer 1 = 2
changePlayer 2 = 1
changePlayer _ = 0 -- this should never be executed, I just didn't want haskell to yell at me

-- input: board and player int
-- output: nothing
-- main function, recurses for each player's turn
play :: Board -> Int -> IO()
play board player = do
    -- first, display the board
    putChar '\n'
    displayBoard board 1
    putChar '\n'

    if checkGameOver board -- check if game is over
    then do displayGameOver player -- then display game is over
    else do -- else continue with game
        choices <- promptPlayer player -- prompt player for their choices
        if valid board choices -- if choices are valid
        then do
            play (changeBoard board choices 0) (changePlayer player) -- play next turn except with the other player and a changed board
        else do -- if choices are not valid, display invalid choice and restart the turn
            putStrLn "INVALID CHOICE"
            play board player

nim :: IO()
nim = play initial 1 -- start the game loop with the initial board and first player
