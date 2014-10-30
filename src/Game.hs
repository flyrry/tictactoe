import TicTacToe.Board
import System.Exit
import System.IO (hSetBuffering, BufferMode(NoBuffering), stdin, stdout)

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  putStrLn $ show EmptyBoard
  p <- readMove
  mainLoop $ move EmptyBoard p

mainLoop :: MoveResult -> IO ()
mainLoop board = do
  putStrLn $ show board
  case board of
    IllegalMove -> error "Illegal move, position was taken!"
    NextMove newboard -> do
      pos <- readMove
      mainLoop (move newboard pos)
    GameOver fb -> do
      putStr "GameOver! "
      case whoWon fb of
        Nothing -> putStrLn "It's a draw."
        Just player -> putStrLn $ (show player) ++ " won!"

readMove :: IO Position
readMove = do
  putStr "Please make a move [1-9], 'q' to quit: "
  c <- getChar
  putStrLn ""
  if c == 'q' then exitSuccess
  else if c `elem` ['1'..'9'] then return $ translateToCoords c
       else putStrLn "Invalid option, try again" >> readMove

translateToCoords :: Char -> Position
translateToCoords '1' = NW
translateToCoords '2' = N
translateToCoords '3' = NE
translateToCoords '4' = W
translateToCoords '5' = O
translateToCoords '6' = E
translateToCoords '7' = SW
translateToCoords '8' = S
translateToCoords '9' = SE
translateToCoords _ = undefined

