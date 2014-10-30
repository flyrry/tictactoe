module TicTacToe.Board where

import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Data.List (find, intercalate)

data Position = NW | N | NE | W | O | E | SW | S | SE deriving (Ord, Eq, Show, Enum)
data Player = PlayerX | PlayerO deriving Eq
instance Show Player where
  show PlayerX = "X"
  show PlayerO = "O"

type TurnLog = (Player, Position)
type GameLog = [TurnLog]

data EmptyBoard = EmptyBoard
data Board = Board GameLog (M.Map Position Player) deriving Eq
data FinishedBoard = FinishedBoard Board (Maybe Player)
data MoveResult = IllegalMove | NextMove Board | GameOver FinishedBoard

class Movable a where
  (~>) :: a -> Position -> MoveResult

instance Movable EmptyBoard where
  (~>) _ p = NextMove $ Board [(PlayerX,p)] (M.singleton p PlayerX)

instance Movable Board where
  (~>) (Board gamelog m) pos =
    if M.member pos m then IllegalMove
    else let next_player = nextPlayer gamelog
             new_board = Board ((next_player,pos):gamelog) (M.insert pos next_player m)
         in case findWinner new_board of
            Nothing -> if boardFull new_board then GameOver $ FinishedBoard new_board Nothing
                       else NextMove new_board
            winner -> GameOver $ FinishedBoard new_board winner

infix 4 ~>

move :: Movable a => a -> Position -> MoveResult
move a p = a ~> p

whoWon :: FinishedBoard -> Maybe Player
whoWon (FinishedBoard _ w) = w

takeBack :: MoveResult -> Board
takeBack mr = case mr of
              NextMove (Board ((_,pos):gamelog) m) -> Board gamelog $ M.delete pos m
              GameOver (FinishedBoard (Board ((_,pos):gamelog) m) _) -> Board gamelog $ M.delete pos m
              _ -> undefined

playerAt :: Board -> Position -> Maybe Player
playerAt (Board _ m) p = M.lookup p m



nextPlayer :: GameLog -> Player
nextPlayer ((PlayerX,_):_) = PlayerO
nextPlayer ((PlayerO,_):_) = PlayerX
nextPlayer [] = error "impossible to determine next player!"

boardFull :: Board -> Bool
boardFull (Board _ m) = M.size m == 9

scoringLanes :: [[Position]]
scoringLanes = [[N, O, S], [NE, E, SE],
                [NW, N, NE], [W, O, E],
                [SW, S, SE], [NW, W, SW],
                [NW, O, SE], [SW, O, NE]]

findWinner :: Board -> Maybe Player
findWinner (Board _ m) =
  let lanes_of_maybes = map (map (`M.lookup` m)) scoringLanes
      complete_lanes = mapMaybe sequence lanes_of_maybes
  in find (\(h:r) -> all (h==) r) complete_lanes >>= \(f:_) -> return f

instance Show EmptyBoard where
  show _ = " | | \n-----\n | | \n-----\n | | "

instance Show Board where
  show (Board _ m) = intercalate "\n" [showLane m [NW, N, NE], showLane m [W, O, E], showLane m [SW, S, SE]]

showLane :: M.Map Position Player -> [Position] -> String
showLane m lane = intercalate "|" $ map (showSlot m) lane

showSlot :: M.Map Position Player -> Position -> String
showSlot m p = maybe " " show (M.lookup p m)

instance Show FinishedBoard where
  show (FinishedBoard b _) = show b

instance Show MoveResult where
  show IllegalMove = ""
  show (NextMove b) = show b
  show (GameOver b) = show b

