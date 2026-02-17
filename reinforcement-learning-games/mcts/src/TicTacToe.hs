{-# LANGUAGE MultiParamTypeClasses #-}
module TicTacToe 
    ( TicTacToeState(..)
    , TicTacToeAction(..)
    , initGame ) where

import qualified MCTS (Environment(..))
import Data.Maybe (isNothing, isJust)


data Player = Player1
            | Player2
    deriving (Eq, Show)

data TicTacToeState = TicTacToe [Maybe Player] Player
                    | GameWon Player
                    | GameDraw
    deriving (Eq, Show)

initGame :: TicTacToeState
initGame = TicTacToe (replicate 9 Nothing) Player1

data TicTacToeAction = Action Int 
    deriving (Eq, Show)

otherPlayer :: Player -> Player
otherPlayer Player1 = Player2
otherPlayer Player2 = Player1

(|>) :: Int -> a -> [a] -> [a]
(|>) 0 y (_ : xs) = y : xs
(|>) n y (x : xs) = x : (|>) (n - 1) y xs
(|>) _ _ _        = error "(|>): index out of bounds"

tttwinning :: Player -> [Maybe Player] -> [Int] -> Bool
tttwinning p s is = all (== Just p) $ map (s !!) is

tttrows :: [[Int]]
tttrows = [[0, 1, 2], [3, 4, 5], [6, 7, 8], [0, 3, 6], [1, 4, 7], [2, 5, 8], [0, 4, 8], [2, 4, 6]]

move :: TicTacToeState -> TicTacToeAction -> TicTacToeState
-- move n (p, TicTacToe s) = (1 - p, TicTacToe $ (n |> Just p) s)
move (TicTacToe s p) (Action n) = if winningMove
        then GameWon p
        else if boardFilled
             then GameDraw
             else TicTacToe updatedBoard (otherPlayer p)
    where updatedBoard = ((n |> Just p) s)
          winningMove = any (tttwinning p updatedBoard) tttrows
          boardFilled = all isJust updatedBoard
move gameover _ = gameover

instance MCTS.Environment TicTacToeState TicTacToeAction Float where
    validActions (TicTacToe posms _) = map (Action . snd) $ filter ((isNothing) . fst) $ zip posms [0 .. 8]
    validActions _ = []

    isTerminal (GameWon _)          = True
    isTerminal GameDraw             = True
    isTerminal (TicTacToe _ _)      = False

    act = move

    reward (TicTacToe _ _) GameDraw = 0
    reward (TicTacToe _ cp) (GameWon wp) = fromIntegral $ 2 * isWinner - 1
        where isWinner = fromEnum (cp == wp)
    reward _ _ = 0

{-
import Game

instance Game TicTacToe where

  standard _ = Properties { players = 2, boardsize = 3, human = [True, False] }

  possible _ = PropertyRange { playersrange = [2], boardsizerange = [3] }
  
  moves _ _ (TicTacToe s)
    | any (\p -> any (tttwinning p s) tttrows) [0, 1] = []
    | otherwise                                       = map (move . snd) $ possiblemoves s

  showmove _pr _p (TicTacToe s) i = let j = snd (possiblemoves s !! i)
                                    in ["abc" !! (j `mod` 3), "321" !! (j `div` 3)]

  value _ _ (TicTacToe s) | any (tttwinning 0 s) tttrows = [ 1, -1]
                          | any (tttwinning 1 s) tttrows = [-1,  1]
                          | otherwise                    = [ 0,  0]

  board p _pr vart _ia move' = do

    marble <- bitmapCreateLoad "images\\marble.bmp" wxBITMAP_TYPE_ANY
    varg <- varCreate $ grate rectZero 0 (0, 0) sizeZero

    let 

possiblemoves :: [Maybe Player] -> [(Maybe Player, Int)]
possiblemoves st = filter ((== Nothing) . fst) $ zip st [0 .. 8]
-}
