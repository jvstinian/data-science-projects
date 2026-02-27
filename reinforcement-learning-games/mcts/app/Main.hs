module Main where

import Control.Monad.State (runState)
import qualified LineWalk as LW
    ( LineWalkAction(..)
    , initState
    , applyAction )
import MCTS 
    ( uctSearch, initTree, uctUpdate
    , UCTParams(UCTParams, explorationConstant) )
import System.Random.MWC

main :: IO ()
main = do
  let state = LW.initState 5 3
      uctparams = UCTParams { explorationConstant = sqrt (fromIntegral (2 :: Int)) }
  print state
  print $ runState (sequence . replicate 3 $ LW.applyAction (LW.MoveRight)) state

  let tree = initTree state
  print tree
  g <- create
  {-
  tree1 <- updateTree g (5 :: Int) tree
  putStrLn ("Updated tree: " ++ show tree1)
  -}
  actionTreeM <- uctSearch 100 g uctparams tree
  case actionTreeM of
    Just (a, tree1) -> do
        putStrLn ("Action: " ++ show a) 
        putStrLn ("Updated tree: " ++ show tree1) 
    Nothing        ->
        putStrLn "UCT search returned nothing."
  {-
  where updateTree g k tree0 | k > 0     = uctUpdate g tree0 >>= updateTree g (k - 1)
                             | otherwise = return tree0
  -}

{-
import TicTacToe ( TicTacToeState(..), TicTacToeAction(..), initGame )
import MCTS (uctSearch, initTree, uctUpdate, Tree(..), act, UCTParams(UCTParams, explorationConstant))
import System.Random.MWC
import Data.Maybe (fromJust)

takeAction (InteriorNode _ _ _ as) a = find ((==a) . fst) as

let tree = initTree initGame
    uctparams = UCTParams { explorationConstant = sqrt (fromIntegral (2 :: Int)) }
print tree
g <- create
actionTreeM <- uctSearch 100 g uctparams tree
case actionTreeM of
Just (a, tree1) -> do
    putStrLn ("Action: " ++ show a) 
    putStrLn ("Updated tree: " ++ show tree1) 
Nothing        ->
    putStrLn "UCT search returned nothing."

let tree3 = initTree $ act (act initGame (Action 4)) (Action 3)
    uctparams3 = UCTParams { explorationConstant = sqrt (fromIntegral (2 :: Int)) }
    updateCount = 10000
g <- create
actionM3 <- uctSearch updateCount g uctparams3 tree3
putStrLn $ ("Player 1: " ++) . show . fst . fromJust $ actionM3
tree4 = snd . fromJust $ actionM3

actionM4 <- uctSearch updateCount g uctparams3 tree4
putStrLn $ ("Player 2: " ++) . show . fst . fromJust $ actionM4
tree5 = snd . fromJust $ actionM4

actionM5 <- uctSearch updateCount g uctparams3 tree5
putStrLn $ ("Player 1: " ++) . show . fst . fromJust $ actionM5
tree6 = snd . fromJust $ actionM5
print tree6

actionM6 <- uctSearch updateCount g uctparams3 tree6
putStrLn $ ("Player 2: " ++) . show . fst . fromJust $ actionM6
tree7 = snd . fromJust $ actionM6

actionM7 <- uctSearch updateCount g uctparams3 tree7
putStrLn $ ("Player 1: " ++) . show . fst . fromJust $ actionM7
tree8 = snd . fromJust $ actionM7
-}

