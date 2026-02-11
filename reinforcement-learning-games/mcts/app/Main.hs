module Main where

import Control.Monad.State (runState)
import qualified LineWalk as LW
    ( LineWalkAction(..)
    , initState
    , applyAction )
import MCTS (uctSearch, initTree)
import System.Random.MWC

main :: IO ()
main = do
  let state = LW.initState 5 3
  print state
  print $ runState (sequence . replicate 3 $ LW.applyAction (LW.MoveRight)) state

  let tree = initTree state
  print tree
  g <- create
  actionTreeM <- uctSearch 8 g tree
  case actionTreeM of
    Just (a, tree1) -> do
        putStrLn ("Action: " ++ show a) 
        putStrLn ("Updated tree: " ++ show tree1) 
    Nothing        ->
        putStrLn "UCT search returned nothing."
