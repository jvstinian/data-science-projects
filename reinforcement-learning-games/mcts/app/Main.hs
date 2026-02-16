module Main where

import Control.Monad.State (runState)
import qualified LineWalk as LW
    ( LineWalkAction(..)
    , initState
    , applyAction )
import MCTS (uctSearch, initTree, uctUpdate)
import System.Random.MWC

main :: IO ()
main = do
  let state = LW.initState 5 3
  print state
  print $ runState (sequence . replicate 3 $ LW.applyAction (LW.MoveRight)) state

  let tree = initTree state
  print tree
  g <- create
  {-
  tree1 <- updateTree g (5 :: Int) tree
  putStrLn ("Updated tree: " ++ show tree1)
  -}
  actionTreeM <- uctSearch 100 g tree
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
