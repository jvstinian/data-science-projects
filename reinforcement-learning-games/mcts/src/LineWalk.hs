{-# LANGUAGE MultiParamTypeClasses #-}
module LineWalk
    ( LineWalkState
    , LineWalkAction(..)
    , initState
    , applyAction ) where

import Control.Monad.State (State, state, execState)
import qualified MCTS


type Reward = Int

data LineWalkState = LineWalkState Int Int
    deriving (Show, Eq)

data LineWalkAction = MoveLeft | MoveRight
    deriving (Show, Eq)

initState :: Int -> Int -> LineWalkState
initState n pos | pos < 1   = LineWalkState n 1
                | pos > n   = LineWalkState n n
                | otherwise = LineWalkState n pos

applyAction :: LineWalkAction -> State LineWalkState Reward
applyAction MoveLeft = state (\s -> 
    let LineWalkState n pos = s
        newpos = pos - 1
        reward = if newpos < 1
                 then -1
                 else 0
    in (reward, LineWalkState n newpos))
applyAction MoveRight = state (\s -> 
    let LineWalkState n pos = s
        newpos = pos + 1
        reward = if newpos > n
                 then 1
                 else 0
    in (reward, LineWalkState n newpos))

instance MCTS.Environment LineWalkState LineWalkAction Double where
    validActions _ = [MoveLeft, MoveRight]
    isTerminal (LineWalkState n pos) = (pos < 1) || (pos > n)
    act s = (flip execState s) . applyAction
    -- act s a = execState (applyAction a) s
    reward _ (LineWalkState n pos) =
        if pos < 1
        then (-1.0)
        else if pos > n
             then 1.0
             else 0.0

