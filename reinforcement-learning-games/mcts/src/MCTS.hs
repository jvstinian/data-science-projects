{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module MCTS
    ( someFunc
    , Tree(..)
    , Environment(..)
    -- , expand
    , uctSearch
    , initTree ) where

-- import System.Random.MWC
import Data.Ord (comparing)
import Data.List (maximumBy)
import System.Random.Stateful (StatefulGen, uniformRM)


someFunc :: IO ()
someFunc = putStrLn "someFunc"


data InteriorNode s a r = InteriorNode Int r s [(a, Tree s a r)]
    deriving (Show, Eq)

data Tree s a r = Node (InteriorNode s a r) -- Int r s [(a, Tree s a r)]
                | Terminal Int r
    deriving (Show, Eq)

{-
visits :: Tree s a r -> Int
visits (Node (InteriorNode numvisits _ _ _)) = numvisits
visits (Terminal numvisits _ ) = numvisits

totalReward :: (Floating r) => Tree s a r -> r
totalReward (Node (InteriorNode _ totalreward _ _)) = totalreward
totalReward (Terminal numvisits incrementalreward) = (fromIntegral numvisits) * incrementalreward
-}

class Environment s a r | s -> a, s -> r where
    validActions :: s -> [a]
    isTerminal :: s -> Bool
    act :: s -> a -> s
    reward :: s -> r

expand :: (Eq a, Environment s a r, StatefulGen g m) => g -> InteriorNode s a r -> m (Maybe a)
expand g (InteriorNode _ _ state actionnodes) =
        if num_remaining_actions > 0
        then do
            idx <- uniformRM (0, num_remaining_actions - 1) g
            return $ Just $ remaining_actions !! idx
        else
            return Nothing
    where attempted_actions = map fst actionnodes
          all_actions = validActions state
          not_attempted = not . (`elem` attempted_actions)
          remaining_actions = filter not_attempted all_actions
          num_remaining_actions = length remaining_actions

uctObjective :: (Floating r) => Int -> Tree s a r -> r
uctObjective parentvisits (Node (InteriorNode visits totalreward _ _)) =
    let pvs = fromIntegral parentvisits
        cvs = fromIntegral visits
        -- two = (fromIntegral 2) :: r
        -- uctconst = sqrt ((fromIntegral (2 :: Int)) :: r)
        -- uctconst = sqrt two
        uctconst = sqrt (fromIntegral (2 :: Int))
    in 
        (totalreward / cvs) + uctconst * sqrt (2 * (log pvs) / cvs)
uctObjective parentvisits (Terminal visits incrementalreward) =
    let pvs = fromIntegral parentvisits
        cvs = fromIntegral visits
        uctconst = sqrt (fromIntegral (2 :: Int))
    in 
        -- Note that we don't need something like totalreward / visits in the first
        -- term of the following since the reward won't vary in the terminal state
        incrementalreward + uctconst * sqrt (2 * (log pvs) / cvs)

bestChild :: (Eq a, Ord r, Floating r) => Int -> [(a, Tree s a r)] -> (a, Tree s a r)
bestChild numvisits action_nodes =
    let comparefn = comparing (uctObjective numvisits . snd)
    in maximumBy comparefn action_nodes

data PolicyAction s a r = Expand a
                    | BestChild a (Tree s a r)
    deriving (Show, Eq)

bestChildForNode :: (Eq a, Ord r, Floating r) => InteriorNode s a r -> PolicyAction s a r
bestChildForNode (InteriorNode numvisits _ _ action_nodes) = uncurry BestChild $ bestChild numvisits action_nodes

defaultPolicy :: (Environment s a r, StatefulGen g m) => g -> s -> m r
defaultPolicy g s = if isTerminal s
    then return $ reward s
    else do
        let possible_actions = validActions s
            num_actions = length possible_actions 
        idx <- uniformRM (0, num_actions - 1) g
        let next_action = possible_actions !! idx
            next_state = act s next_action
        defaultPolicy g next_state 

-- treePolicy
-- treePolicyNext :: (Environment s a r, StatefulGen g m) => g -> Int -> s -> [(a, Tree s a r)] -> m (PolicyAction s a r)
treePolicyNext :: (Eq a, Ord r, Floating r, Environment s a r, StatefulGen g m) => g -> InteriorNode s a r -> m (PolicyAction s a r)
treePolicyNext g node = do
    actionM <- expand g node
    let next_policy = case actionM of
            Just action -> Expand action
            Nothing     -> bestChildForNode node
    return next_policy

replaceListElement :: a -> (a -> a -> Bool) -> [a] -> [a]
replaceListElement new_val cond (x : xs) = if cond new_val x then (new_val : xs) else (x : replaceListElement new_val cond xs)
replaceListElement _ _ []       = []

backup :: r -> r
backup = id

uctUpdateWithReward :: (Eq a, Ord r, Num r, Floating r, Environment s a r, StatefulGen g m) => g -> InteriorNode s a r -> m (r, Tree s a r)
uctUpdateWithReward g node@(InteriorNode numvisits totalreward state action_nodes) = do
    next_policy <- treePolicyNext g node
    case next_policy of
        Expand a -> do
            let child_state = act state a
            inc_reward <- defaultPolicy g child_state
            let child_node = createNewChildNode child_state inc_reward
                updated_node = Node (InteriorNode (numvisits + 1) (totalreward + inc_reward) state ((a, child_node) : action_nodes))
            return (backup inc_reward, updated_node)
        BestChild a child_tree -> case child_tree of
            Node child_node -> do
                (inc_reward, updated_child_node) <- uctUpdateWithReward g child_node
                let updated_action_nodes = replaceListElement (a, updated_child_node) nodeActionEq action_nodes
                    updated_node = Node (InteriorNode (numvisits + 1) (totalreward + inc_reward) state updated_action_nodes)
                return (backup inc_reward, updated_node)
            Terminal termvisits inc_reward -> do
                let updated_child_node = Terminal (termvisits + 1) inc_reward
                    updated_action_nodes = replaceListElement (a, updated_child_node) nodeActionEq action_nodes
                    updated_node = Node (InteriorNode (numvisits + 1) (totalreward + inc_reward) state updated_action_nodes)
                return (backup inc_reward, updated_node)
    where nodeActionEq x y = (fst x) == (fst y)
          createNewChildNode child_state inc_reward = if isTerminal child_state
                then Terminal 1 inc_reward
                else Node (InteriorNode 1 inc_reward child_state [])

uctUpdate :: (Eq a, Ord r, Floating r, Environment s a r, StatefulGen g m) => g -> Tree s a r -> m (Tree s a r)
uctUpdate g tree = do
    case tree of
        Node node    -> snd <$> uctUpdateWithReward g node
        Terminal _ _ -> return tree

{-
data MCTSState s a r = 
    deriving (Show)
-}

uctSearch :: (Eq a, Ord r, Floating r, Environment s a r, StatefulGen g m) => Int -> g -> Tree s a r -> m (Maybe (a, Tree s a r))
uctSearch n g tree = do
    updated_tree <- updateTree n tree
    case updated_tree of
        Node (InteriorNode numvisits _ _ action_nodes) -> return $ Just (bestChild numvisits action_nodes)
        Terminal _ _                                   -> return $ Nothing
 where updateTree k tree0 | k > 0     = uctUpdate g tree0 >>= updateTree (k - 1)
                          | otherwise = return tree0

initTree :: (Num r, Environment s a r) => s -> Tree s a r
initTree s = if isTerminal s
    then Terminal 0 (reward s)
    else Node (InteriorNode 0 (fromIntegral (0 :: Int)) s [])
