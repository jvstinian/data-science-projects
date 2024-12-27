import Data.Word
import Data.List
import Numeric.LinearAlgebra
import Data.Ord
import qualified Data.Map.Lazy as Map
import Control.Monad (forM_)


data Maze = Maze {
    mazeWidth  :: Word32
  , mazeHeight :: Word32
  , barrierCoords :: [ (Word32,Word32) ]
  , endCoords     :: [ (Word32,Word32) ]
} deriving (Eq)

instance Show Maze where 
    show (Maze width height barriers endcoords) = intercalate "\n" $ [ boundary ] ++ localrows ++ [ boundary ]
        where boundary = replicate (fromIntegral width + 2) '-'
              print_cell_state i j
                  | (i,j) `elem` barriers  = 'x'
                  | (i,j) `elem` endcoords = 'E'
                  | otherwise              = ' '
              row_interior j = map (`print_cell_state` j) [0..(width-1)]
              localrow j     = concat [ "|", row_interior j, "|" ]
              localrows      = map localrow (reverse [0..(height-1)])

data Action = MoveRight | MoveLeft | MoveUp | MoveDown 
  deriving (Show,Eq)

data MazeState = MazeState { 
    getMaze :: Maze
  , getLocationInMaze :: (Word32,Word32)
} deriving (Eq)

instance Show MazeState where 
    show (MazeState (Maze width height barriers endcoords) loc) = intercalate "\n" $ [ boundary ] ++ localrows ++ [ boundary ]
        where boundary = replicate (fromIntegral width + 2) '-'
              print_cell_state i j
                  | (i,j) `elem` barriers    = 'x'
                  | (i,j) `elem` endcoords   = 'E'
                  | (i,j) == loc             = 'o'
                  | otherwise                = ' '
              row_interior j = map (`print_cell_state` j) [0..(width-1)]
              localrow j     = concat [ "|", row_interior j, "|" ]
              localrows      = map localrow (reverse [0..(height-1)])

instance Ord MazeState where 
    compare = comparing getLocationInMaze

isAtTerminalState :: MazeState -> Bool
isAtTerminalState s = getLocationInMaze s `elem` (endCoords . getMaze $ s)

applyAction :: Action -> MazeState -> (Double,MazeState)
applyAction a s = (reward,normalized_new_state)
    where (orig_x, orig_y) = getLocationInMaze s
          (new_x, new_y)   = case a of 
              MoveRight -> (orig_x+1,orig_y)
              MoveLeft  -> if orig_x >=1 then (orig_x-1,orig_y) else (orig_x,orig_y) 
              MoveUp    -> (orig_x,orig_y+1)
              MoveDown  -> if orig_y >=1 then (orig_x,orig_y-1) else (orig_x,orig_y) 
          width            = mazeWidth  . getMaze $ s
          height           = mazeHeight . getMaze $ s
          normalized_new_loc
              | new_x >= width   = (width-1,new_y)
              | new_y >= height  = (new_x,height-1)
              | otherwise        = (new_x,new_y)
          normalized_new_state =
              if normalized_new_loc `elem` (barrierCoords . getMaze $ s) then s else s { getLocationInMaze = normalized_new_loc }
          reward = if isAtTerminalState normalized_new_state then 1.0 else 0.0
    
-- This implements the policy iteration algorithm from section 3.2.2 of 
-- "Reinforcement Learning: A Survey" by Kaelbling, Littman, and Moore (1996).
iteratePolicy :: Maze -> [ ( (Word32,Word32), Action ) ] -> [ Action ] -> Double -> ( [ ( (Word32,Word32), Action ) ], Map.Map MazeState Double )
iteratePolicy maze policy actions gamma = ( new_policy, stateValueMap )
    where 
        coords = map fst policy
        identityMat = ident $ length coords :: Matrix Double
        mazeStateActionPairs = map (\(loc,action) -> (MazeState maze loc,action)) policy
        mazeStates    = map fst mazeStateActionPairs 
        actionResults = map (uncurry $ flip applyAction) mazeStateActionPairs 
        newLocs       = map (getLocationInMaze . snd) actionResults
        newLocIdxs    = map (`elemIndex` coords) newLocs
        idxPairs0     = zip [0..] newLocIdxs  -- type is [(Int,Maybe Int)]
        index_processor (x,maybe_y) ls = case maybe_y of 
            Just y    -> (x,y) : ls
            Nothing   -> ls -- Shouldn't happen
        idxPairs      = foldr index_processor [] idxPairs0
        negGammaList  = replicate (length idxPairs) (-gamma)
        rewardsVec    = asColumn $ fromList $ map fst actionResults
        id_minus_T    = accum identityMat (+) (zip idxPairs negGammaList)
        valueMat      = linearSolveLS id_minus_T rewardsVec 
        values        = toList . flatten $ valueMat :: [Double]
        stateValuePairs = zip mazeStates values
        stateValueMap   = Map.fromList stateValuePairs -- :: Map.Map MazeState Double
        -- We now have a map from the maze state to the value.  
        -- We now update the action
        rewardNewMazeStatePairsForState state = map (`applyAction` state) actions -- Get a list of rewards and new maze states
        valuesForState state = map (\(reward,newState) -> reward + gamma * Map.findWithDefault 0.0 newState stateValueMap) $ rewardNewMazeStatePairsForState state
        actionValuePairsForState state  = zip actions (valuesForState state)
        optimalActionAndValueForState state = maximumBy (\(_,v1) (_,v2) -> compare v1 v2) $ actionValuePairsForState state
        new_actions = map (fst . optimalActionAndValueForState) mazeStates
        new_policy  = zip coords new_actions 

main :: IO ()
main = do 
        print examplemaze
        let coords = filter (notMazeBarrier examplemaze) [ (x,y) | x <- [0..(width-1)], y <- [0..(height-1)] ] 
        let initpolicy = map (\c -> (c,MoveUp)) coords
        let actions = [ MoveLeft, MoveUp, MoveRight, MoveDown ]
        let gamma = 0.99
        let (optimal_policy,state_value_map) = findBestPolicy examplemaze initpolicy actions gamma 
        putStrLn "Printing optimal policy and values for each coordinate: "
        forM_ optimal_policy
              (\(coord, action) -> putStrLn $ unwords [ "Optiomal action at" 
                                                      , show coord 
                                                      , "is" 
                                                      , show action
                                                      , "with value"
                                                      , show $ Map.findWithDefault 0.0 (MazeState examplemaze coord) state_value_map ])
    where 
        width = 3
        height = 3
        barrier_coords = [ (0,1), (1,1) ]
        end_coords = [(0,2)]
        examplemaze = Maze width height barrier_coords end_coords

        notMazeBarrier maze = not . (`elem` (barrierCoords maze ++ endCoords maze))
        findBestPolicy maze policy actions gamma = 
            let (updated_policy, policy_value_map) = iteratePolicy maze policy actions gamma 
                original_actions = map snd policy
                updated_actions = map snd updated_policy
                policy_unchanged = all (uncurry (==)) $ zip original_actions updated_actions 
            in 
                if policy_unchanged then (updated_policy,policy_value_map)
                else findBestPolicy maze updated_policy actions gamma

