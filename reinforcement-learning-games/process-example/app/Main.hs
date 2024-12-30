{-# LANGUAGE OverloadedStrings #-}
import System.IO (hPutStr, hPutStrLn, hClose, hGetLine, hFlush)
import System.Process.Typed
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import Control.Concurrent.STM (atomically)
import Control.Exception (throwIO)
import Data.Maybe (Maybe, fromMaybe)
import Data.Aeson (encode, decode)
import Zombsole 
  ( ZombsoleRequest(GameStatus, StartGame, Exit, GameConfigUpdate, GameAction)
  , GameConfig(GameConfig)
  , ZombsoleResponse(GameState, GameObservation, Error)
  , Action(MoveAction, AttackClosestAction, HealAction)
  , BasicObservation
  , GameStateParameters(GameStateParameters)
  , GymObservation(GymObservation) 
  , tlbotStrategy
  , PositionEncodingStyle(PESSimple, PESChannels)
  , ObservationScope(OSWorld, OSSurroundings)
  , GameEncodingStyle(GameEncodingStyle) )


{-
 - Need a game client manager to track 
 - the count of games played and the 
 - state of the current game.  
 - While a game is active, we need to take the 
 - last observation and decide on an action.
-}
data ZombsolePlayer = ZombsolePlayer 
  { playerId :: Int 
  , strategy :: GameEncodingStyle -> [BasicObservation] -> Action
  }

instance Show ZombsolePlayer where
    show (ZombsolePlayer id _) = show ("ZombsolePlayer " ++ show id)
{-
or maybe 
zombsoleStrategy :: [Observation] -> Action
-}

type GameHistory = [(BasicObservation, Double)]

{-
A GameManager could have the following fields
- total number of games to play
- current number of games played
- outcomes of completed games (start with observations and rewards)
- current game in progress

GameManager needs to be able to process the following responses: 
- GameState GameStateParameters -- if active and game count not exhausted, start game?
- GameObservation GymObservation -- store observation to current game history, if done then move to next game, else determine move
- Error String -- exit
-}

data GameManager = GameManager Int Int [GameHistory] GameHistory GameEncodingStyle ZombsolePlayer 
  deriving (Show)

incrementGameCounter :: GameManager -> GameManager
incrementGameCounter (GameManager n c history current ge player) = GameManager n (c+1) history current ge player

data ProcessAction = MakeRequest ZombsoleRequest
                   | Stop String
  deriving (Eq, Show)

gameManagement :: ZombsoleResponse -> GameManager -> (ProcessAction, GameManager)
-- TODO: 1. Eventually consider GameConfigUpdate GameConfig - DONE
--       2. Consider GameManager parameters - DONE
--       3. Need to consider active and config_required fields - DONE
gameManagement (GameState (GameStateParameters status active config_required _)) gm@(GameManager _ _ _ _ (GameEncodingStyle scope posenc) _) 
  | active && config_required = (MakeRequest $ GameConfigUpdate $ GameConfig "extermination" "bridge" ["terminator"] ["0"] 10 0 (show scope) (show posenc), gm)
  | active                    = (MakeRequest StartGame, incrementGameCounter gm)
  | otherwise                 = (Stop "Process no longer active", gm)
gameManagement (Error _) gm = (MakeRequest Exit, gm) -- TODO: Consider printing or returning the error string
gameManagement (GameObservation (GymObservation obs reward done truncated)) (GameManager n c history current ge player) = (req, updated_gm)
  where 
        -- GameEncodingStyle _ pes = ge
        finished = done || truncated
        current_game_history = (obs, reward) : current
        updated_current = if finished
                          then [] 
                          else current_game_history
        updated_history = if finished
                          then current_game_history : history
                          else history 
        updated_c       = if finished then c + 1 else c
        updated_gm      = GameManager n updated_c updated_history updated_current ge player
        req             | not finished                 = MakeRequest $ GameAction (strategy player ge (map fst current_game_history))
                        | finished && (updated_c <= n) = MakeRequest StartGame 
                        | otherwise                    = Stop "Have played the number of games specified"
 

main :: IO ()
main = do
    -- Run a process, print its exit code
    runProcess "true" >>= print
    runProcess "false" >>= print
    runProcess "zombsole -h" >>= print

    -- Check that the exit code is a success
    runProcess_ "true"
    -- This will throw an exception: runProcess_ "false"

    -- Capture output and error
    (dateOut, dateErr) <- readProcess_ "date"
    print (dateOut, dateErr)

    -- Use shell commands
    (dateOut2, dateErr2) <- readProcess_ "date >&2"
    print (dateOut2, dateErr2)

    -- Interact with a process
    let catConfig = setStdin createPipe
                  $ setStdout byteStringOutput
                  $ proc "cat" ["/etc/hosts", "-", "/etc/group"]
    withProcessWait_ catConfig $ \p -> do
        hPutStr (getStdin p) "\n\nHELLO\n"
        hPutStr (getStdin p) "WORLD\n\n\n"
        hClose (getStdin p)

        atomically (getStdout p) >>= L8.putStr
    
    -- Interact with a custom process
    let echoConfig = setStdin createPipe
                   $ setStdout createPipe
                   $ proc "echopy" []
    withProcessWait_ echoConfig $ \p -> do
        {-
        hPutStr (getStdin p) "message 1 i!\n"
        hFlush (getStdin p)
        hGetLine (getStdout p) >>= print
        hClose (getStdin p)
        hClose (getStdout p)
        -}
        echoProcessor 3 (getStdin p) (getStdout p)
    
    let zombsoleConfig = setStdin createPipe
                       $ setStdout createPipe
                       $ proc "zombsole-stdio-json" []
    withProcessWait_ zombsoleConfig $ \p -> do
        zombsoleProcessor (getStdin p) (getStdout p)

    putStrLn "Starting zombsoleProcessor2"
    let zombsoleConfig2 = setStdin createPipe
                        $ setStdout createPipe
                        $ proc "zombsole-stdio-json" []
    withProcessWait_ zombsoleConfig2 $ \p -> do
        -- zombsoleProcessor2 (getStdin p) (getStdout p) (GameManager 1 0 [] [] (ZombsolePlayer 0 (const AttackClosestAction)))
        -- zombsoleProcessor2 (getStdin p) (getStdout p) (GameManager 1 0 [] [] (GameEncodingStyle OSWorld PESSimple) (ZombsolePlayer 0 tlbotStrategy))
        zombsoleProcessor2 (getStdin p) (getStdout p) (GameManager 1 0 [] [] (GameEncodingStyle OSWorld PESChannels) (ZombsolePlayer 0 tlbotStrategy))
     
        where echoProcessor count hin hout 
                  | count <= 0 = do
                      hClose hin
                      hClose hout
                  | otherwise = do
                      hPutStr hin $ "message " ++ show count ++ "!\n"
                      hFlush hin
                      hGetLine hout >>= print
                      echoProcessor (count - 1) hin hout
     
              zombsoleProcessor hin hout = do
                  -- zombsole interactive publishes an initial game status when run
                  -- hGetLine hout >>= print
                  zombsoleResponseProcessor hout

                  hPutStrLn hin $ L8.unpack $ encode GameStatus
                  hFlush hin
                  -- hGetLine hout >>= print
                  zombsoleResponseProcessor hout
                  
                  hPutStrLn hin $ L8.unpack $ encode (GameConfigUpdate $ GameConfig "extermination" "bridge" ["terminator"] ["0"] 10 0 "surroundings:5" "channels")
                  hFlush hin
                  -- hGetLine hout >>= print
                  zombsoleResponseProcessor hout
                  
                  hPutStrLn hin $ L8.unpack $ encode StartGame
                  hFlush hin
                  -- hGetLine hout >>= print
                  zombsoleResponseProcessor hout

                  hPutStrLn hin $ L8.unpack $ encode Exit
                  hFlush hin
                  zombsoleResponseProcessor hout
                  -- hGetLine hout >>= print
                  hClose hin
                  hClose hout
              
              zombsoleResponseProcessor hout = do
                  message <- hGetLine hout
                  let respM = (decode . L8.pack) message :: Maybe ZombsoleResponse
                  case respM of 
                    Just resp -> print resp
                    Nothing   -> print $ "Unable to decode JSON response: " ++ message

              zombsoleResponseProcessor2 hout = do
                  message <- hGetLine hout
                  let respM = (decode . L8.pack) message :: Maybe ZombsoleResponse
                  return $ fromMaybe (Error $ "Unable to decode JSON response: " ++ message) respM

              zombsoleProcessor2 hin hout initgm = do
                  -- zombsole interactive publishes an initial game status when run
                  resp <- zombsoleResponseProcessor2 hout
                  let (action, gm) = gameManagement resp initgm
                  case action of 
                    MakeRequest req -> do
                      print req
                      hPutStrLn hin $ L8.unpack $ encode req
                      hFlush hin
                      zombsoleProcessor2 hin hout gm
                    Stop message    -> do
                      print message
                      hPutStrLn hin $ L8.unpack $ encode Exit
                      -- Am seeing python complain about broken pipes in some cases, so adding some logic
                      hFlush hin 
                      _ <- hGetLine hout -- process the last line to avoid the broken pipe in the python application
                      -- let lastRespM = (decode . L8.pack) message :: Maybe ZombsoleResponse
                      -- print lastRespM
                      hClose hin
                      hClose hout
 
