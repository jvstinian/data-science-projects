{-# LANGUAGE OverloadedStrings #-}
import System.IO (hPutStr, hPutStrLn, hClose, hGetLine, hFlush)
import System.Process.Typed
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import Control.Concurrent.STM (atomically)
import Control.Exception (throwIO)
import Data.Aeson (encode, decode)
import Zombsole 
  ( ZombsoleRequest(GameStatus, StartGame, Exit, GameConfigUpdate), GameConfig(GameConfig)
  , ZombsoleResponse )

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
                  
                  hPutStrLn hin $ L8.unpack $ encode (GameConfigUpdate $ GameConfig "extermination" "bridge" ["terminator"] ["a0"] 10 0)
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

