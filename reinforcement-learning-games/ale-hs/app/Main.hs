module Main where

import qualified MyLib (someFunc)
import qualified InlineC (c_cos, faster_c_cos)
import qualified Ale
    ( hello, welcomeMessage
    , newAleInterface, getEpisodeFrameNumber
    , setInt, setBool, loadRom
    , Action(..), act
    , lives
    , getAvailableModes
    , getAvailableDifficulties
    , getLegalActionSet
    , getMinimalActionSet
    , getScreenGrayscale
    , getScreenRGB
    , saveScreenPNG )
import qualified Gym.Roms as Roms (getRomPath)
-- import Foreign.C.String (peekCAString)

main :: IO ()
main = do
  let breakout = "/nix/store/ywni35n1b5alf045w58barll7kzbh9b0-python3.12-ale-py-0.10.1/lib/python3.12/site-packages/ale_py/roms/breakout.bin"
      -- breakout = "/home/justinian/Code/datascience-miscellaneous/reinforcement-learning-games/tetris26/tetris.bin"
      paddlestrength = 1.0 :: Float
  putStrLn "Hello, Haskell!"
  tetrisPath <- Roms.getRomPath "tetris"
  print tetrisPath
  MyLib.someFunc
  InlineC.c_cos 0.0 >>= (putStrLn . ("cos(0) = " ++) . show)
  InlineC.faster_c_cos 1.57 >>= (putStrLn . ("cos(1.57) = " ++) . show)
  -- putStrLn $ "cos(1.57) = " ++ show (faster_c_cos 1.57)
  Ale.hello -- >>= (putStrLn . ("Ale: " ++) . show)
  Ale.welcomeMessage >>= (putStrLn . ("Ale: " ++))
  aleiface <- Ale.newAleInterface
  Ale.setInt aleiface "random_seed" 123
  Ale.setBool aleiface "display_screen" True
  Ale.loadRom aleiface breakout
  Ale.getAvailableModes aleiface >>= (putStrLn . ("Game modes: " ++) . show)
  Ale.getAvailableDifficulties aleiface >>= (putStrLn . ("Game difficulties: " ++) . show)
  Ale.getLegalActionSet aleiface >>= (putStrLn . ("Legal actions: " ++) . show)
  Ale.getMinimalActionSet aleiface >>= (putStrLn . ("Legal actions: " ++) . show)
  Ale.act aleiface Ale.PLAYER_A_FIRE  paddlestrength >>= (putStrLn . ("Reward from fire: " ++) . show)
  Ale.act aleiface Ale.PLAYER_A_RIGHT paddlestrength >>= (putStrLn . ("Reward from moving right: " ++) . show)
  Ale.act aleiface Ale.PLAYER_A_RIGHT paddlestrength >>= (putStrLn . ("Reward from moving right: " ++) . show)
  Ale.act aleiface Ale.PLAYER_A_LEFT  paddlestrength >>= (putStrLn . ("Reward from moving left: " ++) . show)
  Ale.act aleiface Ale.PLAYER_A_LEFT  paddlestrength >>= (putStrLn . ("Reward from moving left: " ++) . show)
  -- playGame aleiface (320 :: Int)
  Ale.getEpisodeFrameNumber aleiface >>= (putStrLn . ("Episode Frame Number: " ++) . show)
  Ale.getScreenGrayscale aleiface >>= (\(r, c, screenvec) -> putStrLn $ "Screen rows: " ++ show r ++ ", columns: " ++ show c ++ ", length of grayscale screen: " ++ show (length screenvec))
  Ale.getScreenRGB aleiface >>= (\(r, c, screenvec) -> putStrLn $ "RGB Screen rows: " ++ show r ++ ", columns: " ++ show c ++ ", length of RGB screen: " ++ show (length screenvec))
  Ale.saveScreenPNG aleiface "./output.png"
  where playGame aleinterface num_steps = do
            let action | (num_steps `div` 2) `mod` 2 == 0 = Ale.PLAYER_A_RIGHT
                       | otherwise                        = Ale.PLAYER_A_LEFT
                paddlestrength = 1.0 :: Float
            Ale.act aleinterface action paddlestrength >>= (putStrLn . ("Reward: " ++) . show)
            Ale.lives aleinterface >>= (putStrLn . ("Lives: " ++) . show)
            if num_steps > 0
            then playGame aleinterface (num_steps - 1)
            else return ()
            

{- Running: /nix/store/wi8b7895zmzv4h1kva0bmw0p4lqvxm0l-ghc-9.6.6-with-packages/bin/ghc --make -no-link -fbuilding-cabal-package -O -static -outputdir /home/justinian/Code/datascience-miscellaneous/reinforcement-learning-games/ale-hs/dist-newstyle/build/x86_64-linux/ghc-9.6.6/ale-hs-0.1.0.0/x/ale-hs/build/ale-hs/ale-hs-tmp -odir /home/justinian/Code/datascience-miscellaneous/reinforcement-learning-games/ale-hs/dist-newstyle/build/x86_64-linux/ghc-9.6.6/ale-hs-0.1.0.0/x/ale-hs/build/ale-hs/ale-hs-tmp -hidir /home/justinian/Code/datascience-miscellaneous/reinforcement-learning-games/ale-hs/dist-newstyle/build/x86_64-linux/ghc-9.6.6/ale-hs-0.1.0.0/x/ale-hs/build/ale-hs/ale-hs-tmp -hiedir /home/justinian/Code/datascience-miscellaneous/reinforcement-learning-games/ale-hs/dist-newstyle/build/x86_64-linux/ghc-9.6.6/ale-hs-0.1.0.0/x/ale-hs/build/ale-hs/ale-hs-tmp/extra-compilation-artifacts/hie -stubdir /home/justinian/Code/datascience-miscellaneous/reinforcement-learning-games/ale-hs/dist-newstyle/build/x86_64-linux/ghc-9.6.6/ale-hs-0.1.0.0/x/ale-hs/build/ale-hs/ale-hs-tmp -i -iapp -i/home/justinian/Code/datascience-miscellaneous/reinforcement-learning-games/ale-hs/dist-newstyle/build/x86_64-linux/ghc-9.6.6/ale-hs-0.1.0.0/x/ale-hs/build/ale-hs/ale-hs-tmp -i/home/justinian/Code/datascience-miscellaneous/reinforcement-learning-games/ale-hs/dist-newstyle/build/x86_64-linux/ghc-9.6.6/ale-hs-0.1.0.0/x/ale-hs/build/ale-hs/autogen -i/home/justinian/Code/datascience-miscellaneous/reinforcement-learning-games/ale-hs/dist-newstyle/build/x86_64-linux/ghc-9.6.6/ale-hs-0.1.0.0/x/ale-hs/build/global-autogen -I/home/justinian/Code/datascience-miscellaneous/reinforcement-learning-games/ale-hs/dist-newstyle/build/x86_64-linux/ghc-9.6.6/ale-hs-0.1.0.0/x/ale-hs/build/ale-hs/autogen -I/home/justinian/Code/datascience-miscellaneous/reinforcement-learning-games/ale-hs/dist-newstyle/build/x86_64-linux/ghc-9.6.6/ale-hs-0.1.0.0/x/ale-hs/build/global-autogen -I/home/justinian/Code/datascience-miscellaneous/reinforcement-learning-games/ale-hs/dist-newstyle/build/x86_64-linux/ghc-9.6.6/ale-hs-0.1.0.0/x/ale-hs/build/ale-hs/ale-hs-tmp -optP-include -optP/home/justinian/Code/datascience-miscellaneous/reinforcement-learning-games/ale-hs/dist-newstyle/build/x86_64-linux/ghc-9.6.6/ale-hs-0.1.0.0/x/ale-hs/build/ale-hs/autogen/cabal_macros.h -this-unit-id ale-hs-0.1.0.0-inplace-ale-hs -hide-all-packages -Wmissing-home-modules -no-user-package-db -package-db /home/justinian/.local/state/cabal/store/ghc-9.6.6/package.db -package-db /home/justinian/Code/datascience-miscellaneous/reinforcement-learning-games/ale-hs/dist-newstyle/packagedb/ghc-9.6.6 -package-db /home/justinian/Code/datascience-miscellaneous/reinforcement-learning-games/ale-hs/dist-newstyle/build/x86_64-linux/ghc-9.6.6/ale-hs-0.1.0.0/x/ale-hs/package.conf.inplace -package-id ale-hs-0.1.0.0-inplace -package-id base-4.18.2.1 -package-id inline-c-0.9.1.10-D76GAfkM74pLa29elUEy2l -package-id inline-c-cpp-0.5.0.2-BY82m5wwVgCCDDrmXB1rjA -XHaskell2010 app/Main.hs -Wall -hide-all-packages

Linking...
Running: 
    /nix/store/wi8b7895zmzv4h1kva0bmw0p4lqvxm0l-ghc-9.6.6-with-packages/bin/ghc
    --make -fbuilding-cabal-package -O -static -outputdir /home/justinian/Code/datascience-miscellaneous/reinforcement-learning-games/ale-hs/dist-newstyle/build/x86_64-linux/ghc-9.6.6/ale-hs-0.1.0.0/x/ale-hs/build/ale-hs/ale-hs-tmp
    -odir /home/justinian/Code/datascience-miscellaneous/reinforcement-learning-games/ale-hs/dist-newstyle/build/x86_64-linux/ghc-9.6.6/ale-hs-0.1.0.0/x/ale-hs/build/ale-hs/ale-hs-tmp
    -hidir /home/justinian/Code/datascience-miscellaneous/reinforcement-learning-games/ale-hs/dist-newstyle/build/x86_64-linux/ghc-9.6.6/ale-hs-0.1.0.0/x/ale-hs/build/ale-hs/ale-hs-tmp
    -hiedir /home/justinian/Code/datascience-miscellaneous/reinforcement-learning-games/ale-hs/dist-newstyle/build/x86_64-linux/ghc-9.6.6/ale-hs-0.1.0.0/x/ale-hs/build/ale-hs/ale-hs-tmp/extra-compilation-artifacts/hie
    -stubdir /home/justinian/Code/datascience-miscellaneous/reinforcement-learning-games/ale-hs/dist-newstyle/build/x86_64-linux/ghc-9.6.6/ale-hs-0.1.0.0/x/ale-hs/build/ale-hs/ale-hs-tmp
    -i -iapp -i/home/justinian/Code/datascience-miscellaneous/reinforcement-learning-games/ale-hs/dist-newstyle/build/x86_64-linux/ghc-9.6.6/ale-hs-0.1.0.0/x/ale-hs/build/ale-hs/ale-hs-tmp
    -i/home/justinian/Code/datascience-miscellaneous/reinforcement-learning-games/ale-hs/dist-newstyle/build/x86_64-linux/ghc-9.6.6/ale-hs-0.1.0.0/x/ale-hs/build/ale-hs/autogen
    -i/home/justinian/Code/datascience-miscellaneous/reinforcement-learning-games/ale-hs/dist-newstyle/build/x86_64-linux/ghc-9.6.6/ale-hs-0.1.0.0/x/ale-hs/build/global-autogen
    -I/home/justinian/Code/datascience-miscellaneous/reinforcement-learning-games/ale-hs/dist-newstyle/build/x86_64-linux/ghc-9.6.6/ale-hs-0.1.0.0/x/ale-hs/build/ale-hs/autogen
    -I/home/justinian/Code/datascience-miscellaneous/reinforcement-learning-games/ale-hs/dist-newstyle/build/x86_64-linux/ghc-9.6.6/ale-hs-0.1.0.0/x/ale-hs/build/global-autogen
    -I/home/justinian/Code/datascience-miscellaneous/reinforcement-learning-games/ale-hs/dist-newstyle/build/x86_64-linux/ghc-9.6.6/ale-hs-0.1.0.0/x/ale-hs/build/ale-hs/ale-hs-tmp
    -optP-include -optP/home/justinian/Code/datascience-miscellaneous/reinforcement-learning-games/ale-hs/dist-newstyle/build/x86_64-linux/ghc-9.6.6/ale-hs-0.1.0.0/x/ale-hs/build/ale-hs/autogen/cabal_macros.h
    -this-unit-id ale-hs-0.1.0.0-inplace-ale-hs
    -hide-all-packages -Wmissing-home-modules -no-user-package-db -package-db /home/justinian/.local/state/cabal/store/ghc-9.6.6/package.db
    -package-db /home/justinian/Code/datascience-miscellaneous/reinforcement-learning-games/ale-hs/dist-newstyle/packagedb/ghc-9.6.6
    -package-db /home/justinian/Code/datascience-miscellaneous/reinforcement-learning-games/ale-hs/dist-newstyle/build/x86_64-linux/ghc-9.6.6/ale-hs-0.1.0.0/x/ale-hs/package.conf.inplace
    -package-id ale-hs-0.1.0.0-inplace
    -package-id base-4.18.2.1
    -package-id inline-c-0.9.1.10-D76GAfkM74pLa29elUEy2l
    -package-id inline-c-cpp-0.5.0.2-BY82m5wwVgCCDDrmXB1rjA
    -XHaskell2010 app/Main.hs -o /home/justinian/Code/datascience-miscellaneous/reinforcement-learning-games/ale-hs/dist-newstyle/build/x86_64-linux/ghc-9.6.6/ale-hs-0.1.0.0/x/ale-hs/build/ale-hs/ale-hs -Wall -hide-all-packages

-}
