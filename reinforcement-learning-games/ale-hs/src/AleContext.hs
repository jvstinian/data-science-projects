{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module AleContext 
    ( aleCtx
    , C'ALEInterface
    , C'Reward
    , C'GameMode
    , C'GameDifficulty
    , Action(..)
    , C'Pixel ) where

import qualified Data.Map as M
import Foreign.Storable (Storable(..))
import Foreign.Ptr (Ptr, castPtr)
-- import Data.Int(Int32)
-- import Data.Word(Word32)
import Foreign.C.Types (CInt, CUInt, CUChar)
import qualified Language.C.Inline as C
import qualified Language.C.Types as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Inline.Cpp as C


data C'ALEInterface

type C'Reward = CInt
type C'GameMode = CUInt
type C'GameDifficulty = CUInt
type C'Pixel = CUChar

data Action = PLAYER_A_NOOP 
            | PLAYER_A_FIRE
            | PLAYER_A_UP
            | PLAYER_A_RIGHT
            | PLAYER_A_LEFT
            | PLAYER_A_DOWN
            | PLAYER_A_UPRIGHT
            | PLAYER_A_UPLEFT
            | PLAYER_A_DOWNRIGHT
            | PLAYER_A_DOWNLEFT
            | PLAYER_A_UPFIRE
            | PLAYER_A_RIGHTFIRE
            | PLAYER_A_LEFTFIRE
            | PLAYER_A_DOWNFIRE
            | PLAYER_A_UPRIGHTFIRE
            | PLAYER_A_UPLEFTFIRE
            | PLAYER_A_DOWNRIGHTFIRE
            | PLAYER_A_DOWNLEFTFIRE
            | PLAYER_B_NOOP
            | PLAYER_B_FIRE
            | PLAYER_B_UP
            | PLAYER_B_RIGHT
            | PLAYER_B_LEFT
            | PLAYER_B_DOWN
            | PLAYER_B_UPRIGHT
            | PLAYER_B_UPLEFT
            | PLAYER_B_DOWNRIGHT
            | PLAYER_B_DOWNLEFT
            | PLAYER_B_UPFIRE
            | PLAYER_B_RIGHTFIRE
            | PLAYER_B_LEFTFIRE
            | PLAYER_B_DOWNFIRE
            | PLAYER_B_UPRIGHTFIRE
            | PLAYER_B_UPLEFTFIRE
            | PLAYER_B_DOWNRIGHTFIRE
            | PLAYER_B_DOWNLEFTFIRE
            | RESET
            | UNDEFINED
            | RANDOM
            | SAVE_STATE
            | LOAD_STATE
            | SYSTEM_RESET
            | LAST_ACTION_INDEX
  deriving (Show, Eq, Ord)

instance Enum Action where
  fromEnum PLAYER_A_NOOP          = 0
  fromEnum PLAYER_A_FIRE          = 1
  fromEnum PLAYER_A_UP            = 2
  fromEnum PLAYER_A_RIGHT         = 3
  fromEnum PLAYER_A_LEFT          = 4
  fromEnum PLAYER_A_DOWN          = 5
  fromEnum PLAYER_A_UPRIGHT       = 6
  fromEnum PLAYER_A_UPLEFT        = 7
  fromEnum PLAYER_A_DOWNRIGHT     = 8
  fromEnum PLAYER_A_DOWNLEFT      = 9
  fromEnum PLAYER_A_UPFIRE        = 10
  fromEnum PLAYER_A_RIGHTFIRE     = 11
  fromEnum PLAYER_A_LEFTFIRE      = 12
  fromEnum PLAYER_A_DOWNFIRE      = 13
  fromEnum PLAYER_A_UPRIGHTFIRE   = 14
  fromEnum PLAYER_A_UPLEFTFIRE    = 15
  fromEnum PLAYER_A_DOWNRIGHTFIRE = 16
  fromEnum PLAYER_A_DOWNLEFTFIRE  = 17
  fromEnum PLAYER_B_NOOP          = 18
  fromEnum PLAYER_B_FIRE          = 19
  fromEnum PLAYER_B_UP            = 20
  fromEnum PLAYER_B_RIGHT         = 21
  fromEnum PLAYER_B_LEFT          = 22
  fromEnum PLAYER_B_DOWN          = 23
  fromEnum PLAYER_B_UPRIGHT       = 24
  fromEnum PLAYER_B_UPLEFT        = 25
  fromEnum PLAYER_B_DOWNRIGHT     = 26
  fromEnum PLAYER_B_DOWNLEFT      = 27
  fromEnum PLAYER_B_UPFIRE        = 28
  fromEnum PLAYER_B_RIGHTFIRE     = 29
  fromEnum PLAYER_B_LEFTFIRE      = 30
  fromEnum PLAYER_B_DOWNFIRE      = 31
  fromEnum PLAYER_B_UPRIGHTFIRE   = 32
  fromEnum PLAYER_B_UPLEFTFIRE    = 33
  fromEnum PLAYER_B_DOWNRIGHTFIRE = 34
  fromEnum PLAYER_B_DOWNLEFTFIRE  = 35
  fromEnum RESET                  = 40 -- MGB: Use SYSTEM_RESET to reset the environment.
  fromEnum UNDEFINED              = 41
  fromEnum RANDOM                 = 42
  fromEnum SAVE_STATE             = 43
  fromEnum LOAD_STATE             = 44
  fromEnum SYSTEM_RESET           = 45
  fromEnum LAST_ACTION_INDEX      = 50

  toEnum  0 = PLAYER_A_NOOP
  toEnum  1 = PLAYER_A_FIRE
  toEnum  2 = PLAYER_A_UP
  toEnum  3 = PLAYER_A_RIGHT
  toEnum  4 = PLAYER_A_LEFT
  toEnum  5 = PLAYER_A_DOWN
  toEnum  6 = PLAYER_A_UPRIGHT
  toEnum  7 = PLAYER_A_UPLEFT
  toEnum  8 = PLAYER_A_DOWNRIGHT
  toEnum  9 = PLAYER_A_DOWNLEFT
  toEnum 10 = PLAYER_A_UPFIRE
  toEnum 11 = PLAYER_A_RIGHTFIRE
  toEnum 12 = PLAYER_A_LEFTFIRE
  toEnum 13 = PLAYER_A_DOWNFIRE
  toEnum 14 = PLAYER_A_UPRIGHTFIRE
  toEnum 15 = PLAYER_A_UPLEFTFIRE
  toEnum 16 = PLAYER_A_DOWNRIGHTFIRE
  toEnum 17 = PLAYER_A_DOWNLEFTFIRE
  toEnum 18 = PLAYER_B_NOOP
  toEnum 19 = PLAYER_B_FIRE
  toEnum 20 = PLAYER_B_UP
  toEnum 21 = PLAYER_B_RIGHT
  toEnum 22 = PLAYER_B_LEFT
  toEnum 23 = PLAYER_B_DOWN
  toEnum 24 = PLAYER_B_UPRIGHT
  toEnum 25 = PLAYER_B_UPLEFT
  toEnum 26 = PLAYER_B_DOWNRIGHT
  toEnum 27 = PLAYER_B_DOWNLEFT
  toEnum 28 = PLAYER_B_UPFIRE
  toEnum 29 = PLAYER_B_RIGHTFIRE
  toEnum 30 = PLAYER_B_LEFTFIRE
  toEnum 31 = PLAYER_B_DOWNFIRE
  toEnum 32 = PLAYER_B_UPRIGHTFIRE
  toEnum 33 = PLAYER_B_UPLEFTFIRE
  toEnum 34 = PLAYER_B_DOWNRIGHTFIRE
  toEnum 35 = PLAYER_B_DOWNLEFTFIRE
  toEnum 40 = RESET                  -- MGB: Use SYSTEM_RESET to reset the environment.
  toEnum 41 = UNDEFINED
  toEnum 42 = RANDOM
  toEnum 43 = SAVE_STATE
  toEnum 44 = LOAD_STATE
  toEnum 45 = SYSTEM_RESET
  toEnum 50 = LAST_ACTION_INDEX
  toEnum x = error $ "toEnum {Action}: Invalid value: " ++ show x

{-
instance Storable Action where
  -- sizeOf _ = sizeOf (undefined :: CInt)
  sizeOf = sizeOf . enumToCint
      where enumToCint = fromIntegral . fromEnum :: Action -> CInt

  -- alignment _ = alignment (undefined :: CInt)
  alignment = alignment . enumToCint
      where enumToCint = fromIntegral . fromEnum :: Action -> CInt

  -- Read value as a CInt and convert to an Action
  peek p = do
    intVal <- peek (castPtr p :: Ptr CInt)
    return $ toEnum (fromIntegral intVal)

  -- Store as a CInt
  poke p action = do
    poke (castPtr p :: Ptr CInt) (fromIntegral . fromEnum $ action)
-}

aleCtx :: C.Context
aleCtx = C.cppCtx <> C.bsCtx <> C.vecCtx <> ctx
  where
    ctx = mempty { C.ctxTypesTable = aleTypesTable }

aleTypesTable :: C.TypesTable
aleTypesTable = M.fromList [ ( C.TypeName "ALEInterface" , [t| C'ALEInterface |] )
                           , ( C.TypeName "reward_t"     , [t| C'Reward |] )
                           , ( C.TypeName "game_mode_t"  , [t| C'GameMode |] )
                           , ( C.TypeName "Action"       , [t| CInt |] )
                           , ( C.TypeName "difficulty_t" , [t| C'GameDifficulty |] )
                           , ( C.TypeName "pixel_t"      , [t| C'Pixel|] ) ]

