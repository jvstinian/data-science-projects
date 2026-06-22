module Gym.Registration
    ( moduleName
    , romIdToName
    , getAllRomIds ) where

import Data.Char (isAlphaNum, toUpper, toLower)
import Gym.Env 
    ( AtariEnvParams(repeatActionProbability, fullActionSpace, frameskip, maxNumFramesPerEpisode)
    , defaultAtariEnvParams
    , Frameskip(FrameskipLowHigh, FrameskipValue) )
import Gym.Roms hiding (moduleName)

moduleName :: IO ()
moduleName = putStrLn "Gym.Registration "

-- """Registration for Atari environments."""
-- 
-- import ale_py.roms as roms

-- The following is just a modification of the words function
titlewords :: String -> [String]
{-# NOINLINE [1] titlewords  #-}
titlewords s = case dropWhile isNotAlphaNum s of
                "" -> []
                s' -> w : words s''
                    where (w, s'') = break isNotAlphaNum s'
    where isNotAlphaNum = not . isAlphaNum

makeUpperCase :: String -> String
makeUpperCase s = case s of
    u : r -> toUpper u : map toLower r
    "" -> ""

--     """Convert a rom id in snake_case to the name in PascalCase."""
romIdToName :: String -> String
romIdToName = concat . map makeUpperCase . titlewords

data AtariRomEnvParams = AtariRomEnvParams
    { romName :: String
    , romEnvParams :: AtariEnvParams }
    deriving (Eq, Show)

-- We define a type to represent a simplified EnvSpec found in the Gymnasium python package.
-- We only specify the fields necessary for the Atari registrations below.
data GymEnvSpec = AtariEnvSpec
    { envSpecId :: String
    , entryPoint :: Maybe String
    , gymEnvParams :: AtariRomEnvParams }
    deriving (Eq, Show)

--     """Registers all v0 and v4 environments."""
register_v0_v4_envs :: [GymEnvSpec]
register_v0_v4_envs = concatMap createEnvSpecOverFrameskips legacyGames
    where legacyGames =
                [ "adventure" , "air_raid", "alien", "amidar"
                , "assault", "asterix", "asteroids", "atlantis"
                , "bank_heist", "battle_zone", "beam_rider", "berzerk"
                , "bowling", "boxing", "breakout", "carnival"
                , "centipede", "chopper_command", "crazy_climber", "defender"
                , "demon_attack", "double_dunk", "elevator_action", "enduro"
                , "fishing_derby", "freeway", "frostbite", "gopher"
                , "gravitar", "hero", "ice_hockey", "jamesbond"
                , "journey_escape", "kangaroo", "krull", "kung_fu_master"
                , "montezuma_revenge", "ms_pacman", "name_this_game", "phoenix"
                , "pitfall", "pong", "pooyan", "private_eye"
                , "qbert", "riverraid", "road_runner", "robotank"
                , "seaquest", "skiing", "solaris", "space_invaders"
                , "star_gunner", "tennis", "time_pilot", "tutankham"
                , "up_n_down", "venture", "video_pinball", "wizard_of_wor"
                , "yars_revenge", "zaxxon" ]

          createEnvSpec rom suffix fskip version repeat_action_probability = let
                   envname = romIdToName rom
                   envid = concat [envname, suffix, "-", version]
                   adjParams = defaultAtariEnvParams 
                        { repeatActionProbability = repeat_action_probability
                        , fullActionSpace = False
                        , frameskip = fskip
                        , maxNumFramesPerEpisode = Just 108000 }
                in AtariEnvSpec envid Nothing (AtariRomEnvParams rom adjParams)

          versionRepeatActionProbs = [("v0", 0.25), ("v4", 0.0)]

          createEnvSpecOverRepeatProbs rom suffix fskip = map (uncurry $ createEnvSpec rom suffix fskip) versionRepeatActionProbs
          
          suffixFrameskips = [("", FrameskipLowHigh 2 5), ("NoFrameskip", FrameskipValue 1)]

          createEnvSpecOverFrameskips rom = concatMap (uncurry $ createEnvSpecOverRepeatProbs rom) suffixFrameskips


-- def register_v0_v4_envs():
-- 
--     for rom in legacy_games:
--         for suffix, frameskip in (("", (2, 5)), ("NoFrameskip", 1)):
--             for version, repeat_action_probability in (("v0", 0.25), ("v4", 0.0)):
--                 name = rom_id_to_name(rom)
-- 
--                 # Register the environment
--                 gymnasium.register(
--                     id=f"{name}{suffix}-{version}",
--                     entry_point="ale_py.env:AtariEnv",
--                     kwargs=dict(
--                         game=rom,
--                         repeat_action_probability=repeat_action_probability,
--                         full_action_space=False,
--                         frameskip=frameskip,
--                         max_num_frames_per_episode=108_000,
--                     ),
--                 )
-- 
-- 

--     """Register all v5 environments."""
register_v5_envs :: IO [GymEnvSpec]
register_v5_envs = map createEnvSpec . filterGames <$> getAllRomIds
    where gamesOnly2Player = ["combat", "joust", "maze_craze", "warlords"]
          filterGames = filter (flip notElem gamesOnly2Player)
          createEnvSpec rom = let
                    name = romIdToName rom
                    envid = concat ["ALE/", name, "-v5"]
                    adjParams = defaultAtariEnvParams 
                        { repeatActionProbability = 0.25
                        , fullActionSpace = False
                        , frameskip = FrameskipValue 4
                        , maxNumFramesPerEpisode = Just 108000 }
                in AtariEnvSpec envid Nothing (AtariRomEnvParams rom adjParams)

--     for rom in all_games:
--         # These roms don't have a single-player ROM attached (do have a multi-player mode)
--         if rom in {"combat", "joust", "maze_craze", "warlords"}:
--             continue
-- 
--         name = rom_id_to_name(rom)
-- 
--         # max_episode_steps is 108k frames which is 30 mins of gameplay.
--         # This corresponds to 108k / 4 = 27,000 steps
--         gymnasium.register(
--             id=f"ALE/{name}-v5",
--             entry_point="ale_py.env:AtariEnv",
--             vector_entry_point="ale_py.vector_env:AtariVectorEnv",
--             kwargs=dict(
--                 game=rom,
--                 repeat_action_probability=0.25,
--                 full_action_space=False,
--                 frameskip=4,
--                 max_num_frames_per_episode=108_000,
--             ),
--         )

-- def register_v5_envs():
--     all_games = roms.get_all_rom_ids()
-- 
--     for rom in all_games:
--         # These roms don't have a single-player ROM attached (do have a multi-player mode)
--         if rom in {"combat", "joust", "maze_craze", "warlords"}:
--             continue
-- 
--         name = rom_id_to_name(rom)
-- 
--         # max_episode_steps is 108k frames which is 30 mins of gameplay.
--         # This corresponds to 108k / 4 = 27,000 steps
--         gymnasium.register(
--             id=f"ALE/{name}-v5",
--             entry_point="ale_py.env:AtariEnv",
--             vector_entry_point="ale_py.vector_env:AtariVectorEnv",
--             kwargs=dict(
--                 game=rom,
--                 repeat_action_probability=0.25,
--                 full_action_space=False,
--                 frameskip=4,
--                 max_num_frames_per_episode=108_000,
--             ),
--         )
