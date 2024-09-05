{-|
Module      : Zombsole

This module defines types for interacting with zombsole using JSON
-}
{-# LANGUAGE DeriveGeneric #-}
module Zombsole
  ( message
  , GameConfig(GameConfig)
  , RelativeCoords(RelativeCoords)
  , Action(MoveAction, AttackClosestAction, HealAction)
  , ZombsoleRequest(..)
  , ZombsoleResponse(..)
  ) where

import GHC.Generics
import Data.Aeson 
  ( SumEncoding(TaggedObject, tagFieldName, contentsFieldName)
  , ToJSON(toEncoding)
  , genericToEncoding
  , FromJSON(parseJSON)
  , genericParseJSON
  , Options(fieldLabelModifier, constructorTagModifier, sumEncoding)
  , defaultOptions
  , Value(Array) )

message :: IO String
message = return "Test message"

zombsoleTaggedObject :: SumEncoding
zombsoleTaggedObject = TaggedObject { tagFieldName = "tag", contentsFieldName = "parameters" }

actionTaggedObject :: SumEncoding
actionTaggedObject = TaggedObject { tagFieldName = "action_type", contentsFieldName = "parameter" }

data GameConfig = GameConfig 
    { gameConfig_rules_name :: String
    , gameConfig_map_name :: String
    , gameConfig_players :: [String]
    , gameConfig_agent_ids :: [String]
    , gameConfig_initial_zombies :: Int
    , gameConfig_minimum_zombies :: Int }
    deriving (Eq, Show, Generic)

instance FromJSON GameConfig where
    parseJSON = genericParseJSON (defaultOptions { fieldLabelModifier = Prelude.drop (Prelude.length "gameConfig_")
                                                 , sumEncoding = zombsoleTaggedObject })

instance ToJSON GameConfig where
    toEncoding = genericToEncoding (defaultOptions { fieldLabelModifier = Prelude.drop (Prelude.length "gameConfig_")
                                                   , sumEncoding = zombsoleTaggedObject })

data RelativeCoords = RelativeCoords Int Int
    deriving (Eq, Show, Generic)

instance ToJSON RelativeCoords where
    toEncoding (RelativeCoords x y) = toEncoding [x, y]

data Action = MoveAction RelativeCoords
            | AttackClosestAction
            | HealAction
    deriving (Eq, Show, Generic)

actionConstructorTagMap :: String -> String
actionConstructorTagMap tag | tag == "MoveAction" = "move"
                            | tag == "AttackClosestAction" = "attack_closest"
                            | tag == "HealAction" = "heal"
                            | otherwise = "heal"

instance ToJSON Action where
    toEncoding = genericToEncoding (defaultOptions { constructorTagModifier = actionConstructorTagMap
                                                   , sumEncoding = actionTaggedObject })

-- TODO: Probably have to drop "Request" from the following names
data ZombsoleRequest = GameConfigUpdate GameConfig
                     | GameStatus
                     | Exit
                     | StartGame
                     | GameAction Action
    deriving (Eq, Show, Generic)

instance ToJSON ZombsoleRequest where
    toEncoding = genericToEncoding (defaultOptions { sumEncoding = zombsoleTaggedObject })

type BasicObservation = [[[Int]]]

data GymObservation = GymObservation 
    { observation :: BasicObservation
    , reward :: Double
    , done :: Bool
    , truncated :: Bool } -- omitting info as we won't use it for now
    deriving (Eq, Show, Generic)

instance FromJSON GymObservation where
    parseJSON = genericParseJSON defaultOptions

data GameStateParameters = GameStateParameters 
    { status :: String
    , active :: Bool
    , config_required :: Bool
    , last_observation :: Maybe GymObservation {- TODO: Do we want BasicObservation here? -} }
    deriving (Eq, Show, Generic)

instance FromJSON GameStateParameters where
    parseJSON = genericParseJSON defaultOptions

data ZombsoleResponse = GameState GameStateParameters
                      | GameObservation GymObservation
                      | Error String
    deriving (Eq, Show, Generic)

instance FromJSON ZombsoleResponse where
    parseJSON = genericParseJSON (defaultOptions { sumEncoding = zombsoleTaggedObject })

{-
class GameStateResponse(GameResponse):
    def __init__(self, status: str, active: bool, config_required: bool, last_observation: Union[None, Dict] = None):
        self.status = status
        self.active = active
        self.config_required = config_required
        self.last_observation = last_observation

    def get_tag(self) -> str:
        return "GameState"
    
    def get_parameters(self) -> Dict:
        return {
            "status": self.status,
            "active": self.active,
            "config_required": self.config_required,
            "last_observation": self.last_observation
        }

class GameObservationResponse(GameResponse):
    def __init__(self, last_observation: Dict = None):
        self.last_observation = last_observation

    def get_tag(self) -> str:
        return "GameObservation"
    
    def get_parameters(self) -> Dict:
        return self.last_observation

class ErrorResponse(GameResponse):
    def __init__(self, message: str):
        self.message = message

    def get_tag(self) -> str:
        return "Error"
    
    def get_parameters(self) -> Dict:
        return self.message

-}

