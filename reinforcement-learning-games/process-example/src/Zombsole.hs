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
  , GymObservation(GymObservation)
  , BasicObservation
  , GameStateParameters(GameStateParameters)
  , ZombsoleRequest(..)
  , ZombsoleResponse(..)
  , tlbotStrategy 
  ) where

import GHC.Generics
import Data.List (sortOn, zipWith4)
import Data.Maybe (listToMaybe, catMaybes)
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
    , gameConfig_minimum_zombies :: Int
    , gameConfig_observation_scope :: String
    , gameConfig_observation_position_encoding :: String }
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

data Position = Position Int Int
    deriving (Eq, Show)

data Weapon = ZombieClaws
            | Knife
            | Axe
            | Gun
            | Rifle
            | Shotgun
            | NoWeapon
    deriving (Eq, Show)

data Thing = Wall
           | Box
           | ObjectiveLocation
           | DeadBody
           | Zombie Int
           | Player Int Weapon
           | Agent Int Weapon
           | UnknownThing
    deriving (Eq, Show)

data ThingWithPosition = ThingWithPosition Position Thing
    deriving (Eq, Show)

type World = [ThingWithPosition]

parseWeaponCode :: Int -> Weapon
parseWeaponCode wc = case wc of 
    1  -> ZombieClaws
    10 -> Knife
    11 -> Axe
    12 -> Gun
    13 -> Rifle
    14 -> Shotgun
    _  -> NoWeapon

parseSimplePosition :: Int -> Maybe Thing
parseSimplePosition x = case thing_code of
    1 -> Just Box
    2 -> Just DeadBody
    3 -> Just ObjectiveLocation
    4 -> Just Wall
    5 -> Just $ Zombie life
    6 -> Just $ Player life (parseWeaponCode weapon_code)
    7 -> Just $ Agent life (parseWeaponCode weapon_code)
    _ -> Nothing
  where (x1, scaledLife) = quotRem x 16
        life = (100 * scaledLife) `div` 15
        (thing_code, weapon_code) = quotRem x1 16

parseSimpleObservation :: [[[Int]]] -> World
parseSimpleObservation sobs = case sobs of 
    obs : _ -> parsePositions obs
    _       -> []
  where parsePositions = concat . zipWith parseRow [0..]
        parseRow rownum = catMaybes . zipWith (parseCell rownum) [0..]
        -- The resulting type of the following is Maybe Thing
        parseCell rownum colnum p = fmap (ThingWithPosition (Position colnum rownum)) (parseSimplePosition p)

parseChannelsPosition :: Int -> Int -> Int -> Maybe Thing
parseChannelsPosition channelthingcode life weaponcode = case thingcode of
    1 -> Just Box
    2 -> Just DeadBody
    3 -> Just ObjectiveLocation
    4 -> Just Wall
    5 -> Just $ Zombie life
    6 -> Just $ Player life (parseWeaponCode weaponcode)
    7 -> Just $ Agent life (parseWeaponCode weaponcode)
    _ -> Nothing
  where thingcode = if channelthingcode >= 8 then 7 else channelthingcode
        -- TODO: Need to add an agent ID to the Agent
        -- agentid = if channelthingcode >= 8 then channelthingcode - 8 else -1

parseChannelsObservation :: [[[Int]]] -> World
parseChannelsObservation sobs = case sobs of 
    ch1 : ch2 : ch3 : _ -> parsePositions ch1 ch2 ch3
    _       -> []
  where parsePositions arr1 arr2 = concat . zipWith4 parseRow [0..] arr1 arr2
        parseRow rownum vec1 vec2 = catMaybes . zipWith4 (parseCell rownum) [0..] vec1 vec2
        -- The resulting type of the following is Maybe Thing
        parseCell rownum colnum val1 val2 val3 = fmap (ThingWithPosition (Position colnum rownum)) (parseChannelsPosition val1 val2 val3)

data PositionEncodingStyle = PESSimple
                           | PEFChannels
  deriving (Eq, Show)

parseObservation :: PositionEncodingStyle -> [[[Int]]] -> World
parseObservation PESSimple = parseSimpleObservation
parseObservation PEFChannels = parseChannelsObservation

{-
        if thing is not None:
            scaled_life = 16*getattr(thing, 'life', 0)//100
            thing_code = self.thing_labels.get(thing.icon_basic, 0)
            weapon = getattr(thing, 'weapon', None)
            weapon_name = weapon.name if weapon is not None else 'none'
            weapon_code = self.weapon_labels.get(weapon_name, 0)
            return 16*16*thing_code + 16*weapon_code + scaled_life
        else:
            return 0 
-}

getAgents :: World -> [ThingWithPosition]
getAgents world = 
  let isAgent thing = case thing of 
        Agent _ _ -> True
        _         -> False
      agents = filter (\(ThingWithPosition _ thing) -> isAgent thing) world
  in agents

getAgentPositions :: World -> [Position]
getAgentPositions world = map (\(ThingWithPosition position _) -> position) (getAgents world)

weaponRange :: Weapon -> Double
weaponRange ZombieClaws = 1.5
weaponRange Knife       = 1.5
weaponRange Axe         = 1.5
weaponRange Gun         = 6.0
weaponRange Rifle       = 10.0
weaponRange Shotgun     = 3.0
weaponRange NoWeapon    = 0.0

positionDistance :: Position -> Position -> Double
positionDistance (Position x1 y1) (Position x2 y2) = sqrt $ fromIntegral $ (x1 - x2) ^ (2 :: Int) + (y1 - y2) ^ (2 :: Int)

zombiesWithinRange :: Position -> Double -> World -> [Position]
zombiesWithinRange pos dist world = map unwrapPosition $ filter isZombieWithinRange world
  where isZombieWithinRange (ThingWithPosition tpos thing) = case thing of 
          Zombie _ -> positionDistance pos tpos <= dist
          _        -> False
        unwrapPosition (ThingWithPosition tpos _) = tpos

closestZombiePosition :: Position -> World -> Maybe Position
closestZombiePosition pos world = listToMaybe sortedZombiePositions
  where isZombie (ThingWithPosition _ thing) = case thing of 
          Zombie _ -> True
          _        -> False
        extractPosition (ThingWithPosition tpos _) = tpos
        zombiePositions = map extractPosition (filter isZombie world)
        sortedZombiePositions = sortOn (positionDistance pos) zombiePositions

adjacentPositions :: Position -> [Position]
adjacentPositions (Position x y) = [left, up, right, down]
  where left  = Position (x-1) y
        up    = Position x (y+1)
        right = Position (x+1) y
        down  = Position x (y-1)

getThingAtPosition :: World -> Position -> Maybe Thing
getThingAtPosition world pos = listToMaybe $ map unwrapThing (filter (positionMatch pos) world)
  where positionMatch pos (ThingWithPosition tpos _) = tpos == pos
        unwrapThing (ThingWithPosition _ thing) = thing

playerStep :: Position -> Weapon -> World -> Action
playerStep pos weapon world = maybe HealAction attackIfZombieClose zombieposM
  where zombieposM = closestZombiePosition pos world
        attackIfZombieClose zpos =  
          if positionDistance pos zpos <= weaponRange weapon
          then AttackClosestAction
          else actionWithDistantZombie zpos
        moveCloserToDistantZombie zpos = head $ sortOn (positionDistance zpos) (adjacentPositions pos)
        getMoveAction (Position cx cy) (Position tx ty) = MoveAction $ RelativeCoords (tx - cx) (ty - cy)
        actionWithDistantZombie zpos = let obspos = moveCloserToDistantZombie zpos
          in case getThingAtPosition world obspos of
                  Just targetThing -> HealAction -- We deviate from the Terminator bot here.  See comments below.
                  Nothing          -> getMoveAction pos obspos
        -- The Terminator bot would heal a player at the "obstacle" position
        -- or attack the obstacle position otherwise 
        -- As we don't support healing or attacking arbitrary locations 
        -- with the current actions, we can't engage in this activity.  We instead heal.

tlbotStrategy :: [BasicObservation] -> Action
tlbotStrategy (bobs : _) = action
  where world = parseSimpleObservation bobs
        agents = getAgents world
        action = case listToMaybe agents of 
                      Just (ThingWithPosition pos (Agent _ weapon)) -> playerStep pos weapon world
                      -- There should be an agent.  If the following is triggered, there's a serious issue, 
                      -- which is why we pass an invalid action.
                      _                                             -> MoveAction $ RelativeCoords 0 2
-- tlbotStrategy _ = AttackClosestAction -- This shouldn't happen
tlbotStrategy _ = MoveAction $ RelativeCoords 0 (-2)


{-
 - Terminator logic
    def next_step(self, things, t):
        zombies = [thing for thing in things.values()
                   if isinstance(thing, Zombie)]

        if zombies:
            target = closest(self, zombies)
            if distance(self, target) > self.weapon.max_range:
                best_move = closest(target, adjacent_positions(self))
                obstacle = things.get(best_move)
                if obstacle:
                    if isinstance(obstacle, Player):
                        # zombie not in range. Player blocking path. Heal it.
                        return 'heal', obstacle
                    else:
                        # zombie not in range. Obstacle in front. Shoot it.
                        self.status = u'shooting obstacle to chase target'
                        return 'attack', obstacle
                else:
                    # zombie not in range. Not obstacle. Move.
                    self.status = u'chasing target'
                    return 'move', best_move
            else:
                # zombie in range. Shoot it.
                self.status = u'shooting target'
                return 'attack', target
        else:
            # no zombies. Heal.
            self.status = u'no targets, healing'
            return 'heal', self
-}
