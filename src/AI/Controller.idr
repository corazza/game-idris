module AI.Controller

import Data.AVL.Dict

import Descriptors
import GameIO
import Exception
import Common
import Resources
import Events
import Physics.Vector2D

public export
data AIState = Initial | Roam | Chase

Cast String (Checked AIState) where
  cast "initial" = pure Initial
  cast "roam" = pure Roam
  cast "chase" = pure Chase
  cast _ = fail "AIState must be of \"roam\"|\"chase\""

-- TODO expand
public export
data AIAction = MoveRight | MoveLeft | ChangeDirection | Attack

export
Show AIAction where
  show MoveRight = "move right"
  show MoveLeft = "move left"
  show ChangeDirection = "change direction"
  show Attack = "attack"

Cast String (Checked AIAction) where
  cast "move right" = pure MoveRight
  cast "move left" = pure MoveLeft
  cast "change direction" = pure ChangeDirection
  cast "attack" = pure Attack
  cast _ = fail "action must be of \"attack\""

public export
record Transition where
  constructor MkTransition
  state : AIState
  action : Maybe AIAction

getState : Dict String JSON -> Checked AIState
getState dict = with Checked do
  state <- getString "state" dict
  cast state

ObjectCaster Transition where
  objectCast dict = with Checked do
    state <- getState dict
    action <- maybeFromString "action" cast dict
    pure $ MkTransition state action

-- additional parameters only affect handlers, i.e. whether they fire,
-- which arguments they give, etc.
public export
record Handlers where
  constructor MkHandlers
  onCollision : Maybe Transition
  onTime : Maybe (Double, Transition) -- wait time

getCollision : Dict String JSON -> Checked (Maybe Transition)
getCollision dict = case lookup "onCollision" dict of
  Nothing => pure Nothing
  Just (JObject xs) => case lookup "transition" (fromList xs) of
    Nothing => fail "onCollision must have transition field"
    Just x => case the (Checked Transition) (cast x) of
      Left e => fail e
      Right transition => pure $ Just transition
  _ => fail "onCollision must be JObject"

getTime : Dict String JSON -> Checked (Maybe (Double, Transition))
getTime dict = case lookup "onTime" dict of
  Nothing => pure Nothing
  Just (JObject xs) => let dict' = fromList xs in
    case lookup "transition" dict' of
      Nothing => fail "onTime must have transition field"
      Just x => case the (Checked Transition) (cast x) of
        Left e => fail e
        Right transition => with Checked do
          time <- getDouble "time" dict'
          pure $ Just (time, transition)
  _ => fail "onTime must be JObject"

getHandlers : Dict String JSON -> Checked Handlers
getHandlers dict = with Checked do
  onCollision <- getCollision dict -- getHandler "onCollision" Collision dict
  onTime <- getTime dict -- getHandler "onTime" Time dict
  pure $ MkHandlers onCollision onTime

public export
record AIDescriptor where
  constructor MkAIDescriptor
  initial : Handlers
  roam : Maybe Handlers
  chase : Maybe Handlers
%name AIDescriptor aidesc

getStateHandlers : (name : String) -> Dict String JSON -> Checked (Maybe Handlers)
getStateHandlers name dict = case lookup name dict of
  Nothing => pure Nothing
  Just (JObject xs) => let dict' = fromList xs in getHandlers dict' >>= pure . Just
  _ => fail $ "state " ++ name ++ " must be JObject"

ObjectCaster AIDescriptor where
  objectCast dict = with Checked do
    Just initial <- getStateHandlers "initial" dict
    roam <- getStateHandlers "roam" dict
    chase <- getStateHandlers "chase" dict
    pure $ MkAIDescriptor initial roam chase

public export
GameIO m => SimpleLoader m AIDescriptor where
  load id = checkedJSONLoad (refToFilepath id)

public export
data AIDirection = Leftward | Rightward

export
Show AIDirection where
  show Leftward = "left"
  show Rightward = "right"

public export
record AIData where
  constructor MkAIData
  direction : Maybe AIDirection -- ms since last transition

initialData : AIData
initialData = MkAIData Nothing

export
commandToDataUpdate : Command -> AIData -> AIData
commandToDataUpdate (Start (Movement Left)) = record { direction = Just Leftward }
commandToDataUpdate (Start (Movement Right)) = record { direction = Just Rightward }
commandToDataUpdate _ = id

public export
record AIController where
  constructor MkAIController
  descriptor : AIDescriptor
  aidata : AIData
  state : AIState
  transitioned : Int
%name AIController controller

export
fromDescriptor : AIDescriptor -> AIController
fromDescriptor aidesc = MkAIController aidesc initialData Initial 0

export
updateData : (f : AIData -> AIData) -> AIController -> AIController
updateData f = record { aidata $= f }

export
initial : AIController -> Handlers
initial = initial . descriptor

export
roam : AIController -> Maybe Handlers
roam = roam . descriptor

export
chase : AIController -> Maybe Handlers
chase = chase . descriptor

export
direction : AIController -> Maybe AIDirection
direction = direction . aidata

export
transition : (time : Int) -> (state : AIState) -> AIController -> AIController
transition time state = record { state = state, transitioned = time }
