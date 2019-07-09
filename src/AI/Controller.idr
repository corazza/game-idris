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
AIState = Initial | Roam | Chase | Hold

export
Show AIState where
  show Initial = "initial"
  show Roam = "roam"
  show Chase = "chase"
  show Hold = "hold"

Cast String (Checked AIState) where
  cast "initial" = pure Initial
  cast "roam" = pure Roam
  cast "chase" = pure Chase
  cast "hold" = pure Hold
  cast _ = fail "AIState must be of \"roam\"|\"chase\""

-- TODO expand
public export
data AIAction = MoveRight | MoveLeft | ChangeDirection | Attack | Stop

export
Show AIAction where
  show MoveRight = "move right"
  show MoveLeft = "move left"
  show ChangeDirection = "change direction"
  show Attack = "attack"
  show Stop = "stop"

Cast String (Checked AIAction) where
  cast "move right" = pure MoveRight
  cast "move left" = pure MoveLeft
  cast "change direction" = pure ChangeDirection
  cast "attack" = pure Attack
  cast "stop" = pure Stop
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
  onHit : Maybe Transition

getHandler : (name : String) -> (dict : Dict String JSON) -> Checked (Maybe Transition)
getHandler name dict = case lookup name dict of
  Nothing => pure Nothing
  Just (JObject xs) => case lookup "transition" (fromList xs) of
    Nothing => fail $ name ++ " must have transition field"
    Just x => case the (Checked Transition) (cast x) of
      Left e => fail e
      Right transition => pure $ Just transition
  _ => fail $ name ++ " must be JObject"

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
  onTime <- getTime dict
  onCollision <- getHandler "onCollision" dict
  onHit <- getHandler "onHit" dict
  pure $ MkHandlers onCollision onTime onHit

public export
record AIDescriptor where
  constructor MkAIDescriptor
  initial : Handlers
  roam : Maybe Handlers
  chase : Maybe Handlers
  hold : Maybe Handlers
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
    hold <- getStateHandlers "hold" dict
    pure $ MkAIDescriptor initial roam chase hold

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
  target : Maybe ObjectId

export
setTarget : ObjectId -> AIData -> AIData
setTarget id = record { target = Just id }

initialData : AIData
initialData = MkAIData Nothing Nothing

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
hold : AIController -> Maybe Handlers
hold = hold . descriptor

export
direction : AIController -> Maybe AIDirection
direction = direction . aidata

export
transition : (time : Int) -> (state : AIState) -> AIController -> AIController
transition time state = record { state = state, transitioned = time }

export
currentHandlers : AIController -> Maybe Handlers
currentHandlers controller = case state controller of
  Initial => Just (initial controller)
  Roam => roam controller
  Chase => chase controller
  Hold => hold controller

export
collisionHandler : AIController -> Maybe Transition
collisionHandler = join . map onCollision . currentHandlers

export
timeHandler : AIController -> Maybe (Double, Transition)
timeHandler = join . map onTime . currentHandlers

export
hitHandler : AIController -> Maybe Transition
hitHandler = join . map onHit . currentHandlers

export
target : AIController -> Maybe ObjectId
target = target . aidata
