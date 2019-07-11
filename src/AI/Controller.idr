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
AIState : Type
AIState = String

-- TODO expand
public export
data AIAction = MoveRight | MoveLeft | ChangeDirection | Attack | Stop
              | BeginChase | EndChase | BeginWalk | EndWalk

export
Show AIAction where
  show MoveRight = "move right"
  show MoveLeft = "move left"
  show ChangeDirection = "change direction"
  show Attack = "attack"
  show Stop = "stop"
  show BeginChase = "begin chase"
  show EndChase = "end chase"
  show BeginWalk = "begin walk"
  show EndWalk = "end walk"

Cast String (Checked AIAction) where
  cast "move right" = pure MoveRight
  cast "move left" = pure MoveLeft
  cast "change direction" = pure ChangeDirection
  cast "attack" = pure Attack
  cast "stop" = pure Stop
  cast "begin chase" = pure BeginChase
  cast "end chase" = pure EndChase
  cast "begin walk" = pure BeginWalk
  cast "end walk" = pure EndWalk
  cast _ = fail "action must be of \"attack\"|..."

-- TODO add Int parameters to handlers

public export
record Transition where
  constructor MkTransition
  state : AIState
  action : List AIAction

getActions : Dict String JSON -> Checked (List AIAction)
getActions dict = case hasKey "actions" dict of
  False => case hasKey "action" dict of
    False => pure empty
    True => with Checked do
      action_string <- getString "action" dict
      action <- cast action_string
      pure [action]
  True => with Checked do
    action_strings <- getStrings "actions" dict
    let actions = the (List (Checked AIAction)) $ map cast action_strings
    listCheckedtoCheckedList actions

ObjectCaster Transition where
  objectCast dict = with Checked do
    state <- getString "state" dict
    actions <- getActions dict
    pure $ MkTransition state actions

-- additional parameters only affect handlers, i.e. whether they fire,
-- which arguments they give, etc.
public export
record Handlers where
  constructor MkHandlers
  onCollision : Maybe Transition
  onTime : Maybe (Double, Maybe String, Transition) -- wait time
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

getTime : Dict String JSON -> Checked (Maybe (Double, Maybe String, Transition))
getTime dict = case lookup "onTime" dict of
  Nothing => pure Nothing
  Just (JObject xs) => let dict' = fromList xs in
    case lookup "transition" dict' of
      Nothing => fail "onTime must have transition field"
      Just x => case the (Checked Transition) (cast x) of
        Left e => fail e
        Right transition => with Checked do
          time <- getDouble "time" dict'
          let time_parameter = eitherToMaybe $ getString "time_parameter" dict'
          pure $ Just (time, time_parameter, transition)
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
  start : AIState
  states : Dict String Handlers
%name AIDescriptor aidesc

getStateHandlers : (name : String) -> Dict String JSON -> Checked Handlers
getStateHandlers name dict = case lookup name dict of
  Nothing => fail $ "handlers for state " ++ name ++ " missing"
  Just (JObject xs) => let dict' = fromList xs in getHandlers dict' >>= pure
  _ => fail $ "state " ++ name ++ " must be JObject"

ObjectCaster AIDescriptor where
  objectCast dict = with Checked do
    states <- getStrings "states" dict
    start <- getString "start" dict
    case start `elem` states of
      False => fail $ "start state (" ++ start ++ ") not in state list (" ++ show states ++ ")"
      True => with Checked do
        let handlers = map (flip getStateHandlers dict) states
        handlers' <- listCheckedtoCheckedList handlers
        pure $ MkAIDescriptor start (fromList (zip states handlers'))

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
  direction : Maybe AIDirection
  lastHit : Maybe ObjectId
  chasing : Maybe ObjectId
%name AIData aidata

export
setLastHit : ObjectId -> AIData -> AIData
setLastHit id = record { lastHit = Just id }

export
beginChase : ObjectId -> AIData -> AIData
beginChase id = record { chasing = Just id }

export
endChase : AIData -> AIData
endChase = record { chasing = Nothing }

initialData : AIData
initialData = MkAIData Nothing Nothing Nothing

export
commandToDataUpdate : Command -> AIData -> AIData
commandToDataUpdate (Start (Movement Left)) = record { direction = Just Leftward }
commandToDataUpdate (Start (Movement Right)) = record { direction = Just Rightward }
commandToDataUpdate _ = id

public export
record AIController where
  constructor MkAIController
  descriptor : AIDescriptor
  aiparams : Maybe AIParameters
  aidata : AIData
  state : AIState
  transitioned : Int
%name AIController controller

export
getDoubleParameterOrDefault : (default : Double) -> (name : Maybe String) -> AIController -> Double
getDoubleParameterOrDefault default Nothing _ = default
getDoubleParameterOrDefault default (Just name) controller = case aiparams controller of
  Nothing => default
  Just ai_parameters => case lookup name (doubleParameters ai_parameters) of
    Nothing => default
    Just x => x

export
fromDescriptorParameters : AIDescriptor -> Maybe AIParameters -> AIController
fromDescriptorParameters aidesc aiparams
  = MkAIController aidesc aiparams initialData (start aidesc) 0

export
updateData : (f : AIData -> AIData) -> AIController -> AIController
updateData f = record { aidata $= f }

export
direction : AIController -> Maybe AIDirection
direction = direction . aidata

export
transition : (time : Int) -> (state : AIState) -> AIController -> AIController
transition time state = record { state = state, transitioned = time }

export
currentHandlers : AIController -> Maybe Handlers
currentHandlers controller = lookup (state controller) (states $ descriptor controller)

export
collisionHandler : AIController -> Maybe Transition
collisionHandler = join . map onCollision . currentHandlers

export
timeHandler : AIController -> Maybe (Double, Maybe String, Transition)
timeHandler = join . map onTime . currentHandlers

export
hitHandler : AIController -> Maybe Transition
hitHandler = join . map onHit . currentHandlers

export
lastHit : AIController -> Maybe ObjectId
lastHit = lastHit . aidata
