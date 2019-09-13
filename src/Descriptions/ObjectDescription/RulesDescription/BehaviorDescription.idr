module Descriptions.ObjectDescription.RulesDescription.BehaviorDescription

import GameIO
import Exception
import Descriptions.BitsDescription

public export
BehaviorState : Type
BehaviorState = String

public export
data BehaviorAction
  = MoveLeft | MoveRight | Stop | ChangeDirection
  | ProjectileDamage
  | Attack
  | BeginChase | EndChase
  | BeginWalk | EndWalk
  | Door | Loot
  | SetMaskBits (List String)
  | UnsetMaskBits (List String)

export
Show BehaviorAction where
  show MoveLeft = "move left"
  show MoveRight = "move right"
  show Stop = "stop"
  show ChangeDirection = "change direction"
  show ProjectileDamage = "projectile damage"
  show Attack = "attack"
  show BeginChase = "begin chase"
  show EndChase = "end chase"
  show BeginWalk = "begin wald"
  show EndWalk = "end walk"
  show Door = "door"
  show Loot = "loot"
  show (SetMaskBits xs) = "set mask bit " ++ show xs
  show (UnsetMaskBits xs) = "unset mask bit " ++ show xs

actionPicker : Dict String BehaviorAction
actionPicker = fromList [
  ("move left", MoveLeft),
  ("move right", MoveRight),
  ("stop", Stop),
  ("change direction", ChangeDirection),
  ("projectile damage", ProjectileDamage),
  ("attack", Attack),
  ("begin chase", BeginChase),
  ("end chase", EndChase),
  ("begin walk", BeginWalk),
  ("end walk", EndWalk),
  ("door", Door),
  ("loot", Loot)
]

export
Cast String (Checked BehaviorAction) where
  cast actionString = pick "action" actionString actionPicker

ObjectCaster BehaviorAction where
  objectCast dict = with Checked do
    type <- getString "type" dict
    case the (Checked BehaviorAction) $ cast type of
      Left e => case type of
        "set mask bit" => getString "maskBit" dict >>= pure . SetMaskBits . (::[])
        "unset mask bit" => getString "maskBit" dict >>= pure . UnsetMaskBits . (::[])
        "set mask bits" => getStrings "maskBits" dict >>= pure . SetMaskBits
        "unset mask bits" => getStrings "maskBits" dict >>= pure . UnsetMaskBits
      Right r => pure r

getActions : JSONDict -> Checked (List BehaviorAction)
getActions dict = case hasKey "actions" dict of
  False => case hasKey "action" dict of
    False => pure empty
    True => getCastable "action" dict >>= pure . (::[])
  True => getArray "actions" dict >>=
    catResults . map (the (Checked BehaviorAction) . cast)

public export
record Transition where
  constructor MkTransition
  state : BehaviorState
  actions : List BehaviorAction

ObjectCaster Transition where
  objectCast dict = with Checked do
    state <- getString "state" dict
    actions <- getActions dict
    pure $ MkTransition state actions

public export
record Handlers where
  constructor MkHandlers
  onCollision : Maybe Transition
  onTime : Maybe (Double, Maybe String, Transition) -- wait time
  onHit : Maybe Transition
  onInteract : Maybe (String, Transition)

getTransition : JSONDict -> Checked Transition
getTransition dict = the (Checked Transition) $ getCastable "transition" dict

getHandler : (name : String) -> (dict : JSONDict) -> Checked (Maybe Transition)
getHandler name dict = case lookup name dict of
  Nothing => pure Nothing
  Just (JObject xs) => case Dict.lookup "transition" (fromList xs) of
    Nothing => fail $ name ++ " must have transition field"
    Just x => case the (Checked Transition) (cast x) of
      Left e => fail e
      Right transition => pure $ Just transition
  _ => fail $ name ++ " must be JObject"

getTime : JSONDict -> Checked (Maybe (Double, Maybe String, Transition))
getTime dict = case lookup "onTime" dict of
  Nothing => pure Nothing
  Just (JObject xs) => let dict' = Dict.fromList xs in
    case lookup "transition" dict' of
      Nothing => fail "onTime must have transition field"
      Just x => case the (Checked Transition) (cast x) of
        Left e => fail e
        Right transition => with Checked do
          time <- getDouble "time" dict'
          let time_parameter = eitherToMaybe $ getString "time_parameter" dict'
          pure $ Just (time, time_parameter, transition)
  _ => fail "onTime must be JObject"

getInteract : JSONDict -> Checked (Maybe (String, Transition))
getInteract dict = case lookup "onInteract" dict of
  Nothing => pure Nothing
  Just (JObject xs) => let dict' = fromList xs in with Checked do
    transition <- getTransition dict'
    interact_string <- getString "interact_string" dict'
    pure $ Just (interact_string, transition)
  _ => fail "onInteract must be JOBject"

public export
record BehaviorDescription where
  constructor MkBehaviorDescription
  start : BehaviorState
  end : Maybe BehaviorState
  states : Dict String Handlers
%name BehaviorDescription behavior_desc

getHandlers : Dict String JSON -> Checked Handlers
getHandlers dict = with Checked do
  onTime <- getTime dict
  onCollision <- getHandler "onCollision" dict
  onHit <- getHandler "onHit" dict
  onInteract <- getInteract dict
  pure $ MkHandlers onCollision onTime onHit onInteract

getStateHandlers : (name : String) -> JSONDict -> Checked Handlers
getStateHandlers name dict = case lookup name dict of
  Nothing => fail $ "handlers for state " ++ name ++ " missing"
  Just (JObject xs) => let dict' = fromList xs in getHandlers dict' >>= pure
  _ => fail $ "state " ++ name ++ " must be JObject"

export
ObjectCaster BehaviorDescription where
  objectCast dict = with Checked do
    states <- getStrings "states" dict
    start <- getString "start" dict
    end <- getStringMaybe "end" dict
    case start `elem` states of
      False => fail $
        "start state (" ++ start ++ ") not in state list (" ++ show states ++ ")"
      True => with Checked do
        let handlers = map (flip getStateHandlers dict) states
        handlers' <- catResults handlers
        pure $ MkBehaviorDescription start end (fromList (zip states handlers'))
