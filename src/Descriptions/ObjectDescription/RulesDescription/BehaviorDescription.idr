module Descriptions.ObjectDescription.RulesDescription.BehaviorDescription

import GameIO
import Exception

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
  | Door

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

export
Cast String (Checked BehaviorAction) where
  cast "move left" = pure MoveLeft
  cast "move right" = pure MoveRight
  cast "stop" = pure Stop
  cast "change direction" = pure ChangeDirection
  cast "projectile damage" = pure ProjectileDamage
  cast "attack" = pure Attack
  cast "begin chase" = pure BeginChase
  cast "end chase" = pure EndChase
  cast "begin walk" = pure BeginWalk
  cast "end walk" = pure EndWalk
  cast "door" = pure Door
  cast _ = fail "wrong behavior action"

public export
record Transition where
  constructor MkTransition
  state : BehaviorState
  actions : List BehaviorAction

getActions : JSONDict -> Checked (List BehaviorAction)
getActions dict = case hasKey "actions" dict of
  False => case hasKey "action" dict of
    False => pure empty
    True => with Checked do
      action_string <- getString "action" dict
      action <- cast action_string
      pure [action]
  True => with Checked do
    action_strings <- getStrings "actions" dict
    let actions = the (List (Checked BehaviorAction)) $ map cast action_strings
    catResults actions

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
  Just (JObject xs) => case lookup "transition" (fromList xs) of
    Nothing => fail $ name ++ " must have transition field"
    Just x => case the (Checked Transition) (cast x) of
      Left e => fail e
      Right transition => pure $ Just transition
  _ => fail $ name ++ " must be JObject"

getTime : JSONDict -> Checked (Maybe (Double, Maybe String, Transition))
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
