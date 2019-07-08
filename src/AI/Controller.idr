module AI.Controller

import Data.AVL.Dict

import Descriptors
import GameIO
import Exception
import Common
import Resources

public export
data AIState = Leftright | Chase | Attack

initialState : AIState
initialState = Leftright

Cast String (Checked AIState) where
  cast "leftright" = pure Leftright
  cast "chase" = pure Chase
  cast "attack" = pure Attack
  cast _ = fail "AIState must be of \"leftright\"|\"chase\"|\"attack\""

-- additional parameters only affect handlers, i.e. whether they fire,
-- which arguments they give, etc.
public export
record Handlers where
  constructor MkHandlers
  onCollision : Maybe AIState
  onTime : Maybe (Double, AIState) -- wait time

-- TODO abstract and unify
getCollision : Dict String JSON -> Checked (Maybe AIState)
getCollision dict = case lookup "onCollision" dict of
  Nothing => pure Nothing
  Just (JObject xs) => let dict' = fromList xs in with Checked do
    statename <- getString "transition" dict'
    case the (Checked AIState) (cast statename) of
      Left e => fail e
      Right state => pure $ Just state
  _ => fail "onCollision must be JObject"

getTime : Dict String JSON -> Checked (Maybe (Double, AIState))
getTime dict = case lookup "onTime" dict of
  Nothing => pure Nothing
  Just (JObject xs) => let dict' = fromList xs in with Checked do
    statename <- getString "transition" dict'
    case the (Checked AIState) (cast statename) of
      Left e => fail e
      Right state => with Checked do
        time <- getDouble "time" dict'
        pure $ Just (time, state)
  _ => fail "onTime must be JObject"

getHandlers : Dict String JSON -> Checked Handlers
getHandlers dict = with Checked do
  onCollision <- getCollision dict -- getHandler "onCollision" Collision dict
  onTime <- getTime dict -- getHandler "onTime" Time dict
  pure $ MkHandlers onCollision onTime

public export
record AIDescriptor where
  constructor MkAIDescriptor
  leftright : Maybe (Double, Handlers)
  chase : Maybe Handlers
  attack : Maybe Handlers
%name AIDescriptor aidesc

getLeftright : Dict String JSON -> Checked (Maybe (Double, Handlers))
getLeftright dict = case lookup "leftright" dict of
  Nothing => pure Nothing
  Just (JObject xs) => let dict' = fromList xs in with Checked do
    duration <- getDouble "duration" dict'
    handlers <- getHandlers dict'
    pure $ Just (duration, handlers)
  _ => fail "state leftright must be JObject"

getStateHandlers : (name : String) -> Dict String JSON -> Checked (Maybe Handlers)
getStateHandlers name dict = case lookup name dict of
  Nothing => pure Nothing
  Just (JObject xs) => let dict' = fromList xs in getHandlers dict' >>= pure . Just
  _ => fail $ "state " ++ name ++ " must be JObject"

ObjectCaster AIDescriptor where
  objectCast dict = with Checked do
    leftright <- getLeftright dict
    chase <- getStateHandlers "chase" dict
    attack <- getStateHandlers "attack" dict
    pure $ MkAIDescriptor leftright chase attack

public export
GameIO m => SimpleLoader m AIDescriptor where
  load id = checkedJSONLoad (refToFilepath id) -- $ "res/main/maps/" ++ id ++ ".json"

public export
record AIData where
  constructor MkAIData
  lastTime : Int

initialData : AIData
initialData = MkAIData 0

public export
record AIController where
  constructor MkAIController
  descriptor : AIDescriptor
  aidata : AIData
  state : AIState
%name AIController controller

export
fromDescriptor : AIDescriptor -> AIController
fromDescriptor aidesc = MkAIController aidesc initialData initialState

export
updateData : (f : AIData -> AIData) -> AIController -> AIController
updateData f = record { aidata $= f }

-- HELPER
export
leftright : AIController -> Maybe (Double, Handlers)
leftright = leftright . descriptor

export
chase : AIController -> Maybe Handlers
chase = chase . descriptor

export
attack : AIController -> Maybe Handlers
attack = attack . descriptor
