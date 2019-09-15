module Server.Rules.Behavior

import Data.AVL.Dict

import Descriptions.ObjectDescription.RulesDescription.BehaviorDescription
import Descriptions.ObjectDescription.RulesDescription
import Objects

public export
data BehaviorDirection = Leftward | Rightward

export
Show BehaviorDirection where
  show Leftward = "leftward"
  show Rightward = "rightward"

public export
record BehaviorData where
  constructor MkBehaviorData
  direction : Maybe BehaviorDirection
  lastHit : Maybe ObjectId
  chasing : Maybe ObjectId
  lastCollision : Maybe ObjectId
%name BehaviorData behavior_data

export
initialData : BehaviorData
initialData = MkBehaviorData Nothing Nothing Nothing Nothing

export
setLastHit : ObjectId -> BehaviorData -> BehaviorData
setLastHit id = record { lastHit = Just id }

export
setLastCollision : ObjectId -> BehaviorData -> BehaviorData
setLastCollision id = record { lastCollision = Just id }

export
beginChase : ObjectId -> BehaviorData -> BehaviorData
beginChase id = record { chasing = Just id }

export
endChase : BehaviorData -> BehaviorData
endChase = record { chasing = Nothing }

public export
record BehaviorController where
  constructor MkBehaviorController
  description : BehaviorDescription
  behavior_params : BehaviorParameters
  behavior_data : BehaviorData
  state : BehaviorState
  transitioned : Int
%name BehaviorController controller

export
getDoubleParameterOrDefault : (default : Double) ->
                              (name : Maybe String) ->
                              BehaviorController ->
                              Double
getDoubleParameterOrDefault default Nothing _ = default
getDoubleParameterOrDefault default (Just name) controller
  = case lookup name $ doubleParameters $ behavior_params controller of
         Nothing => default
         Just x => x

export
getStringParameter : (name : String) -> BehaviorController -> Maybe String
getStringParameter name controller
  = lookup name $ stringParameters $ behavior_params controller

export
fromDescriptorParameters : BehaviorDescription ->
                           BehaviorParameters ->
                           BehaviorController
fromDescriptorParameters behavior_desc behavior_params
  = MkBehaviorController behavior_desc behavior_params initialData (start behavior_desc) 0

export
updateData : (f : BehaviorData -> BehaviorData) -> BehaviorController -> BehaviorController
updateData f = record { behavior_data $= f }

export
queryData : (q : BehaviorData -> a) -> BehaviorController -> a
queryData q = q . behavior_data

export
direction : BehaviorController -> Maybe BehaviorDirection
direction = queryData direction

export
halted : BehaviorController -> Bool
halted controller = case end (description controller) of
  Nothing => False
  Just end_state => state controller == end_state

export
transition : (time : Int) ->
             (state : BehaviorState) ->
             BehaviorController -> BehaviorController
transition time state = record { state = state, transitioned = time }


export
currentHandlers : BehaviorController -> Maybe Handlers
currentHandlers controller = lookup (state controller) (states $ description controller)

export
collisionHandler : BehaviorController -> Maybe Transition
collisionHandler = join . map onCollision . currentHandlers

export
timeHandler : BehaviorController -> Maybe (Double, Maybe String, Transition)
timeHandler = join . map onTime . currentHandlers

export
hitHandler : BehaviorController -> Maybe Transition
hitHandler = join . map onHit . currentHandlers

export
interactHandler : BehaviorController -> Maybe (String, Transition)
interactHandler = join . map onInteract . currentHandlers

export
lastHit : BehaviorController -> Maybe ObjectId
lastHit = lastHit . behavior_data
