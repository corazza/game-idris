module Dynamics.BodyData

import Physics.Box2D
import Physics.Vector2D
import Data.AVL.Set

import Data.AVL.SetRemove
import Objects
import Dynamics.DynamicsControl
import Dynamics.MoveDirection
import Descriptions.ObjectDescription.BodyDescription

public export
record BodyData where
  constructor MkBodyData
  position : Vector2D -- set on first iterate
  angle : Double
  velocity : Vector2D
  mass : Double
  box2d_id : Int
  body : Body
  controls : Maybe ObjectControl
  touching : Set ObjectId
  grounding : List (ObjectId, Double)
  effects : List PhysicsEffect
%name BodyData body_data

export
Show BodyData where
  show bodyData
    =  "{ position: " ++ show (position bodyData)
    ++ ", angle: " ++ show (angle bodyData)
    ++ " }"

export
controlState : BodyData -> Maybe ControlState
controlState = map controlState . controls

export
facing : BodyData -> Maybe MoveDirection
facing = map facing . controlState

export
forceDirection : BodyData -> MoveDirection
forceDirection body_data = case facing body_data of
  Nothing => Rightward
  Just x => x

-- TODO dynamics iterate should receive clock and update a BodyData field
-- movementLastChanged, which is read on render animation

export
animationState : BodyData -> String
animationState object = case controlState object of
  Nothing => "resting"
  Just ctst => case moving ctst of
    [] => "resting"
    _ => if walking ctst then "walking" else "moving"

export
addTouching : (id : ObjectId) -> BodyData -> BodyData
addTouching id = record { touching $= insert id }

export
removeTouching : (id : ObjectId) -> BodyData -> BodyData
removeTouching id = record { touching $= remove id }

export
addGrounding : ObjectId -> Double -> BodyData -> BodyData
addGrounding id angle = record { grounding $= (::) (id, angle) }

export
setGrounding : List (ObjectId, Double) -> BodyData -> BodyData
setGrounding xs = record { grounding = xs }

export
removeGrounding : BodyData -> BodyData
removeGrounding = record { grounding $= popTop } where
  popTop : List a -> List a
  popTop [] = []
  popTop (x::xs) = xs

export
updateControlInBody : (f : ControlState -> ControlState) -> BodyData -> BodyData
updateControlInBody f = record { controls $= map (promoteToObjectControl f) }

export
onGround : BodyData -> Bool
onGround = not . isNil . grounding
