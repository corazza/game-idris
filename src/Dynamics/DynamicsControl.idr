module Dynamics.DynamicsControl

import Descriptions.ObjectDescription.ControlDescription
import Commands
import Dynamics.MoveDirection
import Objects

public export
record ControlParameters where
  constructor MkControlParameters
  speed : Double
  jump : Double

export
parametersFromDescription : ControlDescription -> ControlParameters
parametersFromDescription desc = MkControlParameters (speed desc) (jump desc)

export
Show ControlParameters where
  show (MkControlParameters speed jump)
    = "{ speed: " ++ show speed ++ ", jump: " ++ show jump ++ " }"

public export
record ControlState where
  constructor MkControlState
  moving : List MoveDirection
  facing : MoveDirection
  jumping : Bool
  canJump : Bool
  walking : Bool
  attacking : Bool
%name ControlState controlState

export
Show ControlState where
  show (MkControlState moving facing jumping canJump walking attacking)
    = "(moving: " ++ show moving ++ ", " ++
      "facing: " ++ show facing ++ ", " ++
      "jumping: " ++ show jumping ++ ", " ++
      "walking: " ++ show walking ++ ", " ++
      "attacking: " ++ show attacking

export
initialControlStateFacing : (facing : MoveDirection) -> ControlState
initialControlStateFacing facing
  = MkControlState empty facing False False False False

export
startWalking : ControlState -> ControlState
startWalking = record { walking = True }

export
stopWalking : ControlState -> ControlState
stopWalking = record { walking = False }

moveToTop : Eq a => a -> List a -> List a
moveToTop x xs = x :: filter (/= x) xs

export
startMoving : (direction : MoveDirection) -> ControlState -> ControlState
startMoving direction = record { moving $= moveToTop direction, facing = direction }

export
stopMoving : (direction : MoveDirection) -> ControlState -> ControlState
stopMoving direction ctst
  = let newCtst = record { moving $= filter (/= direction) } ctst
        in case moving newCtst of
          [] => newCtst
          x :: _ => record {facing=x} newCtst

export
face : (direction : MoveDirection) -> ControlState -> ControlState
face direction = record { facing = direction }

export
moveSign : ControlState -> Double
moveSign controlState = case moving controlState of
  [] => 0
  Leftward :: _ => -1
  Rightward :: _ => 1

export
facingFromMove : ControlState -> Maybe MoveDirection
facingFromMove = head' . moving

export
startJumping : ControlState -> ControlState
startJumping = record { jumping = True }

export
stopJumping : ControlState -> ControlState
stopJumping = record { jumping = False, canJump = True }

export
startMoveAction : Direction -> ControlState -> ControlState
startMoveAction Left = startMoving Leftward
startMoveAction Right = startMoving Rightward
startMoveAction Up = startJumping
startMoveAction Down = id

export
stopMoveAction : Direction -> ControlState -> ControlState
stopMoveAction Left = stopMoving Leftward
stopMoveAction Right = stopMoving Rightward
stopMoveAction Up = stopJumping
stopMoveAction Down = id

export
faceAction : Direction -> ControlState -> ControlState
faceAction Left = face Leftward
faceAction Right = face Rightward
faceAction Up = id
faceAction Down = id

export
startAttacking : ControlState -> ControlState
startAttacking = record { attacking = True }

export
stopAttacking : ControlState -> ControlState
stopAttacking = record { attacking = False }

export
resetControlState : ControlState -> ControlState
resetControlState ctst = record { canJump = not (jumping ctst) } ctst

public export
record ObjectControl where
  constructor MkObjectControl
  controlState : ControlState
  controlParameters : ControlParameters
%name ObjectControl control

export
speed : ObjectControl -> Double
speed (MkObjectControl controlState controlParameters)
  = let speed = speed controlParameters
      in if walking controlState then 0.5 * speed else speed

export
airspeed : ObjectControl -> Double
airspeed = (*) 0.45 . speed

export
initialControl : ControlParameters -> ObjectControl
initialControl = MkObjectControl (initialControlStateFacing Rightward)

Show ObjectControl where
  show (MkObjectControl ctst ctp)
    =  "{ control | "
    ++   "controlState: " ++ show ctst
    ++ ", controlParameters: " ++ show ctp
    ++ "}"

resetObjectControl : ObjectControl -> ObjectControl
resetObjectControl = record { controlState $= resetControlState }

export -- TODO UGLY FIX
promoteToObjectControl : (f : ControlState -> ControlState) -> ObjectControl -> ObjectControl
promoteToObjectControl f = record { controlState $= f }
