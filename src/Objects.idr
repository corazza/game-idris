module Objects

import Graphics.SDL2
import Data.AVL.Dict

import Physics.Vector2D

%access public export


data BoxType = Static | Dynamic

record BoxDescription where
  constructor MkBoxDescription
  mass : Double
  type : BoxType
  dim : Vector2D

data MoveDirection = Leftward | Rightward
%name MoveDirection direction

Show MoveDirection where
  show Leftward = "left"
  show Rightward = "right"

record ControlState where
  constructor MkControlState
  moving : Maybe MoveDirection
  jumping : Bool
  attacking : Bool
%name ControlState controlState

Show ControlState where
  show (MkControlState moving jumping attacking)
    = "(moving: " ++ (show moving) ++ "," ++
      " jumping: " ++ (show jumping) ++ "," ++
      " attacking: " ++ (show attacking) ++ ")"

noControl : ControlState
noControl = MkControlState Nothing False False

startMoving : (direction : MoveDirection) -> ControlState -> ControlState
startMoving direction = record { moving = Just direction }

stopMoving : ControlState -> ControlState
stopMoving = record { moving = Nothing }

startJumping : ControlState -> ControlState
startJumping = record { jumping = True }

stopJumping : ControlState -> ControlState
stopJumping = record { jumping = False }

startAttacking : ControlState -> ControlState
startAttacking = record { attacking = True }

stopAttacking : ControlState -> ControlState
stopAttacking = record { attacking = False }

record Object where
  constructor MkObject
  id : String
  position : Vector2D
  angle : Double
  boxDescription : BoxDescription
  texture : Texture
  controlState : ControlState

%name Object object

export
dim : Object -> Vector2D
dim = dim . boxDescription

export
w : Object -> Double
w = fst . dim

export
h : Object -> Double
h = snd . dim

export
x : Object -> Double
x = fst . position

export
y : Object -> Double
y = snd . position
