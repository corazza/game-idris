module Dynamics.MoveDirection

public export
data MoveDirection = Leftward | Rightward
%name MoveDirection direction

export
Eq MoveDirection where
  Leftward == Leftward = True
  Rightward == Rightward = True
  _ == _ = False

export
Show MoveDirection where
  show Leftward = "left"
  show Rightward = "right"
