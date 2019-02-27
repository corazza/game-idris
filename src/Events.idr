module Events

import Objects

%access public export

data MoveDirection = MoveLeft | MoveRight

%name MoveDirection direction

data Event = MovementStart MoveDirection String
           | MovementStop String
           | Attack String

%name Events.Event event
