module Events

import Objects

%access public export

data MoveDirection = MoveLeft | MoveRight

data Event = MovementStart MoveDirection Object
           | Attack
