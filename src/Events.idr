module Events

import Control.ST

import Common
import Physics.Vector2D
import Settings
import Camera

public export
data Direction = Left | Right | Up | Down

public export
data Action = Movement Direction
            | Attack Vector2D -- x, y of screen

public export
data Command = Start Action
             | Stop Action

public export
data Event = CollisionStart CollisionForObject CollisionForObject
           | CollisionStop CollisionForObject CollisionForObject
%name Events.Event event

export
Show Event where
  show (CollisionStart for_one for_two) = "CollisionStart " ++ (id for_one) ++ " " ++ (id for_two)
  show (CollisionStop for_one for_two) = "CollisionStop " ++ (id for_one) ++ " " ++ (id for_two)
