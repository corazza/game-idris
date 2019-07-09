module Events

import Control.ST

import Common
import Physics.Vector2D
import Settings
import Camera

public export
data Direction = Left | Right | Up | Down

export
Show Direction where
  show Left = "left"
  show Right = "right"
  show Up = "up"
  show Down = "down"

public export
data Action = Movement Direction
            | Attack Vector2D -- x, y of screen

export
Show Action where
  show (Movement x) = "move " ++ show x
  show (Attack x) = "attack " ++ show x

public export
data Command = Start Action
             | Stop Action

export
Show Command where
  show (Start x) = "start " ++ show x
  show (Stop x) = "stop " ++ show x

public export
data Event = CollisionStart CollisionForObject CollisionForObject
           | CollisionStop CollisionForObject CollisionForObject
           | Hit ObjectId ObjectId Double
%name Events.Event event

export
Show Event where
  show (CollisionStart for_one for_two) = "CollisionStart " ++ (id for_one) ++ " " ++ (id for_two)
  show (CollisionStop for_one for_two) = "CollisionStop " ++ (id for_one) ++ " " ++ (id for_two)
  show (Hit attacker target damage) = attacker ++ " hit " ++ target ++ " for " ++ show damage
