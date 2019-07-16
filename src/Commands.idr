module Commands

import Physics.Vector2D

import Objects

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
            | Walk

export
Show Action where
  show (Movement x) = "move " ++ show x
  show (Attack x) = "attack " ++ show x
  show Walk = "walk"

public export
data Command = Start Action ObjectId
             | Stop Action ObjectId

export
Show Command where
  show (Start x id) = id ++ " start " ++ show x
  show (Stop x id) = id ++ " stop " ++ show x
