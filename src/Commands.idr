module Commands

import public Physics.Vector2D

import Objects
import GameIO

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
            | Interact Double

export
Show Action where
  show (Movement x) = "move " ++ show x
  show (Attack x) = "attack " ++ show x
  show Walk = "walk"
  show (Interact x) = "interact " ++ show x

public export
data Command = Start Action ObjectId
             | Stop Action ObjectId
             | Equip ContentReference ObjectId
             | Unequip ContentReference ObjectId

export
getId : Command -> ObjectId
getId (Start x y) = y
getId (Stop x y) = y
getId (Equip ref id) = id
getId (Unequip ref id) = id

export
Show Command where
  show (Start x id) = id ++ " start " ++ show x
  show (Stop x id) = id ++ " stop " ++ show x
  show (Equip ref id) = id ++ " equip " ++ ref
