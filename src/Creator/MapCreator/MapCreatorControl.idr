module Creator.MapCreator.MapCreatorControl

import Physics.Vector2D

import JSONCache
import GameIO
import Commands

public export
record MapCreatorControl where
  constructor MkMapCreatorControl
  movingLeft : Bool
  movingRight : Bool
  movingUp : Bool
  movingDown : Bool

export
initialControl : MapCreatorControl
initialControl = MkMapCreatorControl False False False False

export
moveSelectors : MapCreatorControl -> List (Bool, Vector2D)
moveSelectors control =  [
  (movingLeft control, (-1, 0)),
  (movingRight control, (1, 0)),
  (movingUp control, (0, 1)),
  (movingDown control, (0, -1))
]

export
getMove : MapCreatorControl -> Vector2D
getMove control = let summed = sum $ map snd $ filter fst $ moveSelectors control
                      in if norm summed == 0 then nullVector else normed summed

export
startMoving : Direction -> MapCreatorControl -> MapCreatorControl
startMoving Left = record { movingLeft = True }
startMoving Right = record { movingRight = True }
startMoving Up = record { movingUp = True }
startMoving Down = record { movingDown = True }

export
stopMoving : Direction -> MapCreatorControl -> MapCreatorControl
stopMoving Left = record { movingLeft = False }
stopMoving Right = record { movingRight = False }
stopMoving Up = record { movingUp = False }
stopMoving Down = record { movingDown = False }
