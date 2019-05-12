module Common

import Physics.Vector2D

public export
ObjectId : Type
ObjectId = String

%name ObjectId id

public export
AttackParameters : Type
AttackParameters = Vector2D

public export
screenScale : Double
screenScale = 33

public export
resolution : (Int, Int)
resolution = (1280, 800)

public export
Cast (Int, Int) (Double, Double) where
  cast (x, y) = (cast x, cast y)

public export
resolution' : (Double, Double)
resolution' = cast resolution

public export
positionToScreen : (camera : Vector2D) -> (position : Vector2D) -> (Int, Int)
positionToScreen (cx, cy) (ox, oy)
  = let (x, y) = screenScale `scale` (ox - cx, cy - oy) in
        cast (x + (fst resolution')/2, y + (snd resolution')/2)

public export
screenToPosition : (camera : Vector2D) -> (screen : (Int, Int)) -> Vector2D
screenToPosition camera (sx, sy)
  = let screenVector = (cast sx - (fst resolution')/2, cast sy - (snd resolution')/2)
        s = (1.0 / screenScale) `scale` screenVector
        in camera + s

public export
dimToScreen : (dim : Vector2D) -> (Int, Int)
dimToScreen (x, y) = cast $ (screenScale * x, screenScale * y)
