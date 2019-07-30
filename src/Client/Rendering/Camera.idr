module Client.Rendering.Camera

import Graphics.SDL2 as SDL2
import public Physics.Vector2D

import Settings

round : Double -> Int
round x = if x < 0.0 then cast (x - 0.5) else cast (x + 0.5)

Cast (Int, Int) (Double, Double) where
  cast (x, y) = (cast x, cast y)

public export
record Camera where
  constructor MkCamera
  position : Vector2D
  zoom : Double
  rotation : Double
  resolution : (Int, Int)
  yd : Double
%name Camera camera

export -- TODO this should receive player position
fromSettings : CameraSettings -> Camera
fromSettings settings = let position = nullVector
                            zoom = zoom settings
                            rotation = 0
                            resolution = resolution settings
                            yd = yd settings
                            in MkCamera position zoom rotation resolution yd

export
toSettings : Camera -> CameraSettings
toSettings camera = MkCameraSettings (resolution camera) (yd camera) (zoom camera)

export
resolution' : Camera -> (Double, Double)
resolution' = cast . resolution

export
translate : Vector2D -> Camera -> Camera
translate a = record { position = a }

export
zoomFactor : Double -> Camera -> Camera
zoomFactor factor = record { zoom $= (*) factor }

export
dimToScreen : Camera -> Vector2D -> (Int, Int)
dimToScreen (MkCamera _ zoom _ _ _) (x, y) = (round $ zoom * x, round $ zoom * y)

export
positionToScreen : Camera -> Vector2D -> (Int, Int)
positionToScreen camera@(MkCamera (cx, cy) zoom _ resolution yd) (ox, oy)
  = let (rx, ry) = resolution' camera
        (x, y) = zoom `scale` (ox - cx, cy - oy) in
        (round $ x + rx/2, round $ y + ry/2)

export
screenToPosition : Camera -> (Int, Int) -> Vector2D
screenToPosition camera (sx, sy)
  = let (rx, ry) = resolution' camera
        screenVector = (cast sx - rx/2, -(cast sy - ry/2))
        s = (1.0 / zoom camera) `scale` screenVector
        in position camera + s

export
getRect : Camera -> (position : Vector2D) -> (dimensions : Vector2D) -> SDLRect
getRect camera position dimensions
  = let dimensions' = (fst dimensions, - (snd dimensions))
        (x, y) = positionToScreen camera (position - dimensions')
        (x', y') = positionToScreen camera (position + dimensions')
        (w, h) = (x' - x, y' - y) in MkSDLRect x y w h

export
dimToScreen' : Camera -> Vector2D -> (Int, Int)
dimToScreen' camera dims = let (MkSDLRect x y w h) = getRect camera nullVector dims
                              in (w, h)

export
lengthToScreen : Camera -> Double -> Int
lengthToScreen camera x = cast $ zoom camera * x
