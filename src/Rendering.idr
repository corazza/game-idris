module Rendering

import Control.ST
import Control.ST.ImplicitCall
import Graphics.SDL2 as SDL2

import public Rendering.Draw
import GameIO
import Scene
import Camera
import Descriptors
import Common
import Objects
import Physics.Vector2D
import Settings

export
drawBackground : (GameIO m, Draw m) =>
                (draw : Var) -> (camera : Camera) -> (background : Background) ->
                ST m () [draw ::: SDraw {m}]
drawBackground draw camera (MkBackground image dim) = with ST do
  Just texture <- getTexture draw image | Nothing => ?noTextureDrawBackground
  let (w, h) = dimToScreen camera (2 `scale` dim)
  let (w', h') = dimToScreen camera dim
  let (x, y) = positionToScreen camera (0, 0)
  drawWholeCenter draw texture (MkSDLRect (x - w') (y - h') w h) 0.0

-- w = full width on screen, w' = half width on screen
export
drawObjects : (GameIO m, Draw m) =>
              (draw : Var) -> (camera : Camera) -> List Object ->
              ST m () [draw ::: SDraw {m}]
drawObjects draw camera [] = pure ()
drawObjects {m} draw camera (object :: xs) = (with ST do
  let (w, h) = dimToScreen camera (2 `scale` (dim object))
  let (w', h') = dimToScreen camera (dim object)
  let (x, y) = positionToScreen camera (position object)
  let deg_angle = -(angle object) / (2.0*pi) * 360.0
  case renderDescription object of
    Invisible => drawObjects draw camera xs
    DrawBox textureRef _ => with ST do
      Just texture <- getTexture draw textureRef | Nothing => ?noTextureDrawBox
      drawWholeCenter draw texture (MkSDLRect (x - w') (y - h') w h) deg_angle
      drawObjects draw camera xs
    TileWith textureRef tileDims (nx, ny) => with ST do -- TODO totality in nx, ny
      let (tw, th) = dimToScreen camera (2 `scale` tileDims)
      let (tw', th') = dimToScreen camera tileDims
      Just texture <- getTexture draw textureRef | Nothing => ?noTexture
      tile texture (x, y) (w, h) (w', h') (tw, th) (tw', th') (nx, ny) nx
      drawObjects draw camera xs) where
        tile : Texture -> (Int, Int) ->
               (Int, Int) -> (Int, Int) ->
               (Int, Int) -> (Int, Int) ->
               (Nat, Nat) -> Nat ->
               ST m () [draw ::: SDraw {m}]
        tile texture _ _ _ _ _ (nx , Z) nx' = pure ()
        tile texture xy wh wh' t t' (Z, S ny) nx'
          = tile texture xy wh wh' t t' (nx', ny) nx'
        tile texture (x, y) (w, h) (w', h') (tw, th) (tw', th') (S nx, S ny) nx' = with ST do
          drawWholeCenter draw texture (MkSDLRect (x - w' + (cast nx)*tw) (y - h' + (cast ny)*th) tw th) 0.0
          -- only x remains deincremented
          tile texture (x, y) (w, h) (w', h') (tw, th) (tw', th') (nx, S ny) nx'

export
drawObjectInfo : (GameIO m, Draw m) =>
                 (draw : Var) -> (camera : Camera) -> List Object ->
                 ST m () [draw ::: SDraw {m}]
drawObjectInfo draw _ [] = pure ()
drawObjectInfo draw camera (object :: xs) = case health object of
  Nothing => drawObjectInfo draw camera xs
  Just health => with ST do
    let (x, y) = positionToScreen camera (position object)
    let (w', h') = dimToScreen camera (dim object)
    let fullHealthWidth' = fullHealthWidth defaultSettings
    filledRect draw (MkSDLRect (x - (cast (cast fullHealthWidth'*0.5)))
                               (y + h' + (healthYD defaultSettings))
                               (cast $ (percent health) * (cast fullHealthWidth'))
                               (fullHealthHeight defaultSettings)) (healthColor defaultSettings)
    drawObjectInfo draw camera xs
