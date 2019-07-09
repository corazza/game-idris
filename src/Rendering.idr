module Rendering

import Control.ST
import Control.ST.ImplicitCall
import Graphics.SDL2 as SDL2
import Data.AVL.Dict

import public Rendering.Draw
import GameIO
import Scene
import Camera
import Descriptors
import Common
import Objects
import Physics.Vector2D
import Settings

-- 0 no flip, 1 vertical, 2 horizontal, 3 both
getFlip : Object -> Animation -> Int
getFlip object animation = case (facingRight animation, forceDirection object) of
  (True, Leftward) => 2
  (True, Rightward) => 0
  (False, Leftward) => 0
  (False, Rightward) => 2

getFlip' : Object -> Int
getFlip' object = case forceDirection object of
  Leftward => 2
  Rightward => 0

export
drawBackground : (GameIO m, Draw m) =>
                (draw : Var) -> (camera : Camera) -> (background : Background) ->
                ST m () [draw ::: SDraw {m}]
drawBackground draw camera (MkBackground image dim) = with ST do
  Just texture <- getTexture draw image | Nothing => ?noTextureDrawBackground
  let (w, h) = dimToScreen camera (2 `scale` dim)
  let (w', h') = dimToScreen camera dim
  let (x, y) = positionToScreen camera (0, 0)
  drawWholeCenter draw texture (MkSDLRect (x - w') (y - h') w h) 0.0 0

tile : Draw m => GameIO m =>
       (draw : Var) -> (camera : Camera) -> (texture : Texture) -> (position : (Int, Int)) ->
       (tileDims : (Int, Int)) -> (howMany : (Nat, Nat)) -> ST m () [draw ::: SDraw {m}]
tile draw camera texture (x, y) (w, h) (nx, Z) = pure ()
tile {m} draw camera texture (x, y) (w, h) (nx, S ny) = (with ST do
  tileRow (x, y) nx
  tile draw camera texture (x, y + h) (w, h) (nx, ny)) where
    tileRow : (position : (Int, Int)) -> (k : Nat) -> ST m () [draw ::: SDraw {m}]
    tileRow _ Z = pure ()
    tileRow (x', y') (S k) = with ST do
      let rect = MkSDLRect x' y' w h
      drawWholeCenter draw texture rect 0.0 0
      tileRow (x' + w, y') k

-- w = full width on screen, w' = half width on screen
drawObject : (GameIO m, Draw m) =>
             (draw : Var) -> (camera : Camera) -> Object ->
             ST m () [draw ::: SDraw {m}]
drawObject {m} draw camera object = let deg_angle = -(angle object) / (2.0*pi) * 360.0
  in case renderDescription object of
    DrawBox textureRef dimensions => with ST do
      Just texture <- getTexture draw textureRef
                   | log ("missing texture " ++ textureRef ++ " on " ++ id object)
      let rect = getRect camera (position object) dimensions
      drawWholeCenter draw texture rect deg_angle (getFlip' object)
    Animated states (MkAnimationState state ticks) => case lookup state states of
      Nothing => log $ "missing animation state \"" ++ state ++ "\" in " ++ show states
      Just aparams => with ST do
        Right animation <- getAnimation draw (animation aparams)
                        | Left e => log e
        Just sheet <- getTexture draw (sheet animation)
                   | log ("missing texture " ++ sheet animation)
        let passed_frames = ticks `div` cast (1000 * speed aparams)
        let frame = passed_frames `mod` nx animation * ny animation
        let framex = frame `div` ny animation
        let framey = frame `div` nx animation
        let (w, h) = dimensions animation
        let src = MkSDLRect (framex*w) (framey*h) w h
        let dst = getRect camera (position object) (dimensions aparams)
        drawCenter draw sheet src dst deg_angle (getFlip object animation)
    TileWith textureRef tileDims@(w, h) howMany@(nx, ny) => with ST do
      Just texture <- getTexture draw textureRef
                   | log ("missing texture " ++ textureRef ++ " on " ++ id object)
      let tileDimsFull = dimToScreen camera (2 `scale` tileDims)
      let topleft = position object - (cast nx * w, - cast ny * h)
      let initial = positionToScreen camera topleft
      tile draw camera texture initial tileDimsFull (nx, ny)
    Invisible => pure ()

export
drawObjects : (GameIO m, Draw m) =>
              (draw : Var) -> (camera : Camera) -> List Object ->
              ST m () [draw ::: SDraw {m}]
drawObjects draw camera [] = pure ()
drawObjects {m} draw camera (object :: xs) = with ST do
  drawObject draw camera object
  drawObjects draw camera xs

infoDimensions : Object -> Vector2D
infoDimensions object = case renderDescription object of
  DrawBox ref dimensions => dimensions
  TileWith ref tileDims z => tileDims
  Animated dict (MkAnimationState state _) => case lookup state dict of
    Nothing => nullVector
    Just aparams => dimensions aparams
  Invisible => nullVector

drawInfoObject : (GameIO m, Draw m) =>
                 (draw : Var) -> (camera : Camera) -> Object -> UISettings ->
                 ST m () [draw ::: SDraw {m}]
drawInfoObject draw camera object settings = case health object of
  Just health => with ST do
    let dimensions = infoDimensions object
    let (x, y) = positionToScreen camera (position object)
    let (w, h) = dimToScreen camera dimensions
    let fullHealthWidth' = fullHealthWidth settings
    let rect = MkSDLRect (x - (cast (cast fullHealthWidth'*0.5)))
                         (y + h + (healthYD settings))
                         (cast $ (percent health) * (cast fullHealthWidth'))
                         (fullHealthHeight settings)
    filledRect draw rect (healthColor settings)
  _ => pure ()

export
drawInfoObjects : (GameIO m, Draw m) =>
                 (draw : Var) -> (camera : Camera) -> List Object -> UISettings ->
                 ST m () [draw ::: SDraw {m}]
drawInfoObjects draw _ [] _ = pure ()
drawInfoObjects draw camera (object :: xs) settings = with ST do
  drawInfoObject draw camera object settings
  drawInfoObjects draw camera xs settings
