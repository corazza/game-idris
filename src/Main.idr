module Main

import Graphics.SDL2 as SDL2
import System as System
import Control.ST
import Control.ST.ImplicitCall
import Data.AVL.Dict
import Data.AVL.Set
import Physics.Box2D
import Language.JSON

import Draw
import Scene
import Objects
import Events
import Input
import Physics.Vector2D
import Physics.Box2D
import GameIO
import Resources
import Descriptors
import Common
import Script
import Settings
import Camera

GameState : (Monad m, GameIO m, Draw m, ConsoleIO m, Box2DPhysics m, Scene m) => Type
GameState {m} = Composite [SDraw {m},
                           SScene {m},
                           State Camera,
                           SCache {m} {r=Texture},
                           State Int]

drawBackground : (Draw m, ConsoleIO m) =>
                (draw : Var) -> (camera : Camera) -> (background : Background) -> (cache : Var) ->
                ST m () [cache ::: SCache {m} {r=Texture}, draw ::: SDraw {m}]
drawBackground {m = m} draw camera (MkBackground image dim) cache = with ST do
  Just texture <- get {m} {r=Texture} cache draw image | Nothing => ?noTextureDrawBackground
  let (w, h) = dimToScreen camera (2 `scale` dim)
  let (w', h') = dimToScreen camera dim
  let (x, y) = positionToScreen camera (0, 0)
  drawWholeCenter draw texture (MkSDLRect (x - w') (y - h') w h) 0.0

drawObjects : (Draw m, ConsoleIO m) =>
              (draw : Var) -> (camera : Camera) -> List Object -> (cache : Var) ->
              ST m () [cache ::: SCache {m} {r=Texture}, draw ::: SDraw {m}]
drawObjects draw camera [] cache = pure ()
-- w = full width on screen, w' = half width on screen
drawObjects {m} draw camera (object :: xs) cache = (with ST do
  let (w, h) = dimToScreen camera (2 `scale` (dim object))
  let (w', h') = dimToScreen camera (dim object)
  let (x, y) = positionToScreen camera (position object)
  let deg_angle = -(angle object) / (2.0*pi) * 360.0
  case renderDescription object of
    Invisible => drawObjects draw camera xs cache
    DrawBox textureRef => with ST do
      Just texture <- get {m} {r=Texture} cache draw textureRef | Nothing => ?noTextureDrawBox
      drawWholeCenter draw texture (MkSDLRect (x - w') (y - h') w h) deg_angle
      -- drawWholeCenter draw texture (MkSDLRect x y w h) deg_angle -- TODO why doesn't this work?
      drawObjects draw camera xs cache
    TileWith textureRef tileDims (nx, ny) => with ST do -- TODO totality in nx, ny
      let (tw, th) = dimToScreen camera (2 `scale` tileDims)
      let (tw', th') = dimToScreen camera tileDims
      Just texture <- get {m} {r=Texture} cache draw textureRef | Nothing => ?noTexture
      tile texture (x, y) (w, h) (w', h') (tw, th) (tw', th') (nx, ny) nx
      drawObjects draw camera xs cache) where
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

drawObjectInfo : (Draw m, ConsoleIO m) =>
                 (draw : Var) -> (camera : Camera) -> List Object ->
                 ST m () [draw ::: SDraw {m}]
drawObjectInfo draw _ [] = pure ()
drawObjectInfo {m} draw camera (object :: xs) = case health object of
  Nothing => drawObjectInfo draw camera xs
  Just health => with ST do
    let (x, y) = positionToScreen camera (position object)
    let (w', h') = dimToScreen camera (dim object)
    let fullHealthWidth' = fullHealthWidth defaultSettings
    filledRect draw (MkSDLRect (x - (cast (cast fullHealthWidth'*0.5)))
                               (y + w' + (healthYD defaultSettings))
                               (cast $ (percent health) * (cast fullHealthWidth'))
                               (fullHealthHeight defaultSettings)) (healthColor defaultSettings)
    drawObjectInfo draw camera xs

drawScene : (Monad m,
             ConsoleIO m,
             Box2DPhysics m,
             GameIO m,
             Scene m,
             Draw m) =>
             (state : Var) ->
             ST m () [state ::: GameState {m}]
drawScene state = with ST do
  [draw, scene, camera', textureCache, lastms] <- split state
  let camera = !(read camera')
  let objects = !(getObjects scene)
  clear draw
  drawBackground draw camera !(getBackground scene) textureCache
  drawObjects draw camera objects textureCache
  drawObjectInfo draw camera objects
  present draw
  combine state [draw, scene, camera', textureCache, lastms]

loop : (Monad m,
        ConsoleIO m,
        Box2DPhysics m,
        GameIO m,
        Scene m,
        Draw m) =>
        (state : Var) ->
        ST m () [state ::: GameState {m}]
loop state = with ST do
  Right events <- poll
               | pure ()
  [draw, scene, camera', textureCache, lastms] <- split state
  let camera = !(read camera')
  controlEvent scene "player" camera events
  -- TODO camera smoothing
  beforems <- ticks
  iterate scene (beforems - !(read lastms))
  write lastms beforems
  Just position <- runScript scene $ GetPosition "player" | ?noPlayerPositionLoop
  write camera' (translate position camera)
  combine state [draw, scene, camera', textureCache, lastms]
  drawScene state
  loop state

game : (Monad m, ConsoleIO m, Draw m, GameIO m, Box2DPhysics m, Scene m) => ST m () []
game {m} = with ST do
  Just settings <- lift $ loadSettings "settings.json" | Nothing => ?noSettings
  let r = resolution settings
  draw <- initDraw (fst r) (snd r)
  textureCache <- initCache {r=Texture}
  mapCache <- initCache {r=MapDescriptor}
  emptyContext <- createEmptyContext

  Just map <- get {m} {r=MapDescriptor} mapCache emptyContext "likert" | Nothing => ?noLikert
  scene <- startScene map (sceneSettings settings)
  let playerCreation = MkCreation (Just "player") "disciple" (0, 5) 0 empty (BoxData Nothing)
  create scene playerCreation

  state <- new ()
  camera <- new (fromSettings settings)
  lastms <- new !ticks
  combine state [draw, scene, camera, textureCache, lastms]

  loop state

  [draw, scene, camera, textureCache, lastms] <- split state
  quitDraw draw
  endScene scene
  quitCache {r=Texture} textureCache
  quitCache {r=MapDescriptor} mapCache
  deleteEmptyContext emptyContext
  delete camera; delete lastms
  delete state

main : IO ()
main = do
  disableBuffering
  run game
