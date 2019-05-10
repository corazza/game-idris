module Main

import Graphics.SDL2 as SDL2
import System as System
import Control.ST
import Control.ST.ImplicitCall
import Data.AVL.Dict
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

GameState : (Monad m, GameIO m, Draw m, ConsoleIO m, Box2DPhysics m, Scene m) => Type
GameState {m} = Composite [SDraw {m},
                           SScene {m},
                           State Vector2D,
                           SCache {m} {r=Texture},
                           State Int]

screenScale : Double
screenScale = 33

resolution : (Int, Int)
resolution = (1280, 800)

Cast (Int, Int) (Double, Double) where
  cast (x, y) = (cast x, cast y)

resolution' : (Double, Double)
resolution' = cast resolution

positionToScreen : (camera : Vector2D) -> (position : Vector2D) -> (Int, Int)
positionToScreen (cx, cy) (ox, oy)
  = let (x, y) = screenScale `scale` (ox - cx, cy - oy) in
        cast (x + (fst resolution')/2, y + (snd resolution')/2)

dimToScreen : (dim : Vector2D) -> (Int, Int)
dimToScreen (x, y) = cast $ (screenScale * x, screenScale * y)

drawScene : (Monad m,
             ConsoleIO m,
             Box2DPhysics m,
             GameIO m,
             Scene m,
             Draw m) =>
             (state : Var) ->
             ST m () [state ::: GameState {m}]
drawScene state = (with ST do
  [draw, scene, camera, textureCache, lastms] <- split state
  clear draw
  drawBackground draw !(read camera) !(getBackground scene) textureCache
  drawObjects draw !(read camera) !(getObjects scene) textureCache
  present draw
  combine state [draw, scene, camera, textureCache, lastms]) where
    drawBackground : (Draw m, ConsoleIO m) =>
                     (draw : Var) -> (camera : Vector2D) -> (background : Background) -> (cache : Var) ->
                     ST m () [cache ::: SCache {m} {r=Texture}, draw ::: SDraw {m}]
    drawBackground {m = m} draw camera (MkBackground image dim) cache = with ST do
      Just texture <- get {m} {r=Texture} cache draw image | Nothing => ?noTextureDrawBackground
      let (w, h) = dimToScreen $ 2 `scale` dim
      let (w', h') = dimToScreen dim
      let (x, y) = positionToScreen camera (0, 0)
      drawWholeCenter draw texture (MkSDLRect (x - w') (y - h') w h) 0.0

    drawObjects : (Draw m, ConsoleIO m) =>
                  (draw : Var) -> (camera : Vector2D) -> List Object -> (cache : Var) ->
                  ST m () [cache ::: SCache {m} {r=Texture}, draw ::: SDraw {m}]
    drawObjects draw camera [] cache = pure ()
    -- w = full width on screen, w' = half width on screen
    drawObjects {m} draw camera (object :: xs) cache = (with ST do
      let (w, h) = dimToScreen $ 2 `scale` (dim object)
      let (w', h') = dimToScreen (dim object)
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
          let (tw, th) = dimToScreen $ 2 `scale` tileDims
          let (tw', th') = dimToScreen tileDims
          Just texture <- get {m} {r=Texture} cache draw textureRef | Nothing => ?noTexture
          tile texture (x, y) (w, h) (w', h') (tw, th) (tw', th') (nx, ny) nx
          drawObjects draw camera xs cache) where
            tile : Texture -> (Int, Int) ->
                   (Int, Int) -> (Int, Int) ->
                   (Int, Int) -> (Int, Int) ->
                   (Nat, Nat) -> Nat ->
                   ST m () [draw ::: SDraw {m}]
            tile texture _ _ _ _ _ (nx , Z) nx' = pure ()
            tile texture (x, y) (w, h) (w', h') (tw, th) (tw', th') (Z, S ny) nx'
              = tile texture (x, y) (w, h) (w', h') (tw, th) (tw', th') (nx', ny) nx'
            tile texture (x, y) (w, h) (w', h') (tw, th) (tw', th') (S nx, S ny) nx' = with ST do
              drawWholeCenter draw texture (MkSDLRect (x - w' + (cast nx)*tw) (y - h' + (cast ny)*th) tw th) 0.0
              -- only x remains deincremented
              tile texture (x, y) (w, h) (w', h') (tw, th) (tw', th') (nx, S ny) nx'

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
  [draw, scene, camera, textureCache, lastms] <- split state
  controlEvent scene "player" (case events of -- TODO tf is this
                                    [] => Nothing
                                    (x :: xs) => Just x)
  beforems <- ticks
  iterate scene (beforems - !(read lastms))
  write lastms beforems
  combine state [draw, scene, camera, textureCache, lastms]
  drawScene state
  loop state

game : (Monad m, ConsoleIO m, Draw m, GameIO m, Box2DPhysics m, Scene m) => ST m () []
game {m} = with ST do
  draw <- initDraw (fst resolution) (snd resolution)
  textureCache <- initCache {r=Texture}
  mapCache <- initCache {r=MapDescriptor}
  emptyContext <- createEmptyContext

  Just map <- get {m} {r=MapDescriptor} mapCache emptyContext "likert" | Nothing => ?noLikert
  scene <- startScene map
  let playerCreation = MkCreation (Just "player")
                                  "disciple"
                                  (0, 5)
                                  []
                                  (BoxData Nothing)
  create scene playerCreation

  state <- new ()
  camera <- new (0, 0)
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
