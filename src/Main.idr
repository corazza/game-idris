module Main

import Graphics.SDL2 as SDL2
import System as System
import Control.ST
import Control.ST.ImplicitCall
import Data.AVL.Dict
import Physics.Box2D

import Draw
import Col
import Scene
import Objects
import Events
import Input
import Physics.Vector2D
import Physics
import Physics.Box2D

interface GameIO (m : Type -> Type) where
  ticks : STrans m Int xs (const xs)

GameIO IO where
  ticks = lift getTicks

GameState : (Draw m, ConsoleIO m, Box2DPhysics m, Scene m) => Type
GameState {m} = Composite [SDraw {m},
                           SScene {m},
                           State Vector2D,
                           State Int]

screenScale : Double
screenScale = 33

resolution : (Int, Int)
resolution = (1280, 960)

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

drawScene : (Draw m, ConsoleIO m, Box2DPhysics m, Scene m) => (state : Var) ->
            ST m () [state ::: GameState {m}]
drawScene state = (with ST do
  [draw, scene, camera, lastms] <- split state
  clear draw
  drawObjects draw !(read camera) !(getObjects scene)
  present draw
  combine state [draw, scene, camera, lastms]) where
    drawObjects : Draw m => (draw : Var) -> (camera : Vector2D) -> List Object ->
                  ST m () [draw ::: SDraw {m}]
    drawObjects draw camera [] = pure ()
    drawObjects draw camera (object :: xs) = with ST do
      let (x, y) = positionToScreen camera ((position object) - (dim object))
      let (w, h) = dimToScreen (2 `scale` dim object)
      let dst = MkSDLRect x y w h
      drawTexture draw (texture object) Nothing (Just dst)
      drawObjects draw camera xs

loop : (ConsoleIO m, Draw m, GameIO m, Box2DPhysics m, Scene m) => (state : Var) ->
       ST m () [state ::: GameState {m}]
loop state = with ST do
  Right event <- poll
              | pure ()
  [draw, scene, camera, lastms] <- split state
  controlEvent scene "player" event
  beforems <- ticks
  iterate scene (beforems - !(read lastms))
  write lastms beforems
  combine state [draw, scene, camera, lastms]
  drawScene state
  loop state

game : (ConsoleIO m, Draw m, GameIO m, Box2DPhysics m, Scene m) => ST m () []
game = with ST do
  draw <- initDraw (fst resolution) (snd resolution)
  scene <- startScene (snd resolution) (snd resolution)
  playerTexture <- getTexture draw "disciple"
  let playerBoxDesc = MkBoxDescription 5 Dynamic (0.5, 48.0/33.0/2.0)
  let player = MkObject "player" (0, 20) playerBoxDesc playerTexture
  addObject scene player
  state <- new ()
  camera <- new (0, 0)
  lastms <- new !ticks
  combine state [draw, scene, camera, lastms]
  loop state
  [draw, scene, camera, lastms] <- split state
  quitDraw draw
  endScene scene
  delete camera; delete lastms
  delete state

main : IO ()
main = run game
-- main = print !test
