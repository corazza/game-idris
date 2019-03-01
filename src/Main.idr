module Main

import Graphics.SDL2 as SDL2
import System as System
import Control.ST
import Control.ST.ImplicitCall
import Data.AVL.Dict

import Draw
import Col
import Scene
import Objects
import Events
import Input
import Vector2D
import Physics

GameState : Draw m => Type
GameState {m} = Composite [SDraw {m}, SScene, State Vector2D]

screenScale : Double
screenScale = 33

positionToScreen : (camera : Vector2D) -> (position : Vector2D) -> (Int, Int)
positionToScreen (cx, cy) (ox, oy) = cast $ screenScale `scale` (ox - cx, oy - cy)

dimToScreen : (dim : Vector2D) -> (Int, Int)
dimToScreen (x, y) = cast $ (screenScale * x, screenScale * y)

drawScene : Draw m => (state : Var) ->
            ST m () [state ::: GameState {m}]
drawScene state = (with ST do
  [draw, scene, camera] <- split state
  [idCounter, objects, events, physics] <- split scene
  clear draw
  objectDict <- read objects
  combine scene [idCounter, objects, events, physics]
  drawObjects draw !(read camera) (toList objectDict)
  present draw
  combine state [draw, scene, camera]) where
    drawObjects : Draw m => (draw : Var) -> (camera : Vector2D) -> List Object ->
                  ST m () [draw ::: SDraw {m}]
    drawObjects draw camera [] = pure ()
    drawObjects draw camera (object :: xs) = with ST do
      let (x, y) = positionToScreen camera (position object)
      let (w, h) = dimToScreen (dim (boxDescription object))
      let dst = MkSDLRect x y w h
      drawTexture draw (texture object) Nothing (Just dst)
      drawObjects draw camera xs

loop : (ConsoleIO m, Draw m) => (state : Var) ->
       ST m () [state ::: GameState {m}]
loop state = with ST do
  Right event <- poll
              | pure ()
  [draw, scene, camera] <- split state
  controlEvent scene "player" event
  iterate scene
  combine state [draw, scene, camera]
  drawScene state
  loop state

resolution : (Int, Int)
resolution = (1280, 960)

game : (ConsoleIO m, Draw m) => ST m () []
game = with ST do
  draw <- initDraw (fst resolution) (snd resolution)
  scene <- startScene (snd resolution) (snd resolution)
  playerTexture <- getTexture draw "disciple"
  let playerBoxDesc = MkBoxDescription 5 Dynamic (1, 48.0/33.0)
  let player = MkObject "player" (3, 3) playerBoxDesc playerTexture
  addObject scene player
  state <- new ()
  camera <- new (0, 0)
  combine state [draw, scene, camera]
  loop state
  [draw, scene, camera] <- split state
  quitDraw draw
  endScene scene
  delete camera
  delete state

main : IO ()
main = run game
