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

GameState : Draw m => Type
GameState {m} = Composite [SDraw {m}, SScene]

drawScene : Draw m => (state : Var) ->
            ST m () [state ::: GameState {m}]
drawScene state = (with ST do
  [draw, scene] <- split state
  [idCounter, objects, events] <- split scene
  clear draw
  objectDict <- read objects
  combine scene [idCounter, objects, events]
  drawObjects draw (toList objectDict)
  present draw
  combine state [draw, scene]) where
    drawObjects : Draw m => (draw : Var) -> List Object -> ST m () [draw ::: SDraw {m}]
    drawObjects draw [] = pure ()
    drawObjects draw (object :: xs) = with ST do
      let dst = MkSDLRect (x object) (y object) (w object) (h object)
      drawTexture draw (texture object) Nothing (Just dst)
      drawObjects draw xs

loop : (ConsoleIO m, Draw m) => (state : Var) ->
       ST m () [state ::: GameState {m}]
loop state = with ST do
  Right event <- poll
              | pure ()

  [draw, scene] <- split state
  controlEvent scene "player" event
  iterate scene
  combine state [draw, scene]

  drawScene state
  loop state

game : (ConsoleIO m, Draw m) => ST m () []
game = with ST do
  draw <- initDraw 640 480
  scene <- startScene 640 480

  playerTexture <- getTexture draw "disciple"
  let player = MkObject "player" 50 50 0 0 33 48 playerTexture
  addObject scene player

  state <- new ()
  combine state [draw, scene]
  loop state

  [draw, scene] <- split state
  quitDraw draw
  endScene scene
  delete state

main : IO ()
main = run game
