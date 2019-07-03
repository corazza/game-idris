module Main

import Control.ST
import Control.ST.ImplicitCall
import Data.AVL.Set

import GameIO
import Rendering
import Scene
import Camera
import Input
import Settings
import Script
import Resources
import Physics.Vector2D
import Descriptors
import Common
import Events

GameState : (GameIO m, Draw m, Scene m) => Type
GameState {m} = Composite [SDraw {m},
                           SScene {m},
                           State Camera,
                           State Int]

loop : (GameIO m, Scene m, Draw m) => (state : Var) -> ST m () [state ::: GameState {m}]
loop state = with ST do
  Right events <- poll | pure ()
  [draw, scene, camera', lastms] <- split state
  let camera = !(read camera')
  controlEvent scene "player" camera events
  -- TODO camera smoothing
  beforems <- ticks
  iterate scene (beforems - !(read lastms))
  write lastms beforems
  Just position <- runScript scene $ GetPosition "player" | ?noPlayerPositionLoop
  write camera' (translate position camera)
  drawScene draw scene camera
  combine state [draw, scene, camera', lastms]
  loop state

game : (GameIO m, Draw m, Scene m) => ST m () []
game {m} = with ST do
  Right settings <- lift $ loadSettings "settings.json" | ?noSettings
  let r = resolution settings
  draw <- initDraw (fst r) (snd r)
  mapCache <- initCache {r=MapDescriptor}

  Right map <- get {m} {r=MapDescriptor} mapCache "likert" | ?noLikert
  scene <- startScene map (sceneSettings settings)
  let playerCreation = MkCreation (Just "player") "disciple" (0, 5) 0 empty (BoxData Nothing)
  create scene playerCreation

  state <- new ()
  camera <- new (fromSettings settings)
  lastms <- new !ticks
  combine state [draw, scene, camera, lastms]

  loop state

  [draw, scene, camera, lastms] <- split state
  quitDraw draw
  endScene scene
  quitCache {r=MapDescriptor} mapCache
  delete camera; delete lastms
  delete state

main : IO ()
main = with IO do
  disableBuffering
  run game
