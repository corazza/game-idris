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
import Objects

GameState : (GameIO m, Draw m, Scene m) => Type
GameState {m} = Composite [SDraw {m},
                           SScene {m},
                           State Camera,
                           State Int,
                           State Int] -- carry

render : (GameIO m, Scene m, Draw m) =>
         (state : Var) -> UISettings -> ST m () [state ::: GameState {m}]
render state uiSettings = with ST do
  [draw, scene, camera', lastms, carry] <- split state
  let camera = !(read camera')
  let objects = !(getObjects scene)
  clear draw
  drawBackground draw camera !(getBackground scene)
  drawObjects draw camera objects
  drawInfoObjects draw camera objects uiSettings
  present draw
  combine state [draw, scene, camera', lastms, carry]

iterateCarry : GameIO m => Scene m =>
               (scene : Var) -> (dt : Int) -> (time : Int) ->
               ST m Int [scene ::: SScene {m}]
iterateCarry scene dt time = if time > dt
  then with ST do
    iterate scene dt
    if time `div` dt < 8
      then iterateCarry scene dt (time - dt)
      else pure 0
  else pure time

loop : (GameIO m, Scene m, Draw m) =>
       (state : Var) -> Settings -> ST m () [state ::: GameState {m}]
loop state settings = with ST do
  let (MkSettings uiSettings displaySettings sceneSettings) = settings
  beforems <- ticks
  Right events <- poll | pure ()
  [draw, scene, camera', lastms, carry'] <- split state
  let passed = beforems - !(read lastms)
  carry <- read carry'
  write lastms beforems
  camera <- read camera'
  controlEvent scene "player" camera events
  newCarry <- iterateCarry scene (timeStep sceneSettings) (passed+carry)
  write carry' newCarry
  Just position <- runScript scene $ GetPosition "player" | ?noPlayerPositionLoop
  write camera' (translate (position + (0, yd camera)) camera)
  combine state [draw, scene, camera', lastms, carry']
  render state uiSettings
  loop state settings

game : (GameIO m, Draw m, Scene m) => ST m () []
game {m} = with ST do
  Right settings <- lift $ loadSettings "settings.json"
      | Left e => log ("couldn't load settings: " ++ e)
  let (MkSettings uiSettings displaySettings sceneSettings) = settings
  let r = resolution displaySettings
  draw <- initDraw (fst r) (snd r)
  mapCache <- initCache {r=MapDescriptor}
  Right map <- get {m} {r=MapDescriptor} mapCache "main/maps/castle.json" | ?noMap
  scene <- startScene map sceneSettings
  let playerCreation = MkCreation (Just "player") "main/objects/disciple.json"
    (0, -9) 0 empty (BoxData Nothing)
  create scene playerCreation
  state <- new ()
  camera <- new (fromSettings displaySettings)
  lastms <- new !ticks
  carry <- new 0
  combine state [draw, scene, camera, lastms, carry]
  loop state settings
  [draw, scene, camera, lastms, carry] <- split state
  quitDraw draw
  endScene scene
  quitCache {r=MapDescriptor} mapCache
  delete camera; delete lastms; delete carry
  delete state

main : IO ()
main = with IO do
  disableBuffering
  run game
