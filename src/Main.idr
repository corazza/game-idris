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
import Resources
import Physics.Vector2D
import Descriptors
import Common
import Events
import Objects
import Timeline

GameState : (GameIO m, Draw m, Scene m) => Type
GameState {m} = Composite [SDraw {m},
                           SScene {m},
                           State Camera,
                           State Int,
                           State Int, -- carry
                           State String]

render : (GameIO m, Scene m, Draw m) =>
         (state : Var) -> UISettings -> ST m () [state ::: GameState {m}]
render state uiSettings = with ST do
  [draw, scene, camera', lastms, carry, characterId] <- split state
  let camera = !(read camera')
  let objects = !(getObjects scene)
  clear draw
  drawBackground draw camera !(getBackground scene)
  drawObjects draw camera objects
  drawInfoObjects draw camera objects uiSettings
  present draw
  combine state [draw, scene, camera', lastms, carry, characterId]

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
  [draw, scene, camera', lastms, carry', characterId'] <- split state
  characterId <- read characterId'
  let passed = beforems - !(read lastms)
  carry <- read carry'
  write lastms beforems
  camera <- read camera'
  controlEvent scene characterId camera events
  newCarry <- iterateCarry scene (timeStep sceneSettings) (passed+carry)
  write carry' newCarry
  Just position <- runScript scene $ GetPosition characterId
                | combine state [draw, scene, camera', lastms, carry', characterId']
  write camera' (translate (position + (0, yd camera)) camera)
  combine state [draw, scene, camera', lastms, carry', characterId']
  render state uiSettings
  loop state settings

game : (GameIO m, Draw m, Scene m) =>
       Settings -> String -> Character -> ST m () []
game {m} settings@(MkSettings uiSettings displaySettings sceneSettings)
     characterId character@(MkCharacter name object position map) = with ST do
  let r = resolution displaySettings
  let playerCreation = MkCreation
    (Just characterId) object position 0 empty (BoxData Nothing)
  mapCache <- initCache {r=MapDescriptor}
  Right map <- get {m} {r=MapDescriptor} mapCache map | with ST do
    log $ "can't load map \"" ++ map ++ "\""
    quitCache {r=MapDescriptor} mapCache
  draw <- initDraw (fst r) (snd r)
  scene <- startScene map sceneSettings
  create scene playerCreation
  state <- new ()
  camera <- new (fromSettings displaySettings)
  lastms <- new !ticks
  carry <- new 0
  characterId' <- new characterId
  combine state [draw, scene, camera, lastms, carry, characterId']
  loop state settings
  [draw, scene, camera, lastms, carry, characterId'] <- split state
  quitDraw draw
  endScene scene
  quitCache {r=MapDescriptor} mapCache
  delete camera; delete lastms; delete carry; delete characterId'
  delete state

start : (GameIO m, Draw m, Scene m) => ST m () []
start {m} = with ST do
  Right settings <- lift $ loadSettings "settings.json"
      | Left e => log ("couldn't load settings: " ++ e)

  timelineCache <- initCache {r=Timeline}
  Right timeline <- get {m} {r=Timeline} timelineCache "default" | ?noTimeline

  let characterId = character timeline

  case lookup characterId (characters timeline) of
    Nothing => with ST do
      log $ "can't find character with id \"" ++ characterId ++ "\""
      quitCache {r=Timeline} timelineCache
    Just character => with ST do
      game settings characterId character
      quitCache {r=Timeline} timelineCache

main : IO ()
main = with IO do
  disableBuffering
  run start
