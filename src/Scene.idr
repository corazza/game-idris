module Scene

import Control.ST
import Data.Vect
import Data.Queue
import Data.AVL.Dict
import Graphics.SDL2

import Events
import Objects

public export
SScene : Type
SScene = Composite [State Nat, -- id counter
                    State (Dict String Object),
                    State (List Events.Event)]

export
startScene : Int -> Int -> ST m Var [add SScene]
startScene x y = with ST do
  objects <- new empty
  events <- new []
  idCounter <- new Z
  scene <- new ()
  combine scene [idCounter, objects, events]
  pure scene

export
endScene : (scene : Var) -> ST m () [remove scene SScene]
endScene scene = with ST do
  [idCounter, objects, events] <- split scene
  delete idCounter; delete objects; delete events
  delete scene

export
registerEvent : (scene : Var) -> Events.Event -> ST m () [scene ::: SScene]
registerEvent scene event = with ST do
  [idCounter, objects, events] <- split scene
  write events (event :: !(read events))
  combine scene [idCounter, objects, events]

addWithId : (scene : Var) -> (object : Object) -> ST m String [scene ::: SScene]
addWithId scene object = with ST do
  [idCounter, objects, events] <- split scene
  objectDict <- read objects
  write objects (insert (id object) object objectDict)
  combine scene [idCounter, objects, events]
  pure (id object)

export
addObject : (scene : Var) -> (object : Object) -> ST m String [scene ::: SScene]
addObject scene (MkObject "" x y dx dy w h texture) = with ST do
  [idCounter, objects, events] <- split scene
  let idNum = !(read idCounter)
  let idString = "autoid_" ++ show idNum
  write idCounter (idNum + 1)
  combine scene [idCounter, objects, events]
  addWithId scene (MkObject idString x y dx dy w h texture)

addObject scene object = addWithId scene object


-- TODO generate events
handleEvents : (scene : Var) ->
             List Events.Event ->
             ST m (List Events.Event) [scene ::: SScene]
handleEvents scene [] = pure []
handleEvents scene (x::xs) = handle scene x >>= \_=> handleEvents scene xs where
  handle : (scene : Var) -> Events.Event -> ST m (List Events.Event) [scene ::: SScene]
  handle scene (MovementStart direction id) = with ST do
    [idCounter, objects, events] <- split scene
    objectDict <- read objects
    write objects (Data.AVL.API.Dict.update id (record {
      dx = case direction of
        MoveLeft => -1
        MoveRight => 1
      }) objectDict)
    combine scene [idCounter, objects, events]
    pure []

  handle scene (MovementStop id) = with ST do
    [idCounter, objects, events] <- split scene
    objectDict <- read objects
    write objects (Data.AVL.API.Dict.update id (record {dx=0}) objectDict)
    combine scene [idCounter, objects, events]
    pure []

  handle scene (Attack id) = pure []

iterateEvents : (scene : Var) -> ST m (List Events.Event) [scene ::: SScene]
iterateEvents scene = with ST do
  [idCounter, objects, events] <- split scene
  eventList <- read events
  write events (the (List Events.Event) [])
  combine scene [idCounter, objects, events]
  nextEvents <- handleEvents scene eventList
  [idCounter, objects, events] <- split scene
  write events nextEvents
  combine scene [idCounter, objects, events]
  pure nextEvents

iterateMovement : (scene : Var) -> ST m () [scene ::: SScene]
iterateMovement scene = (with ST do
  [idCounter, objects, events] <- split scene
  objectDict <- read objects
  write objects (map move objectDict)
  combine scene [idCounter, objects, events]) where
    move : Object -> Object
    move object = record {x $= (+ dx object)} object

export
iterate : (scene : Var) -> ST m () [scene ::: SScene]
iterate scene = with ST do
  nextEvents <- iterateEvents scene
  iterateMovement scene
