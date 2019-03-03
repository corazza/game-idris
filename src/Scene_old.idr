module Scene

import Control.ST
import Data.Vect
import Data.Queue
import Data.AVL.Dict
import Graphics.SDL2
import Control.ST.ImplicitCall

import Events
import Objects
import Input
import Physics
import Physics.Box2D
import Physics.Vector2D
-- import Physics.Vector2D

public export
SScene : Type
SScene = Composite [State Nat, -- id counter
                    State (Dict String Object),
                    State (List Events.Event),
                    SPhysics]

export
startScene : Int -> Int -> ST m Var [add SScene]
startScene x y = with ST do
  objects <- new empty
  events <- new []
  idCounter <- new Z
  physics <- initPhysics (MkParameters (-10.0) (Just (Floor (-10.0))))
  scene <- new ()
  combine scene [idCounter, objects, events, physics]
  pure scene

export
endScene : (scene : Var) -> ST m () [remove scene SScene]
endScene scene = with ST do
  [idCounter, objects, events, physics] <- split scene
  quitPhysics physics
  delete idCounter; delete objects; delete events
  delete scene


addWithId : (scene : Var) -> (object : Object) -> ST m String [scene ::: SScene]
addWithId scene object = with ST do
  [idCounter, objects, events, physics] <- split scene
  objectDict <- read objects
  write objects (insert (id object) object objectDict)
  addBox physics (MkBox (id object)
                        (boxDescription object)
                        (position object)
                        nullVector
                        nullVector
                        (insert "movement" nullVector empty))
  combine scene [idCounter, objects, events, physics]
  pure (id object)

export
addObject : (scene : Var) -> (object : Object) -> ST m String [scene ::: SScene]
addObject scene (MkObject "" position boxDescription texture) = with ST do
  [idCounter, objects, events, physics] <- split scene
  let idNum = !(read idCounter)
  let idString = "autoid_" ++ show idNum
  write idCounter (idNum + 1)
  combine scene [idCounter, objects, events, physics]
  addWithId scene (MkObject idString position boxDescription texture)

addObject scene object = addWithId scene object


registerEvent : (scene : Var) -> Events.Event -> ST m () [scene ::: SScene]
registerEvent scene event = with ST do
  [idCounter, objects, events, physics] <- split scene
  write events (event :: !(read events))
  combine scene [idCounter, objects, events, physics]

export
controlEvent : (scene : Var) ->
               (id : String) ->
               Maybe InputEvent ->
               ST m () [scene ::: SScene]
controlEvent scene id Nothing = pure ()
controlEvent scene id (Just input) = case inputToEvent id input of
                                          Nothing => pure ()
                                          Just event => registerEvent scene event

handleEvents : ConsoleIO m =>
               (scene : Var) ->
               List Events.Event ->
               ST m (List Events.Event) [scene ::: SScene]
handleEvents scene [] = pure []
handleEvents scene (x::xs) = handle scene x >>= \_=> handleEvents scene xs where
  handle : ConsoleIO m => (scene : Var) -> Events.Event ->
           ST m (List Events.Event) [scene ::: SScene]
  handle scene (MovementStart direction id) = with ST do
    [idCounter, objects, events, physics] <- split scene
    objectDict <- read objects
    setForce physics id "movement" (case direction of
                                         MoveLeft => (-0.3, 0)
                                         MoveRight => (0.3, 0))
    combine scene [idCounter, objects, events, physics]
    pure [] -- TODO

  handle scene (MovementStop id) = with ST do
    [idCounter, objects, events, physics] <- split scene
    objectDict <- read objects
    setForce physics id "movement" nullVector
    combine scene [idCounter, objects, events, physics]
    pure []

  handle scene (Attack id) = pure []

  handle scene (Jump id) = pure []

iterateEvents : ConsoleIO m => (scene : Var) ->
                ST m (List Events.Event) [scene ::: SScene]
iterateEvents scene = with ST do
  [idCounter, objects, events, physics] <- split scene
  eventList <- read events
  write events (the (List Events.Event) [])
  combine scene [idCounter, objects, events, physics]
  nextEvents <- handleEvents scene eventList
  [idCounter, objects, events, physics] <- split scene
  write events nextEvents
  combine scene [idCounter, objects, events, physics]
  pure nextEvents

export
iterate : ConsoleIO m => (scene : Var) -> (ticks : Int) -> ST m () [scene ::: SScene]
iterate scene ticks = (with ST do
  nextEvents <- iterateEvents scene
  [idCounter, objects, events, physics] <- split scene
  (positionUpdates, collisionEvents) <- iterate physics (0.001 * cast ticks)
  write objects (updatePositions !(read objects) positionUpdates)
  combine scene [idCounter, objects, events, physics]) where
    updatePositions : Dict String Object -> List PositionUpdate -> Dict String Object
    updatePositions x [] = x
    updatePositions x ((id, pos) :: xs)
      = updatePositions (update id (record {position = pos}) x) xs
