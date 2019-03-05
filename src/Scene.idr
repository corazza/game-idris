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

public export
interface Scene (m : Type -> Type) where
  SScene : Type

  startScene : Int -> Int -> ST m Var [add SScene]
  endScene : (scene : Var) -> ST m () [remove scene SScene]

  private
  addWithId : (scene : Var) -> (object : Object) -> ST m String [scene ::: SScene]
  addObject : (scene : Var) -> (object : Object) -> ST m String [scene ::: SScene]

  registerEvent : (scene : Var) -> Events.Event -> ST m () [scene ::: SScene]
  controlEvent : (scene : Var) ->
                 (id : String) ->
                 Maybe InputEvent ->
                 ST m () [scene ::: SScene]

  iterate : (scene : Var) -> (ticks : Int) -> ST m () [scene ::: SScene]

  getObjects : (scene : Var) -> ST m (List Object) [scene ::: SScene]

export
(ConsoleIO m, Box2DPhysics m,  Monad m) => Scene m where
  SScene = Composite [State Nat, -- id counter
                      State (Dict String (Object, Body)),
                      State (List Events.Event),
                      SBox2D {m}]

  startScene x y = with ST do
    objects <- new empty
    events <- new []
    idCounter <- new Z
    -- physics <- initPhysics (MkParameters (-10.0) (Just (Floor (-10.0))))
    physics <- createWorld (0, -10.0)
    createGroundBody physics (0, 0) (10, 1)
    scene <- new ()
    combine scene [idCounter, objects, events, physics]
    pure scene

  endScene scene = with ST do
    [idCounter, objects, events, physics] <- split scene
    -- quitPhysics physics
    destroyWorld physics
    delete idCounter; delete objects; delete events
    delete scene

  getObjects scene = with ST do
    [idCounter, objects, events, physics] <- split scene
    let objectList = map fst (values !(read objects))
    combine scene [idCounter, objects, events, physics]
    pure objectList


  addWithId scene object = with ST do
    [idCounter, objects, events, physics] <- split scene
    objectDict <- read objects
    body <- createBox physics (position object) (dim object) 1.0 0.3
    write objects (insert (id object) (object, body) objectDict)
    combine scene [idCounter, objects, events, physics]
    pure (id object)

  addObject scene (MkObject "" position boxDescription texture) = with ST do
    [idCounter, objects, events, physics] <- split scene
    let idNum = !(read idCounter)
    let idString = "autoid_" ++ show idNum
    write idCounter (idNum + 1)
    combine scene [idCounter, objects, events, physics]
    addWithId scene (MkObject idString position boxDescription texture)

  addObject scene object = addWithId scene object


  registerEvent scene event = with ST do
    [idCounter, objects, events, physics] <- split scene
    write events (event :: !(read events))
    combine scene [idCounter, objects, events, physics]

  controlEvent scene id Nothing = pure ()
  controlEvent scene id (Just input) = case inputToEvent id input of
                                            Nothing => pure ()
                                            Just event => registerEvent scene event


  -- -- updateObject (object, body) = ?sdgf
  --
  -- updatePositions = traverse updateObject

  -- updatePositions = updatePositions' [] where
    -- updatePositions' : (acc : List (Object, Body)) ->
    --                    (objects : List (Object, Body)) ->
    --                    STrans m (List (Object, Body)) xs (const xs)
    -- updatePositions' acc [] = pure acc
    -- updatePositions' acc (x :: xs) = ?updatePositions'_rhs_2

  iterate scene ticks = (with ST do
    nextEvents <- iterateEvents scene
    [idCounter, objects, events, physics] <- split scene
    step physics (0.001 * cast ticks) 6 2
    write objects !(lift ( (traverse updateFromBody) (!(read objects))))
    combine scene [idCounter, objects, events, physics]) where
      updateFromBody : (Object, Body) -> m (Object, Body)
      updateFromBody (object, body) = pure
        (record { position = !(getPosition body) } object, body)
      handleEvents : (scene : Var) ->
                     List Events.Event ->
                     ST m (List Events.Event) [scene ::: SScene {m}]
      handleEvents scene [] = pure []
      handleEvents scene (x::xs) = handle scene x >>= \_=> handleEvents scene xs where
        handle : (scene : Var) -> Events.Event ->
                 ST m (List Events.Event) [scene ::: SScene {m}]
        handle scene (MovementStart direction id) = with ST do
          [idCounter, objects, events, physics] <- split scene
          objectDict <- read objects
          -- setForce physics id "movement" (case direction of
          --                                      MoveLeft => (-0.3, 0)
          --                                      MoveRight => (0.3, 0))
          combine scene [idCounter, objects, events, physics]
          pure [] -- TODO

        handle scene (MovementStop id) = with ST do
          [idCounter, objects, events, physics] <- split scene
          objectDict <- read objects
          -- setForce physics id "movement" nullVector
          combine scene [idCounter, objects, events, physics]
          pure []

        handle scene (Attack id) = pure []

        handle scene (Jump id) = pure []

      iterateEvents : (scene : Var) -> ST m (List Events.Event) [scene ::: SScene {m}]
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
