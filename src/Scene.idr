module Scene

import Control.ST
import Data.Vect
import Data.Queue
import Data.AVL.Dict
import Graphics.SDL2
import Control.ST.ImplicitCall
import Language.JSON

import Events
import Objects
import Input
import Physics.Box2D
import Physics.Vector2D


SceneObjects : Type
SceneObjects = Dict String (Object, Body)

updateObject : (id : String) ->
               (f : Object -> Object) ->
               (dict : SceneObjects) ->
               SceneObjects
updateObject id f = update id (\(obj, body) => (f obj, body))


public export
interface Scene (m : Type -> Type) where
  SScene : Type

  startScene : (descriptor : JSON) -> ST m Var [add SScene]
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
                      State SceneObjects,
                      State (List Events.Event),
                      SBox2D {m}]

  startScene descriptor = with ST do
    printLn descriptor
    objects <- new empty
    events <- new []
    idCounter <- new Z
    physics <- createWorld (0, -8.0)
    createGroundBody physics (0, 0) (10, 1)
    scene <- new ()
    combine scene [idCounter, objects, events, physics]
    pure scene

  endScene scene = with ST do
    [idCounter, objects, events, physics] <- split scene
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
    body <- createBox physics (position object) (dim object) (angle object) 1.0 0.3
    write objects (insert (id object) (object, body) objectDict)
    combine scene [idCounter, objects, events, physics]
    pure (id object)

  addObject scene (MkObject "" position angle boxDescription texture ctst) = with ST do
    [idCounter, objects, events, physics] <- split scene
    let idNum = !(read idCounter)
    let idString = "autoid_" ++ show idNum
    write idCounter (idNum + 1)
    combine scene [idCounter, objects, events, physics]
    addWithId scene (MkObject idString position angle boxDescription texture ctst)

  addObject scene object = addWithId scene object


  registerEvent scene event = with ST do
    [idCounter, objects, events, physics] <- split scene
    write events (event :: !(read events))
    combine scene [idCounter, objects, events, physics]

  controlEvent scene id Nothing = pure ()
  controlEvent scene id (Just input) = case inputToEvent id input of
                                            Nothing => pure ()
                                            Just event => registerEvent scene event


  iterate scene ticks = (with ST do
    nextEvents <- iterateEvents scene
    [idCounter, sobjects, events, physics] <- split scene
    objects <- read sobjects
    commitControl physics (values objects)
    step physics (0.001 * cast ticks) 6 2
    write sobjects !(lift ((traverse updateFromBody) objects))
    combine scene [idCounter, sobjects, events, physics]) where
      updateFromBody : (Object, Body) -> m (Object, Body)
      updateFromBody (object, body) = pure
        (record { position = !(getPosition body),
                  angle = !(getAngle body) } object, body)

      commitControl : (physics : Var) -> List (Object, Body) -> ST m () [physics ::: SBox2D {m}]
      commitControl physics [] = pure ()
      commitControl physics ((obj, body) :: xs) = with ST do
        (x, y) <- lift $ getVelocity body
        mass <- lift $ getMass body
        let x' = case moving (controlState obj) of
                      Nothing => 0
                      Just Leftward => -1.5
                      Just Rightward => 1.5
        let impulse = mass `scale` (x'-x, 0)
        applyImpulse physics body impulse

      handleEvents : (scene : Var) ->
                     List Events.Event ->
                     ST m (List Events.Event) [scene ::: SScene {m}]
      handleEvents scene [] = pure []
      handleEvents scene (x::xs) = handle scene x >>= \_=> handleEvents scene xs where
        handle : (scene : Var) -> Events.Event ->
                 ST m (List Events.Event) [scene ::: SScene {m}]
        handle scene (MovementStart direction id) = with ST do
          [idCounter, objects, events, physics] <- split scene
          write objects (updateObject id (record {
            controlState $= startMoving direction }) !(read objects))
          combine scene [idCounter, objects, events, physics]
          pure [] -- TODO

        handle scene (MovementStop id) = with ST do
          [idCounter, objects, events, physics] <- split scene
          write objects (updateObject id (record {
            controlState $= stopMoving }) !(read objects))
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
