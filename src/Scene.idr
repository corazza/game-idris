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
import Descriptors
import Resources
import GameIO



SceneObjects : Type
SceneObjects = Dict String (Object, Body)

SceneEvents : Type
SceneEvents = List Events.Event

updateObject : (id : String) ->
               (f : Object -> Object) ->
               (dict : SceneObjects) ->
               SceneObjects
updateObject id f = update id (\(obj, body) => (f obj, body))


public export
interface Scene (m : Type -> Type) where
  SScene : Type

  startScene : (descriptor : MapDescriptor) -> ST m Var [add SScene]
  endScene : (scene : Var) -> ST m () [remove scene SScene]

  private
  addWithId : (scene : Var) -> (object : Object) -> ST m String [scene ::: SScene]
  private
  addBody : (scene : Var) -> (object : Object) -> ST m Body [scene ::: SScene]
  addObject : (scene : Var) -> (object : Object) -> ST m String [scene ::: SScene]

  create : (scene : Var) -> (creation : Creation) -> ST m (Maybe String) [scene ::: SScene]
  createMany : (scene : Var) -> (creations : List Creation) -> ST m () [scene ::: SScene]

  registerEvent : (scene : Var) -> Events.Event -> ST m () [scene ::: SScene]
  controlEvent : (scene : Var) ->
                 (id : String) ->
                 Maybe InputEvent ->
                 ST m () [scene ::: SScene]

  iterate : (scene : Var) -> (ticks : Int) -> ST m () [scene ::: SScene]

  getObjects : (scene : Var) -> ST m (List Object) [scene ::: SScene]

-- TODO make SceneState record

record PScene where
  constructor MkPScene
  idCounter : Nat
  objects : SceneObjects
  events : SceneEvents


-- WRITEUP here, omitting GameIO causes the SCache not to be found. The error message
-- just referred to "can't find implementation for SCache m ObjectDescriptor"
export
(ConsoleIO m, Box2DPhysics m,  Monad m, GameIO m) => Scene m where
  SScene = Composite [State PScene,
                      SBox2D {m},
                      EmptyContext {m},
                      SCache {m} {r=ObjectDescriptor}]

  startScene (MkMapDescriptor name background creations) = with ST do
    pscene <- new $ MkPScene Z empty []
    physics <- createWorld (0, -8.0)
    objectCache <- initCache {r=ObjectDescriptor}
    emptyContext <- createEmptyContext
    scene <- new ()
    combine scene [pscene, physics, emptyContext, objectCache]
    createMany scene creations
    pure scene

  endScene scene = with ST do
    [pscene, physics, emptyContext, objectCache] <- split scene
    delete pscene
    destroyWorld physics
    quitCache {r=ObjectDescriptor} objectCache
    deleteEmptyContext emptyContext
    delete scene


  getObjects scene = with ST do
    [spscene, physics, emptyContext, objectCache] <- split scene
    pscene <- read spscene
    let objectList = map fst (values (objects pscene))
    combine scene [spscene, physics, emptyContext, objectCache]
    pure objectList

  addBody scene object = with ST do
    [spscene, physics, emptyContext, objectCache] <- split scene
    case (type . physicsProperties) object of
         Wall => do body <- createWall physics (position object) (dim object)
                    combine scene [spscene, physics, emptyContext, objectCache]
                    pure body
         Box => do body <- createBox physics (position object) (dim object) (angle object) 1.0 0.3
                   combine scene [spscene, physics, emptyContext, objectCache]
                   pure body

  addWithId scene object = with ST do
    body <- addBody scene object
    [spscene, physics, emptyContext, objectCache] <- split scene
    pscene <- read spscene
    write spscene (record {objects $= insert (id object) (object, body)} pscene)
    combine scene [spscene, physics, emptyContext, objectCache]
    pure (id object)

  addObject scene (MkObject "" name physicsProperties ctrl render tags) = with ST do
    [spscene, physics, emptyContext, objectCache] <- split scene
    pscene <- read spscene
    let idString = "autoid_" ++ (show (idCounter pscene))
    write spscene (record {idCounter $= (+1)} pscene)
    combine scene [spscene, physics, emptyContext, objectCache]
    addWithId scene (MkObject idString name physicsProperties ctrl render tags)

  addObject scene object = addWithId scene object

  create scene (MkCreation id ref position tags creationData) = (with ST do
    [spscene, physics, emptyContext, objectCache] <- split scene
    Just desc <- get {m} {r=ObjectDescriptor} objectCache emptyContext ref
              | with ST do combine scene [spscene, physics, emptyContext, objectCache]
                           putStrLn $ "couldn't get descriptor of " ++ ref
                           pure Nothing
    putStrLn $ "creating " ++ show desc
    combine scene [spscene, physics, emptyContext, objectCache]

    ?works
    -- let descriptionDim = (dimensions . bodyDescription) desc
    -- case decideDimensions creationDim descriptionDim of
    --      Nothing => with ST do putStrLn $ "wrong dimensions on " ++ ref
    --                            pure Nothing
    --      Just dimensions => with ST do
    --        let mass' = physicsMass (bodyDescription desc)
    --        let type' = (type . bodyDescription) desc
    --        let physicsProperties = MkPhysicsProperties position dimensions 0.0 mass' type'
    --        let object = MkObject (decideId id)
    --                              (name desc)
    --                              physicsProperties
    --                              noControl
    --                              -- tiled objects don't have dimensions specified in their object descriptors,
    --                              -- but in the creation, so the IncompleteRenderDescriptor must be processed
    --                              (renderDescription desc)
    --                              tags
    --        sceneId <- addObject scene object
    --        pure (Just sceneId)) where
            ) where
      decideDimensions : Maybe Vector2D -> Maybe Vector2D -> Maybe Vector2D
      decideDimensions Nothing Nothing = Nothing
      decideDimensions Nothing x = x
      decideDimensions x Nothing = x
      decideDimensions (Just x) (Just y) = Nothing

      decideId : Maybe String -> String
      decideId Nothing = ""
      decideId (Just x) = x

  createMany scene [] = pure () -- TODO return list of id's
  createMany scene (x :: xs) = with ST do create scene x; createMany scene xs


  registerEvent scene event = with ST do
    [spscene, physics, emptyContext, objectCache] <- split scene
    write spscene (record {events $= (event::)} !(read spscene))
    combine scene [spscene, physics, emptyContext, objectCache]

  controlEvent scene id Nothing = pure ()
  controlEvent scene id (Just input) = case inputToEvent id input of
                                            Nothing => pure ()
                                            Just event => registerEvent scene event

  iterate scene ticks = (with ST do
    nextEvents <- iterateEvents scene
    [spscene, physics, emptyContext, objectCache] <- split scene
    pscene <- read spscene
    commitControl physics (values (objects pscene))
    step physics (0.001 * cast ticks) 6 2
    let objects' = !(lift ((traverse updateFromBody) (objects pscene)))
    write spscene (record {objects=objects'} pscene) -- idk why !lift doesn't work in record
    combine scene [spscene, physics, emptyContext, objectCache]) where
      updateFromBody : (Object, Body) -> m (Object, Body)
      updateFromBody (object, body) = do
        newPosition <- getPosition body -- idk why !(getPosition body) doesn't work in record
        newAngle <- getAngle body
        pure (physicsUpdate (record { position = newPosition,
                                        angle = newAngle }) object, body)

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
          [pscene, physics, emptyContext, objectCache] <- split scene
          write pscene (record { objects $=
            updateObject id (record { controlState $= startMoving direction })
          } !(read pscene))
          combine scene [pscene, physics, emptyContext, objectCache]
          pure [] -- TODO

        handle scene (MovementStop id) = with ST do
          [pscene, physics, emptyContext, objectCache] <- split scene
          write pscene (record { objects $=
            updateObject id (record { controlState $= stopMoving })
          } !(read pscene))
          combine scene [pscene, physics, emptyContext, objectCache]
          pure []

        handle scene (Attack id) = pure []

        handle scene (Jump id) = pure []

      iterateEvents : (scene : Var) -> ST m (List Events.Event) [scene ::: SScene {m}]
      iterateEvents scene = with ST do
        [spscene, physics, emptyContext, objectCache] <- split scene
        pscene <- read spscene
        let eventList = events pscene
        write spscene (record {events=[]} pscene)
        combine scene [spscene, physics, emptyContext, objectCache]
        nextEvents <- handleEvents scene eventList
        [spscene, physics, emptyContext, objectCache] <- split scene
        write spscene (record {events=nextEvents} !(read spscene))
        combine scene [spscene, physics, emptyContext, objectCache]
        pure nextEvents
