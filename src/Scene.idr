module Scene

import Control.ST
import Data.Vect
import Data.Queue
import Data.AVL.Dict
import Data.AVL.Set
import Graphics.SDL2
import Control.ST.ImplicitCall
import Language.JSON

import Common
import Events
import Objects
import Input
import Physics.Box2D
import Physics.Vector2D
import Descriptors
import Resources
import GameIO
import Script

-- TODO MAJOR mess! cleanup!

SceneObjects : Type
SceneObjects = Dict String (Object, Body)

SceneEvents : Type
SceneEvents = List Events.Event

public export
interface Scene (m : Type -> Type) where
  SScene : Type

  startScene : (descriptor : MapDescriptor) -> ST m Var [add SScene]
  endScene : (scene : Var) -> ST m () [remove scene SScene]

  private
  addWithId : (scene : Var) -> (object : Object) -> ST m String [scene ::: SScene]
  private
  addBody : (scene : Var) -> (object : Object) -> ST m (Int, Body) [scene ::: SScene]
  private
  addObject : (scene : Var) -> (object : Object) -> ST m String [scene ::: SScene]
  private
  updateObject : (scene : Var) -> (id : ObjectId) -> (f : Object -> Object) -> ST m () [scene ::: SScene]
  private
  updateObjects : (scene : Var) -> (f : Object -> Object) -> ST m () [scene ::: SScene]
  private
  queryObject : (scene : Var) -> (id : ObjectId) -> (f : Object -> a) -> ST m (Maybe a) [scene ::: SScene]
  private
  applyImpulse : (scene : Var) -> (id : ObjectId) -> (impulse : Vector2D) -> ST m () [scene ::: SScene]

  private
  runScript : (scene : Var) -> (script : Script a) -> ST m a [scene ::: SScene]

  create : (scene : Var) -> (creation : Creation) -> ST m (Maybe String) [scene ::: SScene]
  createMany : (scene : Var) -> (creations : List Creation) -> ST m () [scene ::: SScene]

  registerEvent : (scene : Var) -> Events.Event -> ST m () [scene ::: SScene]
  registerEvents : (scene : Var) -> (List Events.Event) -> ST m () [scene ::: SScene]
  controlEvent : (scene : Var) ->
                 (id : String) ->
                 (camera : Vector2D) ->
                 List InputEvent ->
                 ST m () [scene ::: SScene]

  getObject : (scene : Var) -> (id : ObjectId) -> ST m (Maybe Object) [scene ::: SScene]
  getBody : (scene : Var) -> (id : ObjectId) -> ST m (Maybe Body) [scene ::: SScene]
  getObjects : (scene : Var) -> ST m (List Object) [scene ::: SScene]
  getBackground : (scene : Var) -> ST m Background [scene ::: SScene]

  -- iteratePhysics : (scene : Var) -> (ticks : Int) -> ST m () [scene ::: SScene]
  iterate : (scene : Var) -> (ticks : Int) -> ST m () [scene ::: SScene]

  physicsIdsToSceneIds : (scene : Var) -> List Int -> ST m (List (Maybe ObjectId)) [scene ::: SScene]

  private
  iteratePhysics : (scene : Var) -> (ticks : Int) -> ST m () [scene ::: SScene]
  -- private
  -- commitControl : (scene : Var) -> ST m () [scene ::: SScene]
  private
  handleEvents : (scene : Var) -> ST m () [scene ::: SScene]
  private
  handleControl : (scene : Var) -> String ->
                  (ControlState -> ControlState) ->
                  ST m () [scene ::: SScene]
  private
  handleCollision : (scene : Var) -> CollisionData -> ST m () [scene ::: SScene]
  private -- handle single event
  handle : (scene : Var) -> Events.Event -> ST m () [scene ::: SScene]
  private
  handleEvents' : (scene : Var) -> (List Events.Event) -> ST m () [scene ::: SScene]
  private
  physicsToEvents : (scene : Var) -> (List Box2D.Event) -> ST m (List Events.Event) [scene ::: SScene]

record PScene where
  constructor MkPScene
  idCounter : Nat
  objects : SceneObjects
  events : SceneEvents
  physicsIds : Dict Int String
  background : Background

-- WRITEUP here, omitting GameIO causes the SCache not to be found. The error message
-- just referred to "can't find implementation for SCache m ObjectDescriptor"
export
(ConsoleIO m, Box2DPhysics m,  Monad m, GameIO m) => Scene m where
  SScene = Composite [State PScene,
                      SBox2D {m},
                      EmptyContext {m},
                      SCache {m} {r=ObjectDescriptor}]

  startScene (MkMapDescriptor name background creations) = with ST do
    pscene <- new $ MkPScene Z empty empty empty background
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

  getObject scene id = with ST do
    [spscene, physics, emptyContext, objectCache] <- split scene
    pscene <- read spscene
    let object = map fst (lookup id (objects pscene))
    combine scene [spscene, physics, emptyContext, objectCache]
    pure object

  getBody scene id = with ST do
    [spscene, physics, emptyContext, objectCache] <- split scene
    pscene <- read spscene
    let body = map snd (lookup id (objects pscene))
    combine scene [spscene, physics, emptyContext, objectCache]
    pure body

  addBody scene object = (with ST do
    [spscene, physics, emptyContext, objectCache] <- split scene
    (id, body) <- addBody' physics object
    combine scene [spscene, physics, emptyContext, objectCache]
    pure (id, body)) where
      addBody' : (physics' : Var) -> Object -> ST m (Int, Body) [physics' ::: SBox2D {m}]
      addBody' physics' object' = case (type . physicsProperties) object of
        Wall => createWall physics' (position object') (dim object')
        Box => createBox physics' (position object') (dim object') (angle object')
                                  (density object') (friction object')

  addWithId scene object = with ST do
    (bodyId, body) <- addBody scene object
    mass' <- lift $ getMass body
    let object' = physicsUpdate (record { mass=mass' }) object
    [spscene, physics, emptyContext, objectCache] <- split scene
    pscene <- read spscene
    write spscene (record { objects    $= insert (id object') (object', body),
                            physicsIds $= insert bodyId (id object') } pscene)
    combine scene [spscene, physics, emptyContext, objectCache]
    pure (id object')

  addObject scene (MkObject "" name physicsProperties ctrl render tags health
                            control scripts)
    = with ST do [spscene, physics, emptyContext, objectCache] <- split scene
                 pscene <- read spscene
                 let idString = "autoid_" ++ (show (idCounter pscene))
                 write spscene (record {idCounter $= (+1)} pscene)
                 combine scene [spscene, physics, emptyContext, objectCache]
                 addWithId scene (MkObject idString name physicsProperties ctrl
                                           render tags health control scripts)

  addObject scene object = addWithId scene object

  queryObject scene id f = pure $ map f !(getObject scene id)

  updateObject scene id f = with ST do
    [pscene, physics, emptyContext, objectCache] <- split scene
    write pscene (record { objects $=
      Dict.update id (\(obj, body) => (f obj, body)) } !(read pscene))
    combine scene [pscene, physics, emptyContext, objectCache]

  updateObjects scene f = with ST do
    [pscene, physics, emptyContext, objectCache] <- split scene
    write pscene (record { objects $= map (\(obj, body) => (f obj, body)) } !(read pscene))
    combine scene [pscene, physics, emptyContext, objectCache]

  applyImpulse scene id impulse = with ST do
    Just body <- getBody scene id | pure ()
    [pscene, physics, emptyContext, objectCache] <- split scene
    applyImpulse physics body impulse
    combine scene [pscene, physics, emptyContext, objectCache]

  runScript scene (GetPosition id) = queryObject scene id position
  runScript scene (GetVelocity id) = queryObject scene id velocity
  runScript scene (GetMass id) = queryObject scene id mass
  runScript scene (Damage x id) = updateObject scene id (takeDamage x)
  runScript scene (Create creation) = do create scene creation; pure ()
  runScript scene (Print what) = putStrLn what
  runScript scene (Pure res) = pure res
  runScript scene (x >>= f) = runScript scene x >>= (runScript scene) . f

  -- HERE need to apply BoxData impulse to creations
  create scene (MkCreation id ref position angle ctags creationData) = (with ST do
    [spscene, physics, emptyContext, objectCache] <- split scene
    Just desc <- get {m} {r=ObjectDescriptor} objectCache emptyContext ref
              | with ST do combine scene [spscene, physics, emptyContext, objectCache]
                           putStrLn $ "couldn't get descriptor of " ++ ref
                           pure Nothing
    putStrLn $ "creating " ++ show desc
    combine scene [spscene, physics, emptyContext, objectCache]
    case decideFallible desc creationData of
      Nothing => ?decideFallibleError
      Just (dimensions, crd) => with ST do
        let physicsProperties = MkPhysicsProperties
          position nullVector dimensions angle
          ((BodyDescriptor.density . bodyDescription) desc)
          ((BodyDescriptor.friction . bodyDescription) desc)
          (-1) -- mass is overwritten on addBody
          ((BodyDescriptor.type . bodyDescription) desc)
          empty
        let scripts = MkScripts (decideAttack (attack desc)) empty
        let object = MkObject
          (decideId id)
          (name desc)
          physicsProperties noControl
          -- tiled objects don't have dimensions specified in their object descriptors,
          -- but in the creation, so the IncompleteRenderDescriptor must be processed
          crd (ctags ++ tags desc) (health desc) (control desc) scripts
        sceneId <- addObject scene object
        creationImpulse scene sceneId creationData
        pure (Just sceneId)) where
      creationImpulse : (scene : Var) -> (id' : ObjectId) -> CreationData -> ST m () [scene ::: SScene {m}]
      creationImpulse x id' (BoxData y) = applyImpulse x id' (getOrDefault nullVector y)
      creationImpulse x id' _ = pure ()

      decideAttack : Maybe ScriptDescriptor -> Maybe (ActionParameters -> UnitScript)
      decideAttack Nothing = Nothing
      -- TODO why is this pattern-match necessary?
      decideAttack (Just (Create x)) = Just $ fromDescriptor (Create x) --Just $ fromDescriptor x

      decideRenderDescription : IncompleteRenderDescriptor ->
                                CreationData ->
                                Maybe CompleteRenderDescriptor
      decideRenderDescription Invisible cdata = Just Invisible
      decideRenderDescription (DrawBox x) (BoxData y) = Just $ DrawBox x
      decideRenderDescription (DrawBox x) (WallData y) = Nothing
      decideRenderDescription (TileWith x y) (BoxData z) = Nothing
      decideRenderDescription (TileWith tileRef (x, y)) (WallData (nx, ny))
        = Just $ TileWith tileRef (x, y) (cast nx, cast ny)

      decideDimensions : IncompleteRenderDescriptor ->
                         CreationData ->
                         BodyDescriptor ->
                         Maybe Vector2D
      decideDimensions (DrawBox x) (BoxData y) (MkBodyDescriptor type density friction dimensions) = dimensions
      decideDimensions (DrawBox x) (WallData y) desc = Nothing
      decideDimensions (TileWith tileRef (x, y)) (BoxData z) desc = Nothing
      decideDimensions (TileWith tileRef (x, y)) (WallData (nx, ny)) desc = Just ((cast nx)*x, (cast ny)*y)
      decideDimensions Invisible (BoxData z) desc = Nothing
      decideDimensions Invisible (WallData (nx, ny)) desc = Nothing
      decideDimensions Invisible (InvisibleWallData dims) desc = Just dims

      -- TODO this should return an Either String (...) with the description of what failed
      decideFallible : ObjectDescriptor ->
                       CreationData ->
                       Maybe (Vector2D, CompleteRenderDescriptor)
      decideFallible desc cdata = let ird = renderDescription desc
                                      bodyDesc = bodyDescription desc in
                                      with Maybe do
        dimensions <- decideDimensions ird cdata bodyDesc | Nothing
        crd <- decideRenderDescription ird cdata | Nothing
        pure (dimensions, crd)

      decideId : Maybe String -> String
      decideId Nothing = ""
      decideId (Just x) = x

  createMany scene [] = pure () -- TODO return list of id's
  createMany scene (x :: xs) = with ST do create scene x; createMany scene xs

  registerEvent scene event = with ST do
    [spscene, physics, emptyContext, objectCache] <- split scene
    write spscene (record {events $= (event::)} !(read spscene))
    combine scene [spscene, physics, emptyContext, objectCache]

  registerEvents scene events' = with ST do
    [spscene, physics, emptyContext, objectCache] <- split scene
    write spscene (record {events $= (events'++)} !(read spscene))
    combine scene [spscene, physics, emptyContext, objectCache]

  controlEvent scene id camera xs
    = registerEvents scene (catMaybes $ map (inputToEvent id camera) xs)

  iterate scene ticks = with ST do
    iteratePhysics scene ticks
    handleEvents scene

  iteratePhysics scene ticks = (with ST do
    [spscene, physics, emptyContext, objectCache] <- split scene
    pscene <- read spscene
    commitControl physics (values (objects pscene))
    step physics (0.001 * cast ticks) 6 2
    let objects' = !(lift ((traverse updateFromBody) (objects pscene)))
    write spscene (record {objects=objects'} pscene) -- idk why !lift doesn't work in record
    physicsEvents <- pollEvents physics
    combine scene [spscene, physics, emptyContext, objectCache]
    registerEvents scene !(physicsToEvents scene physicsEvents)
    updateObjects scene Objects.resetControl) where
      getControl : Object -> STrans m (Maybe ControlDescriptor) xs (const xs)
      getControl = pure . control

      commitControl : (physics : Var) -> List (Object, Body) -> ST m () [physics ::: SBox2D {m}]
      commitControl physics [] = pure ()
      commitControl physics ((obj, body) :: xs) = with ST do
        Just control <- getControl obj | commitControl physics xs
        (x, y) <- lift $ getVelocity body
        mass <- lift $ getMass body
        let ctst = controlState obj
        let x' = case moving ctst of
                      Nothing => 0
                      Just Leftward => - (speed control)
                      Just Rightward => speed control
        let y' = if jumping ctst && canJump ctst && size (touching obj) > 0
                    then jump control
                    else 0
        let impulse = mass `scale` (x'-x, y')
        applyImpulse physics body impulse
        commitControl physics xs

      updateFromBody : (Object, Body) -> m (Object, Body)
      updateFromBody (object, body) = do
        newPosition <- getPosition body -- idk why !(getPosition body) doesn't work in record
        newAngle <- getAngle body
        newVelocity <- getVelocity body
        pure (physicsUpdate (record { position   = newPosition,
                                        angle    = newAngle,
                                        velocity = newVelocity}) object, body)

  handleEvents scene = with ST do
    [spscene, physics, emptyContext, objectCache] <- split scene
    pscene <- read spscene
    let eventList = events pscene
    write spscene (record {events=[]} pscene)
    combine scene [spscene, physics, emptyContext, objectCache]
    handleEvents' scene eventList

  handleEvents' scene [] = pure ()
  handleEvents' scene (x::xs) = handle scene x >>= \_=> handleEvents' scene xs

  handleControl scene id f = with ST do
    updateObject scene id (record { controlState $= f })

  handleCollision scene cdata@(MkCollisionData self other) = with ST do
    updateObject scene self (addTouching other)
    Just scripts' <- queryObject scene self (collisions . scripts) | pure ()
    let scripts = map (\f => f cdata) scripts'
    runScript scene (sequence_ scripts)

  handle scene (MovementStart direction id) = handleControl scene id (startMoving direction)
  handle scene (MovementStop id) = handleControl scene id stopMoving
  handle scene (AttackStart pos id) = handleControl scene id startAttacking
  handle scene (AttackStop pos id) = with ST do
    handleControl scene id stopAttacking
    Just (Just attack_script) <- queryObject scene id (attack . scripts) | pure ()
    runScript scene (attack_script (MkActionParameters id pos (Just 2.5)))
  handle scene (JumpStart id) = handleControl scene id startJumping
  handle scene (JumpStop id) = handleControl scene id stopJumping
  handle scene (CollisionStart id_one id_two)
    = let cdata = buildCollisionData id_one id_two in with ST do
          handleCollision scene (cdata First)
          handleCollision scene (cdata Second)

  handle scene (CollisionStop id_one id_two) = with ST do
    updateObject scene id_one (removeTouching id_two)
    updateObject scene id_two (removeTouching id_one)

  physicsIdsToSceneIds scene [] = pure []
  physicsIdsToSceneIds scene (x :: xs) = with ST do
    [spscene, physics, emptyContext, objectCache] <- split scene
    pscene <- read spscene
    combine scene [spscene, physics, emptyContext, objectCache]
    pure $ lookup x (physicsIds pscene) :: !(physicsIdsToSceneIds scene xs)

  physicsToEvents scene [] = pure []
  physicsToEvents scene ((CollisionStart (b2id_one, body_one) (b2id_two, body_two)) :: xs) =
    case !(physicsIdsToSceneIds scene [b2id_one, b2id_two]) of
      [Just id_one, Just id_two] => pure $ CollisionStart id_one id_two :: !(physicsToEvents scene xs)
      _ => physicsToEvents scene xs
  physicsToEvents scene ((CollisionStop (b2id_one, body_one) (b2id_two, body_two)) :: xs) =
    case !(physicsIdsToSceneIds scene [b2id_one, b2id_two]) of
      [Just id_one, Just id_two] => pure $ CollisionStop id_one id_two :: !(physicsToEvents scene xs)
      _ => physicsToEvents scene xs

  getBackground scene = with ST do
    [spscene, physics, emptyContext, objectCache] <- split scene
    let background' = background !(read spscene)
    combine scene [spscene, physics, emptyContext, objectCache]
    pure background'
