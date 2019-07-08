module Scene

import Control.ST
import Control.ST.ImplicitCall
import Data.AVL.Dict
import Data.AVL.Set
import Graphics.SDL2

import public Scene.Util
import public Scene.SceneScript
import Data.AVL.DDict
import Common
import Events
import Objects
import Input
import Physics.Box2D
import Physics.Vector2D
import Descriptors
import Resources
import GameIO
import Camera
import Settings
import AI

public export
interface Scene (m : Type -> Type) where
  SScene : Type

  startScene :  (descriptor : MapDescriptor) -> (settings : SceneSettings) -> ST m Var [add SScene]
  endScene : (scene : Var) -> ST m () [remove scene SScene]

  getObject : (scene : Var) -> (id : ObjectId) -> ST m (Maybe Object) [scene ::: SScene]
  getObjects : (scene : Var) -> ST m (List Object) [scene ::: SScene]
  getBody : (scene : Var) -> (id : ObjectId) -> ST m (Maybe Body) [scene ::: SScene]
  getBackground : (scene : Var) -> ST m Background [scene ::: SScene]
  getDimensions : (scene : Var) -> ST m Vector2D [scene ::: SScene]

  create : (scene : Var) -> (creation : Creation) -> ST m (Maybe String) [scene ::: SScene]
  createMany : (scene : Var) -> (creations : List Creation) -> ST m () [scene ::: SScene]
  destroy : (scene : Var) -> (id : ObjectId) -> ST m () [scene ::: SScene]
  destroyMany : (scene : Var) -> (ids : List ObjectId) -> ST m () [scene ::: SScene]

  registerEvent : (scene : Var) -> Events.Event -> ST m () [scene ::: SScene]
  registerEvents : (scene : Var) -> (List Events.Event) -> ST m () [scene ::: SScene]

  handleCommand : (scene : Var) -> ObjectId -> Command -> ST m () [scene ::: SScene]
  handleCommands : (scene : Var) -> ObjectId -> List Command -> ST m () [scene ::: SScene]
  handleCommands' : (scene : Var) -> List (ObjectId, List Command) -> ST m () [scene ::: SScene]

  iterate : (scene : Var) -> (ticks : Int) -> ST m () [scene ::: SScene]

  private
  addBody : (scene : Var) -> (object : Object) -> ST m (Int, Body) [scene ::: SScene]
  private
  addWithId : (scene : Var) -> (object : Object) -> ST m String [scene ::: SScene]
  private
  addObject : (scene : Var) -> (object : Object) -> ST m String [scene ::: SScene]
  private
  addAI : (scene : Var) -> (id : ObjectId) -> (desc : ObjectDescriptor) -> ST m () [scene ::: SScene]
  private
  queryObject : (scene : Var) -> (id : ObjectId) -> (f : Object -> a) -> ST m (Maybe a) [scene ::: SScene]
  private
  updateObject : (scene : Var) -> (id : ObjectId) -> (f : Object -> Object) -> ST m () [scene ::: SScene]
  private
  updateObjects : (scene : Var) -> (f : Object -> Object) -> ST m () [scene ::: SScene]
  private
  applyImpulse : (scene : Var) -> (id : ObjectId) -> (impulse : Vector2D) -> ST m () [scene ::: SScene]

  -- TODO FIX in all objects, but only a minority have AnimationState
  private
  iterateAnimation : (scene : Var) -> Int -> ST m () [scene ::: SScene]

  private
  iterateAI : (scene : Var) -> Int -> ST m () [scene ::: SScene]

  private
  affectPhysics : (scene : Var) -> ST m () [scene ::: SScene]
  private
  pruneOutside : (scene : Var) -> ST m () [scene ::: SScene]
  private
  iteratePhysics : (scene : Var) -> (ticks : Int) -> ST m () [scene ::: SScene]
  private
  physicsIdsToSceneIds : (scene : Var) -> List Int -> ST m (List (Maybe ObjectId)) [scene ::: SScene]
  private
  processPhysicsCollision : (scene : Var) ->
                            (one : CollisionForBody) ->
                            (two : CollisionForBody) ->
                            (xs : List Box2D.Event) ->
                            (cstr : CollisionForObject -> CollisionForObject -> Events.Event) ->
                            ST m (List Events.Event) [scene ::: SScene]
  private
  physicsToEvents : (scene : Var) -> (List Box2D.Event) -> ST m (List Events.Event) [scene ::: SScene]

  private
  handleEvents : (scene : Var) -> ST m () [scene ::: SScene]
  private
  handleEvents' : (scene : Var) -> (List Events.Event) -> ST m () [scene ::: SScene]
  private
  handleCollision : (scene : Var) -> CollisionData -> ST m () [scene ::: SScene]
  private -- handle single event
  handle : (scene : Var) -> Events.Event -> ST m () [scene ::: SScene]

  private
  runScript : (scene : Var) -> (script : SceneScript a) -> ST m a [scene ::: SScene]

  private
  log : String -> STrans m () xs (const xs)

export
(ConsoleIO m, GameIO m) => Scene m where
  SScene = Composite [State PScene, -- in Scene/Util
                      State Box2D.World,
                      SCache {m} {r=ObjectDescriptor},
                      SAI {m}]

  startScene (MkMapDescriptor name background creations dimensions) settings = with ST do
    pscene <- new $ emptyPScene background dimensions
    world <- new !(lift $ createWorld (0, gravity settings))
    objectCache <- initCache {r=ObjectDescriptor}
    scene <- new ()
    ai <- startAI
    combine scene [pscene, world, objectCache, ai]
    createMany scene creations
    pure scene

  endScene scene = with ST do
    [pscene, world, objectCache, ai] <- split scene
    delete pscene
    lift $ destroyWorld !(read world)
    delete world
    quitCache {r=ObjectDescriptor} objectCache
    delete scene
    endAI ai

  getObject scene id = with ST do
    [spscene, physics, objectCache, ai] <- split scene
    pscene <- read spscene
    combine scene [spscene, physics, objectCache, ai]
    pure $ getObject' id (objects pscene)

  getObjects scene = with ST do
    [spscene, physics, objectCache, ai] <- split scene
    pscene <- read spscene
    combine scene [spscene, physics, objectCache, ai]
    pure $ getObjects' (objects pscene)

  getBody scene id = with ST do
    [spscene, physics, objectCache, ai] <- split scene
    pscene <- read spscene
    combine scene [spscene, physics, objectCache, ai]
    pure $ getBody' id (objects pscene)

  getBackground scene = with ST do
    [spscene, physics, objectCache, ai] <- split scene
    let background' = background !(read spscene)
    combine scene [spscene, physics, objectCache, ai]
    pure background'

  -- TODO getPure dimensions
  getDimensions scene = with ST do
    [spscene, physics, objectCache, ai] <- split scene
    let dimensions' = dimensions !(read spscene)
    combine scene [spscene, physics, objectCache, ai]
    pure dimensions'

  create scene creation = with ST do
    let ref = ref creation
    [spscene, physics, objectCache, ai] <- split scene
    Right desc <- get {m} {r=ObjectDescriptor} objectCache ref
                | Left e => with ST do
                      combine scene [spscene, physics, objectCache, ai]
                      putStrLn $ "couldn't get descriptor of " ++ ref ++ ", error: "
                      putStrLn e
                      pure Nothing
    combine scene [spscene, physics, objectCache, ai]
    Just object <- fromDescriptorCreation' desc creation | pure Nothing
    sceneId <- addObject scene object
    addAI scene sceneId desc
    applyImpulse scene sceneId (impulseOnCreation (creationData creation))
    pure (Just sceneId)

  createMany scene [] = pure () -- TODO return list of id's
  createMany scene (x :: xs) = with ST do create scene x; createMany scene xs

  destroy scene id = with ST do
    Just body <- getBody scene id | pure ()
    [pscene, world, objectCache, ai] <- split scene
    lift $ destroy !(read world) body
    write pscene (record { objects $= DDict.delete id } !(read pscene))
    combine scene [pscene, world, objectCache, ai]

  destroyMany scene [] = pure ()
  destroyMany scene (id :: ids) = with ST do destroy scene id; destroyMany scene ids

  registerEvent scene event = with ST do
    [spscene, physics, objectCache, ai] <- split scene
    write spscene (record {events $= (event::)} !(read spscene))
    combine scene [spscene, physics, objectCache, ai]

  registerEvents scene events' = with ST do
    [spscene, physics, objectCache, ai] <- split scene
    write spscene (record {events $= (events'++)} !(read spscene))
    combine scene [spscene, physics, objectCache, ai]

    -- = registerEvents scene (catMaybes $ map (inputToEvent id camera) xs)

  iterate scene ticks = with ST do
    affectPhysics scene
    iteratePhysics scene ticks
    iterateAnimation scene ticks
    iterateAI scene ticks
    handleEvents scene

  addBody scene object = with ST do
    [spscene, physics, objectCache, ai] <- split scene
    (id, body) <- lift $ addBody' !(read physics) object
    combine scene [spscene, physics, objectCache, ai]
    pure (id, body)

  addAI scene id desc = case control desc of
    Nothing => pure ()
    Just x => case ai x of
      Nothing => pure ()
      Just ref => with ST do
        [pscene, physics, objectCache, ai] <- split scene
        addController ai id ref
        combine scene [pscene, physics, objectCache, ai]

  -- PROOF that id isn't empty
  addWithId scene object = with ST do
    (bodyId, body) <- addBody scene object
    mass' <- lift $ getMass body
    let object' = physicsUpdate (record { mass=mass' }) object
    [spscene, physics, objectCache, ai] <- split scene
    update spscene (record { objects    $= addObjectBody (object', body),
                             physicsIds $= insert bodyId (id object') })
    combine scene [spscene, physics, objectCache, ai]
    pure (id object')

  -- TODO FIX
  addObject scene object@(MkObject "" _ _ _ _ _ _ _ _) = with ST do
    [spscene, physics, objectCache, ai] <- split scene
    pscene <- read spscene
    let idString = "autoid_" ++ show (idCounter pscene)
    write spscene (record {idCounter $= (+1)} pscene)
    combine scene [spscene, physics, objectCache, ai]
    addWithId scene (record {id = idString} object)
  addObject scene object = addWithId scene object

  queryObject scene id f = pure $ map f !(getObject scene id)

  updateObject scene id f = with ST do
    [pscene, physics, objectCache, ai] <- split scene
    write pscene (record { objects $= DDict.update id (objectUpdate f) } !(read pscene))
    combine scene [pscene, physics, objectCache, ai]

  updateObjects scene f = with ST do
    [pscene, physics, objectCache, ai] <- split scene
    -- TODO update pscene ...
    write pscene (record { objects $= map (objectUpdate f) } !(read pscene))
    combine scene [pscene, physics, objectCache, ai]

  applyImpulse scene id impulse = with ST do
    Just body <- getBody scene id | pure ()
    [pscene, world, objectCache, ai] <- split scene
    lift $ applyImpulse body impulse
    combine scene [pscene, world, objectCache, ai]

  iterateAnimation scene ticks = updateObjects scene (updateAnimationState ticks)

  iterateAI scene ticks = with ST do
    [pscene, world, objectCache, ai] <- split scene
    aiCommands <- decisions ai ticks
    combine scene [pscene, world, objectCache, ai]
    handleCommands' scene (toList aiCommands)

  affectPhysics scene = with ST do
    [spscene, physics, objectCache, ai] <- split scene
    pscene <- read spscene
    let objectBodys = values (objects pscene)
    lift $ commitControl !(read physics) objectBodys -- in Scene.Util
    lift $ commitEffects !(read physics) objectBodys -- in Scene.Util
    combine scene [spscene, physics, objectCache, ai]

  pruneOutside scene = with ST do
    objects <- getObjects scene
    dimensions <- getDimensions scene
    let outside = getOutside dimensions objects
    destroyMany scene outside

  iteratePhysics scene ticks = with ST do
    [spscene, world, objectCache, ai] <- split scene
    pscene <- read spscene
    physics <- read world
    lift $ step physics (0.001 * cast ticks) 6 2
    let objects' = !(lift ((traverse updateFromBody) (objects pscene)))
    write spscene (record {objects=objects'} pscene) -- idk why !lift doesn't work in record
    physicsEvents <- lift $ pollEvents physics
    combine scene [spscene, world, objectCache, ai]
    registerEvents scene !(physicsToEvents scene physicsEvents)
    updateObjects scene Objects.resetControl
    pruneOutside scene

  physicsIdsToSceneIds scene [] = pure []
  physicsIdsToSceneIds scene (x :: xs) = with ST do
    [spscene, physics, objectCache, ai] <- split scene
    pscene <- read spscene
    combine scene [spscene, physics, objectCache, ai]
    pure $ lookup x (physicsIds pscene) :: !(physicsIdsToSceneIds scene xs)

  processPhysicsCollision scene one two xs cstr =
    case !(physicsIdsToSceneIds scene [id one, id two]) of
      [Just id_one, Just id_two] =>
            let for_one = MkCollisionForObject id_one (velocity one)
                for_two = MkCollisionForObject id_two (velocity two) in pure $
                  cstr for_one for_two :: !(physicsToEvents scene xs)
      _ => physicsToEvents scene xs

  physicsToEvents scene [] = pure []
  physicsToEvents scene ((CollisionStart one two) :: xs) -- (MkCollisionForBody id body velocity)
    = processPhysicsCollision scene one two xs CollisionStart
  physicsToEvents scene ((CollisionStop one two) :: xs)
    = processPhysicsCollision scene one two xs CollisionStop

  handleEvents scene = with ST do
    [spscene, physics, objectCache, ai] <- split scene
    pscene <- read spscene
    let eventList = events pscene
    write spscene (record {events=[]} pscene)
    handleEvents ai eventList
    combine scene [spscene, physics, objectCache, ai]
    handleEvents' scene eventList

  handleEvents' scene [] = pure ()
  handleEvents' scene (x::xs) = handle scene x >>= const (handleEvents' scene xs)

  handleCollision scene cdata@(MkCollisionData self other) = with ST do
    updateObject scene (id self) (addTouching (id other))
    Just scripts' <- queryObject scene (id self) (activeCollisions . scripts) | pure ()
    let scripts = map (\f => f cdata) scripts'
    runScript scene (sequence_ scripts)

  handle scene (CollisionStart one two)
    = let cdata = buildCollisionData one two in with ST do
          handleCollision scene (cdata First)
          handleCollision scene (cdata Second)
  handle scene (CollisionStop one two) = with ST do
    updateObject scene (id one) (removeTouching (id two))
    updateObject scene (id two) (removeTouching (id one))

  handleCommand scene id (Start (Movement direction))
    = updateObject scene id (updateControl $ startMoveAction direction)
  handleCommand scene id (Stop (Movement direction))
    = updateObject scene id (updateControl $ stopMoveAction direction)
  handleCommand scene id (Start (Attack pos))
    = updateObject scene id (updateControl startAttacking)
  handleCommand scene id (Stop (Attack pos)) = with ST do
    updateObject scene id (updateControl stopAttacking)
    Just (Just attack_script) <- queryObject scene id (attack . scripts) | pure ()
    runScript scene (attack_script (MkActionParameters id pos))

  handleCommands scene id [] = pure ()
  handleCommands scene id (command :: xs) = handleCommand scene id command >>=
    const (handleCommands scene id xs)

  handleCommands' scene [] = pure ()
  handleCommands' scene ((id, commands) :: xs) = handleCommands scene id commands >>=
    const (handleCommands' scene xs)

  runScript scene (GetPosition id) = queryObject scene id position
  runScript scene (GetVelocity id) = queryObject scene id velocity
  runScript scene (GetMass id) = queryObject scene id mass
  runScript scene (Create creation) = do create scene creation; pure ()
  runScript scene (Destroy id) = destroy scene id
  runScript scene (Damage x id) = with ST do
    updateObject scene id (takeDamage x)
    Just alive <- queryObject scene id alive | pure ()
    if not alive then destroy scene id else pure ()
  runScript scene (DeactivateCollision name id)
    = updateObject scene id (deactivateCollision name)
  runScript scene (Log what) = log what
  runScript scene (Pure res) = pure res
  runScript scene (x >>= f) = runScript scene x >>= (runScript scene) . f

  log = lift . GameIO.log
