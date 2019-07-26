module Dynamics

import Control.ST
import Control.ST.ImplicitCall
import Physics.Box2D
import Physics.Vector2D

import Dynamics.PDynamics
import Dynamics.DynamicsControl
import Dynamics.DynamicsEvent
import GameIO
import Objects
import Exception
import Settings
-- import Descriptions
import Descriptions.ObjectDescription.BodyDescription

public export
interface Dynamics (m : Type -> Type) where
  SDynamics : Type

  startDynamics : (settings : DynamicsSettings) -> ST m Var [add SDynamics]
  endDynamics : (dynamics : Var) -> ST m () [remove dynamics SDynamics]

  queryPDynamics : (dynamics : Var) -> (q : PDynamics -> a) -> ST m a [dynamics ::: SDynamics]

  addBody : (dynamics : Var) ->
            (id : ObjectId) ->
            (def : BodyDefinition) ->
            (fixtures : List FixtureDefinition) ->
            (control : Maybe ControlParameters) ->
            (effects : List PhysicsEffect) ->
            ST m () [dynamics ::: SDynamics]

  removeBody : (dynamics : Var) -> (id : ObjectId) -> ST m () [dynamics ::: SDynamics]

  applyImpulse : (dynamics : Var) -> (id : ObjectId) -> Vector2D -> ST m () [dynamics ::: SDynamics]

  updateControl : (dynamics : Var) ->
                  (id : ObjectId) ->
                  (f : ControlState -> ControlState) ->
                  ST m () [dynamics ::: SDynamics]

  runCommand : (dynamics : Var) -> (command : DynamicsCommand) -> ST m () [dynamics ::: SDynamics]
  runCommands : (dynamics : Var) -> (commands : List DynamicsCommand) -> ST m () [dynamics ::: SDynamics]

  iterate : (dynamics : Var) -> ST m (List DynamicsEvent) [dynamics ::: SDynamics]

  private
  updateFromBody : (dynamics : Var) -> (ObjectId, BodyData) -> ST m () [dynamics ::: SDynamics]
  private
  updatesFromBody : (dynamics : Var) -> List (ObjectId, BodyData) -> ST m () [dynamics ::: SDynamics]
  private
  applyControl : (dynamics : Var) -> (ObjectId, BodyData) -> ST m () [dynamics ::: SDynamics]
  private
  applyControls : (dynamics : Var) -> List (ObjectId, BodyData) -> ST m () [dynamics ::: SDynamics]
  private
  applyEffect : (dynamics : Var) -> (ObjectId, BodyData) -> PhysicsEffect -> ST m () [dynamics ::: SDynamics]
  private
  applyEffects : (dynamics : Var) -> (ObjectId, BodyData) -> List PhysicsEffect -> ST m () [dynamics ::: SDynamics]
  private
  applyEffectss : (dynamics : Var) -> List (ObjectId, BodyData) -> ST m () [dynamics ::: SDynamics]

  private
  handleEvent : (dynamics : Var) -> (event : DynamicsEvent) -> ST m () [dynamics ::: SDynamics]
  private
  handleEvents : (dynamics : Var) -> (events : List DynamicsEvent) -> ST m () [dynamics ::: SDynamics]

  private
  idExists : (dynamics : Var) -> (id : ObjectId) -> ST m Bool [dynamics ::: SDynamics]
  private
  getWorld : (dynamics : Var) -> ST m Box2D.World [dynamics ::: SDynamics]

export
Dynamics IO where
  SDynamics = State PDynamics

  startDynamics settings = with ST do
    world <- lift $ createWorld $ gravity settings
    pdynamics <- new (pdynamicsInStart world (timeStep settings))
    pure pdynamics

  endDynamics dynamics = with ST do
    dynamics' <- read dynamics
    lift $ destroyWorld (world dynamics')
    delete dynamics

  queryPDynamics dynamics q = read dynamics >>= pure . q

  addBody dynamics id def fixtures control effects = case !(idExists dynamics id) of
    True => pure ()
    False => with ST do
      box2d <- lift $ addBody' !(getWorld dynamics) def fixtures
      let objectControl = map initialControl control
      update dynamics $ pdynamicsAddObject
        id (position def) (fromMaybe 0 $ angle def) box2d objectControl effects
      mass <- lift $ getMass (snd box2d)
      update dynamics $ setMass id mass

  removeBody dynamics id = case getBody id !(read dynamics) of
    Nothing => pure ()
    Just body => with ST do
      update dynamics $ pdynamicsRemoveObject id
      lift $ destroy !(getWorld dynamics) body

  applyImpulse dynamics id impulse = with ST do
    Just body <- queryPDynamics dynamics $ getBody id
    lift $ applyImpulse body impulse

  updateControl dynamics id f = update dynamics $ pdynamicsUpdateControl id f

  runCommand dynamics (Create id def fixtures control effects impulse) = with ST do
    addBody dynamics id def fixtures control effects
    case impulse of
      Nothing => pure ()
      Just x => applyImpulse dynamics id x
  runCommand dynamics (Destroy id) = removeBody dynamics id
  runCommand dynamics (UpdateControl id f) = updateControl dynamics id f

  runCommands dynamics [] = pure ()
  runCommands dynamics (cmd::xs) = runCommand dynamics cmd >>= const (runCommands dynamics xs)

  iterate dynamics = with ST do
    dynamics' <- read dynamics
    let world = world dynamics'
    let bodyDatas = toList $ objects dynamics'
    dt <- queryPDynamics dynamics timeStep
    applyControls dynamics bodyDatas
    applyEffectss dynamics bodyDatas
    lift $ step world (0.001 * cast dt) 6 2
    updatesFromBody dynamics (toList $ objects dynamics')
    events <- lift $ pollEvents world
    let dynamicsEvents = catMaybes $ map (box2DEventToDynamicsEvent dynamics') events
    handleEvents dynamics dynamicsEvents
    pure dynamicsEvents

  updateFromBody dynamics (id, bodyData) = with ST do
    let body = body bodyData
    newPosition <- lift $ getPosition body
    newAngle <- lift $ getAngle body
    newVelocity <- lift $ getVelocity body
    update dynamics $ pdynamicsUpdateObject id (record {
      position = newPosition,
      angle = newAngle,
      velocity = newVelocity
    })

  updatesFromBody dynamics [] = pure ()
  updatesFromBody dynamics (x::xs) = updateFromBody dynamics x >>= const (updatesFromBody dynamics xs)

  applyControl dynamics (id, bodyData) = with ST do
    lift $ applyImpulse (body bodyData) (movementImpulse bodyData)
    updateControl dynamics id resetControlState

  applyControls dynamics [] = pure ()
  applyControls dynamics (x::xs) = applyControl dynamics x >>= const (applyControls dynamics xs)

  -- TODO move to execute effect... = lift $ executeEffect bodyData effect
  applyEffect dynamics (id, bodyData) (Drag factor offset) = with ST do
    let velocity = velocity bodyData
    let norm = norm velocity
    if norm > 0.001
      then with ST do
        let direction = negate $ normed velocity
        let force = (0.5 * factor * norm * norm) `scale` direction
        lift $ applyForce (body bodyData) force offset
      else pure ()

  applyEffects dynamics bodyData [] = pure ()
  applyEffects dynamics bodyData (x::xs)
    = applyEffect dynamics bodyData x >>= const (applyEffects dynamics bodyData xs)

  applyEffectss dynamics [] = pure ()
  applyEffectss dynamics (both@(id, bodyData)::xs)
    = applyEffects dynamics both (effects bodyData) >>= const (applyEffectss dynamics xs)

  handleEvent dynamics (CollisionStart one two) = update dynamics $ touched (id one) (id two)
  handleEvent dynamics (CollisionStop one two) = update dynamics $ untouched (id one) (id two)

  handleEvents dynamics [] = pure ()
  handleEvents dynamics (x::xs) = handleEvent dynamics x >>= const (handleEvents dynamics xs)

  idExists dynamics id = pure $ hasKey id $ objects !(read dynamics)
  getWorld dynamics = pure $ world !(read dynamics)
