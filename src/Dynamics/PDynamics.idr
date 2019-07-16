module Dynamics.PDynamics

import Physics.Box2D
import Physics.Vector2D
import Data.AVL.Set

import Dynamics.Control
import Dynamics.DynamicsEvent
import GameIO
import Objects
import Descriptions
import Descriptions.ObjectDescription.BodyDescription

-- indirect way of calling methods because I don't know how to pass the dynamics
-- Var to the Client and Server directly
public export
data DynamicsCommand
  = Create ObjectId
           BodyDefinition
           (List FixtureDefinition)
           (Maybe ControlParameters)
           (List PhysicsEffect)
           (Maybe Vector2D) -- impulse
  | Destroy ObjectId
  | UpdateControl ObjectId (ControlState -> ControlState)

export
Show DynamicsCommand where
  show (Create id bodyDef fixtures control effects impulse) = "create " ++ id
    -- =    "create ( "
    -- ++   "id = " ++ id
    -- ++ ", bodyDef = " ++ "can't show" -- show bodyDef
    -- ++ ", fixtures = " ++ show fixtures
    -- ++ ", control = " ++ show control
    -- ++ ", effects = " ++ show effects
    -- ++ ", impulse = " ++ show impulse
    -- ++ " )"
  show (Destroy id) = "destroy " ++ id
  show (UpdateControl id f) = "update control of " ++ id

export
createWallCommand : WallCreation -> ObjectDescription -> DynamicsCommand
createWallCommand wall_creation object_description
  = let body_description = body object_description
        bodyDef = wallCreationBodyDescriptionToCreation wall_creation body_description
        fixtures = fixtures body_description
        effects = effects body_description
        id = id wall_creation
        in Create id bodyDef fixtures Nothing effects Nothing

export
createObjectCommand : Creation -> ObjectDescription -> ObjectId -> DynamicsCommand
createObjectCommand creation object_description id
  = let body_description = body object_description
        bodyDef = creationBodyDescriptionToDefinition creation body_description
        fixtures = fixtures body_description
        effects = effects body_description
        control = map parametersFromDescription $ control object_description
        in Create id bodyDef fixtures control effects Nothing

public export
record BodyData where
  constructor MkBodyData
  position : Vector2D -- set on first iterate
  angle : Double
  velocity : Vector2D
  mass : Double
  box2d_id : Int
  body : Body
  controls : Maybe ObjectControl
  touching : Set ObjectId
  effects : List PhysicsEffect
%name BodyData body_data

addTouching : (id : ObjectId) -> BodyData -> BodyData
addTouching id = record { touching $= insert id }

removeTouching : (id : ObjectId) -> BodyData -> BodyData
removeTouching id = let to_remove = insert id empty in
  record { touching $= flip difference to_remove }

updateControlInBody : (f : ControlState -> ControlState) -> BodyData -> BodyData
updateControlInBody f = record { controls $= map (promoteToObjectControl f) }

onGround : BodyData -> Bool
onGround bodyData = size (touching bodyData) > 0

export
movementImpulse : BodyData -> Vector2D
movementImpulse bodyData = case controls bodyData of
  Nothing => nullVector
  Just (MkObjectControl controlState controlParameters) =>
    let (x, y) = velocity bodyData
        x' = speed controlParameters * moveSign controlState
        y' = if jumping controlState && canJump controlState && onGround bodyData
                  then jump controlParameters else 0
        x_correction = if abs (x' - x) < 0.01 then 0 else x' - x
        in (mass bodyData) `scale` (x_correction, y')

public export
record PDynamics where
  constructor MkPDynamics
  world : Box2D.World
  objects : Objects BodyData
  ids : Ids Int
%name PDynamics dynamics

pairUpdate : (f : ObjectId -> BodyData -> BodyData) -> ObjectId -> ObjectId -> PDynamics -> PDynamics
pairUpdate f one two = record { objects $= updateObject one (f two) . updateObject two (f one) }

export
touched : (one : ObjectId) -> (two : ObjectId) -> PDynamics -> PDynamics
touched = pairUpdate addTouching

export
untouched : (one : ObjectId) -> (two : ObjectId) -> PDynamics -> PDynamics
untouched = pairUpdate removeTouching

export
pdynamicsFromWorld : Box2D.World -> PDynamics
pdynamicsFromWorld world = MkPDynamics world emptyObjects emptyIds

export
addBody' : (GameIO m, Monad m) =>
           (world : Box2D.World) ->
           (def : BodyDefinition) ->
           (fixtures : List FixtureDefinition) ->
           m (Int, Body)
addBody' world def fixtures = with m do
  (id, body) <- createBody world def
  traverse (createFixture body) fixtures
  pure (id, body)

export
pdynamicsAddObject : (id : ObjectId) ->
                     (position : Vector2D) ->
                     (angle : Double) ->
                     (box2d : (Int, Body)) ->
                     (control : Maybe ObjectControl) ->
                     (effects : List PhysicsEffect) ->
                     PDynamics -> PDynamics
pdynamicsAddObject id position angle (box2d_id, body) control effects
  = let body_data = MkBodyData position angle nullVector 0 box2d_id body control empty effects
        in record { objects $= addObject id body_data, ids $= addId box2d_id id }

export
pdynamicsRemoveObject : (id : ObjectId) -> PDynamics -> PDynamics
pdynamicsRemoveObject id dynamics = case lookup id (objects dynamics) of
  Nothing => dynamics
  Just body_data => record {
    objects $= removeObject id,
    ids $= removeId (box2d_id body_data) } dynamics

export
pdynamicsUpdateObject : (id : ObjectId) -> (f : BodyData -> BodyData) -> PDynamics -> PDynamics
pdynamicsUpdateObject id f = record { objects $= updateObject id f }

export
pdynamicsUpdateControl : (id : ObjectId) -> (f : ControlState -> ControlState) -> PDynamics -> PDynamics
pdynamicsUpdateControl id f = pdynamicsUpdateObject id (updateControlInBody f)

export
getBody : (id : ObjectId) -> PDynamics -> Maybe Body
getBody id = map BodyData.body . lookup id . objects

collisionConvert : (ids : Ids Int) ->
                   (one : CollisionForBody) ->
                   (two : CollisionForBody) ->
                   (cstr : CollisionForObject -> CollisionForObject -> DynamicsEvent) ->
                   Maybe DynamicsEvent
collisionConvert ids one two cstr = case (lookup (id one) ids, lookup (id two) ids) of
    (Just objectId_one, Just objectId_two) => Just $ cstr
      (MkCollisionForObject objectId_one (velocity one)) (MkCollisionForObject objectId_two (velocity two))
    _ => Nothing

export
box2DEventToDynamicsEvent : PDynamics -> Box2D.Event -> Maybe DynamicsEvent
box2DEventToDynamicsEvent dynamics (CollisionStart one two) = collisionConvert (ids dynamics) one two CollisionStart
box2DEventToDynamicsEvent dynamics (CollisionStop one two) = collisionConvert (ids dynamics) one two CollisionStop
