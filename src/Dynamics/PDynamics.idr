module Dynamics.PDynamics

import Physics.Box2D
import Physics.Vector2D
import Data.AVL.Set

import Dynamics.DynamicsControl
import Dynamics.DynamicsEvent
import GameIO
import Objects
import Commands
import Descriptions.MapDescription
import Descriptions.ObjectDescription
import Descriptions.JointDescription
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
  | CreateJoint ObjectId JointDescription
  | Destroy ObjectId
  | UpdateControl ObjectId (ControlState -> ControlState)
  | QueryFor ObjectId Double

export
fromCommand : Command -> Maybe DynamicsCommand
fromCommand (Start (Movement direction) id)
  = Just $ UpdateControl id (startMoveAction direction)
fromCommand (Stop (Movement direction) id)
  = Just $ UpdateControl id (stopMoveAction direction)
fromCommand (Start (Attack x) id) = Just $ UpdateControl id startAttacking
fromCommand (Stop (Attack x) id) = Just $ UpdateControl id stopAttacking
fromCommand (Start Walk id) = Just $ UpdateControl id startWalking
fromCommand (Stop Walk id) = Just $ UpdateControl id stopWalking
fromCommand (Start (Interact x) id) = Nothing
fromCommand (Stop (Interact x) id) = Just $ QueryFor id x

export
filterControl : ObjectId -> List DynamicsCommand -> List DynamicsCommand
filterControl id = filter notSame where
  notSame : DynamicsCommand -> Bool
  notSame (UpdateControl id' f) = id /= id'
  notSame (QueryFor id' _) = id /= id'
  notSame _ = True

export
Show DynamicsCommand where
  show (Create id bodyDef fixtures control effects impulse) = "create " ++ id
  show (Destroy id) = "destroy " ++ id
  show (UpdateControl id f) = "update control of " ++ id
  show (QueryFor id x) = "query for " ++ id ++ ", " ++ show x
  show (CreateJoint id desc) = "create joint " ++ id

export
createWallCommand : WallCreation -> ObjectDescription -> DynamicsCommand
createWallCommand wall_creation object_description
  = let body_description = body object_description
        bodyDef = wallCreationBodyDescriptionToCreation wall_creation body_description
        fixtures = fixtures body_description
        effects = effects body_description
        id = id wall_creation
        in Create id bodyDef fixtures Nothing effects Nothing

applyCatMaskIndex' : BodyDescription -> FixtureDefinition -> FixtureDefinition
applyCatMaskIndex' body_desc fixture_def = record {
    categoryBits = fixturePrecedence (categoryBits body_desc) (categoryBits fixture_def),
    maskBits = fixturePrecedence (maskBits body_desc) (maskBits fixture_def),
    groupIndex = fixturePrecedence (groupIndex body_desc) (groupIndex fixture_def)
  } fixture_def where
      fixturePrecedence : Maybe Int -> Maybe Int -> Maybe Int
      fixturePrecedence a b = case (a, b) of
        (Just a, Just b) => Just b
        (Just a, Nothing) => Just a
        (Nothing, Just b) => Just b
        (Nothing, Nothing) => Nothing

applyCatMaskIndex : BodyDescription -> List FixtureDefinition -> List FixtureDefinition
applyCatMaskIndex desc = map $ applyCatMaskIndex' desc

export
createObjectCommand : Creation -> ObjectDescription -> ObjectId -> DynamicsCommand
createObjectCommand creation object_description id
  = let body_description = body object_description
        bodyDef = creationBodyDescriptionToDefinition creation body_description
        fixtures = applyCatMaskIndex body_description (fixtures body_description)
        effects = effects body_description
        control = map parametersFromDescription $ control object_description
        impulse  = impulse creation
        in Create id bodyDef fixtures control effects impulse

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

export
Show BodyData where
  show bodyData
    =  "{ position: " ++ show (position bodyData)
    ++ ", angle: " ++ show (angle bodyData)
    ++ " }"

export
controlState : BodyData -> Maybe ControlState
controlState = map controlState . controls

export
facing : BodyData -> Maybe MoveDirection
facing = map facing . controlState

export
forceDirection : BodyData -> MoveDirection
forceDirection body_data = case facing body_data of
  Nothing => Rightward
  Just x => x

-- TODO dynamics iterate should receive clock and update a BodyData field
-- movementLastChanged, which is read on render animation

export
animationState : BodyData -> String
animationState object = case controlState object of
  Nothing => "resting"
  Just ctst => case moving ctst of
    [] => "resting"
    _ => if walking ctst then "walking" else "moving"

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
movementImpulse : BodyData -> Maybe Vector2D
movementImpulse bodyData = case controls bodyData of
  Nothing => Nothing
  Just object_control@(MkObjectControl controlState controlParameters) =>
    let (x, y) = velocity bodyData
        x' = speed object_control * moveSign controlState
        y' = if jumping controlState && canJump controlState && onGround bodyData
                  then jump controlParameters else 0
        x_correction = if abs (x' - x) < 0.01 then 0 else x' - x
        impulse = (mass bodyData) `scale` (x_correction, y')
        in if impulse == nullVector then Nothing else Just impulse

public export
record PDynamics where
  constructor MkPDynamics
  world : Box2D.World
  objects : Objects BodyData
  -- joints : Objects Joint
  ids : Ids Int
  timeStep : Int
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
pdynamicsInStart : (world : Box2D.World) -> (timeStep : Int) -> PDynamics
pdynamicsInStart world timeStep = MkPDynamics world emptyObjects emptyIds timeStep

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
setMass : ObjectId -> Double -> PDynamics -> PDynamics
setMass id mass' = pdynamicsUpdateObject id $ record { mass = mass' }

export
pdynamicsUpdateControl : (id : ObjectId) -> (f : ControlState -> ControlState) -> PDynamics -> PDynamics
pdynamicsUpdateControl id f = pdynamicsUpdateObject id (updateControlInBody f)

export
getBodyData : (id : ObjectId) -> PDynamics -> Maybe BodyData
getBodyData id = lookup id . objects

export
getBody : (id : ObjectId) -> PDynamics -> Maybe Body
getBody id = map BodyData.body . lookup id . objects

export
getControlState : (id : ObjectId) -> PDynamics -> Maybe ControlState
getControlState id pdynamics = case lookup id (objects pdynamics) of
  Nothing => Nothing
  (Just x) => case controls x of
    Nothing => Nothing
    (Just x) => Just $ controlState x

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
box2DEventToDynamicsEvent dynamics (CollisionStart one two)
  = collisionConvert (ids dynamics) one two CollisionStart
box2DEventToDynamicsEvent dynamics (CollisionStop one two)
  = collisionConvert (ids dynamics) one two CollisionStop
box2DEventToDynamicsEvent dynamics (QueryResult query_id body_id body)
  = case (lookup query_id (ids dynamics), lookup body_id (ids dynamics)) of
      (Just initiator, Just target) => pure $ Interact initiator target
      (Nothing, Just target) => pure $ QueryResult query_id target
      _ => Nothing

export
jointDescToDef : JointDescription -> PDynamics -> Maybe RevoluteJointDefinition
jointDescToDef desc pdynamics
  = case (getBody (bodyA desc) pdynamics, getBody (bodyB desc) pdynamics) of
      (Just a, Just b) => pure $ MkRevoluteJointDefinition
        a (localAnchorA desc) b (localAnchorB desc) (collideConnected desc)
      _ => Nothing
