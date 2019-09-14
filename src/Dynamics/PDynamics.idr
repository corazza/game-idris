module Dynamics.PDynamics

import Physics.Box2D
import Physics.Vector2D
import Data.AVL.Set

import Dynamics.DynamicsCommand
import Dynamics.DynamicsControl
import Dynamics.DynamicsEvent
import Dynamics.BodyData
import GameIO
import Objects
import Descriptions.MapDescription
import Descriptions.ObjectDescription
import Descriptions.JointDescription
import Descriptions.ObjectDescription.BodyDescription

public export
record PDynamics where
  constructor MkPDynamics
  world : Box2D.World
  objects : Objects BodyData
  ids : Ids Int
  timeStep : Int
  animationUpdates : List AnimationUpdate
%name PDynamics dynamics

export
flushAnimationUpdates : PDynamics -> PDynamics
flushAnimationUpdates = record { animationUpdates = empty }

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
pdynamicsInStart world timeStep
  = MkPDynamics world emptyObjects emptyIds timeStep empty

export
addBody' : (GameIO m, Monad m) =>
           (world : Box2D.World) ->
           (def : BodyDefinition) ->
           (fixtures : List FixtureDefinition) ->
           m (Int, Body)
addBody' world def fixtures = with m do
  (id, body) <- createBody world def
  traverse (createFixture world body) fixtures
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
  = let body_data = MkBodyData position angle nullVector 0 box2d_id body control
                               empty empty effects
        in record { objects $= addObject id body_data, ids $= addId box2d_id id }

export
pdynamicsRemoveObject : (id : ObjectId) -> PDynamics -> PDynamics
pdynamicsRemoveObject id dynamics = case lookup id (objects dynamics) of
  Nothing => dynamics
  Just body_data => record {
    objects $= removeObject id,
    ids $= removeId (box2d_id body_data) } dynamics

export
pdynamicsUpdateObject : (id : ObjectId) ->
                        (f : BodyData -> BodyData) ->
                        PDynamics ->
                        PDynamics
pdynamicsUpdateObject id f = record { objects $= updateObject id f }

export
pdynamicsQueryObject : (id : ObjectId) ->
                       (q : BodyData -> a) ->
                       PDynamics ->
                       Maybe a
pdynamicsQueryObject id q = map q . lookup id . objects

export
setMass : ObjectId -> Double -> PDynamics -> PDynamics
setMass id mass' = pdynamicsUpdateObject id $ record { mass = mass' }

export
pdynamicsUpdateControl : (id : ObjectId) ->
                         (f : ControlState -> ControlState) ->
                         PDynamics ->
                         PDynamics
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
      (MkCollisionForObject objectId_one (velocity one) (fixtureName one))
      (MkCollisionForObject objectId_two (velocity two) (fixtureName two))
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

export
pdynamicsAddAnimationUpdate : AnimationUpdate -> PDynamics -> PDynamics
pdynamicsAddAnimationUpdate update = record { animationUpdates $= append update }
