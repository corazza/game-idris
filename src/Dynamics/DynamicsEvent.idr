module Dynamics.DynamicsEvent

import Physics.Box2D

import Objects

public export
record CollisionForObject where
  constructor MkCollisionForObject
  id : ObjectId
  velocity : Vector2D
  fixtureName : String

export
Show CollisionForObject where
  show (MkCollisionForObject id velocity fixtureName)
    = id ++ " with velocity " ++ show velocity ++ " (fixture name: " ++ fixtureName ++ ")"

public export
data DynamicsEvent
  = CollisionStart CollisionForObject CollisionForObject
  | CollisionStop CollisionForObject CollisionForObject
  | QueryResult ObjectId ObjectId String -- initiator, target, name

-- this exists only to introduce ordering to collision events (needed by scripts)
public export
record CollisionData where
  constructor MkCollisionData
  self : CollisionForObject
  other : CollisionForObject
%name CollisionData collision_data

export
Show CollisionData where
  show (MkCollisionData self other)
    =  "{ self: " ++ show self
    ++ ", other: " ++ show other
    ++ " }"

public export
data Selector = First | Second

export
buildCollisionData : CollisionForObject -> CollisionForObject -> Selector -> CollisionData
buildCollisionData one two First = MkCollisionData one two
buildCollisionData one two Second = MkCollisionData two one

export
self_id : CollisionData -> ObjectId
self_id (MkCollisionData self other) = id self

export
other_id : CollisionData -> ObjectId
other_id (MkCollisionData self other) = id other

export
self_fixture : CollisionData -> String
self_fixture (MkCollisionData self other) = fixtureName self

export
other_fixture : CollisionData -> String
other_fixture (MkCollisionData self other) = fixtureName other

public export
data AnimationUpdate = MkAnimationUpdate ObjectId String
