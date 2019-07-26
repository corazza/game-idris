module Dynamics.DynamicsEvent

import Physics.Box2D

import Objects

public export
record CollisionForObject where
  constructor MkCollisionForObject
  id : ObjectId
  velocity : Vector2D

export
Show CollisionForObject where
  show (MkCollisionForObject id velocity) = id ++ " with velocity " ++ show velocity

public export
data DynamicsEvent
  = CollisionStart CollisionForObject CollisionForObject
  | CollisionStop CollisionForObject CollisionForObject

-- this exists only to introduce ordering to collision events (needed by scripts)
public export
record CollisionData where
  constructor MkCollisionData
  self : CollisionForObject
  other : CollisionForObject

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
