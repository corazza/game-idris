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
