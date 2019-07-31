module Descriptions.JointDescription

import Physics.Box2D
import Objects
import Exception
import GameIO

public export
record JointDescription where
  constructor MkJointDescription
  bodyA : ObjectId
  localAnchorA : Vector2D
  bodyB : ObjectId
  localAnchorB : Vector2D
  collideConnected : Maybe Bool

export
Show JointDescription where
  show jd
    = " { bodyA: " ++ bodyA jd
    ++ ", localAnchorA: " ++ show (localAnchorA jd)
    ++ ", bodyB: " ++ bodyB jd
    ++ ", localAnchorB: " ++ show (localAnchorB jd)
    ++ ", collideConnected: " ++ show (collideConnected jd)
    ++ " }"

export
ObjectCaster JointDescription where
  objectCast dict = with Checked do
    type <- getString "type" dict
    case type of
      "revolute" => with Checked do
        bodyA <- getString "bodyA" dict
        bodyB <- getString "bodyB" dict
        localAnchorA <- getVector "localAnchorA" dict
        localAnchorB <- getVector "localAnchorB" dict
        collideConnected <- getBoolMaybe "collideConnected" dict
        pure $ MkJointDescription bodyA localAnchorA bodyB localAnchorB collideConnected
      _ => fail "joint type must be of \"revolute\""
