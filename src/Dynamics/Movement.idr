module Dynamics.Movement

import Physics.Box2D
import Physics.Vector2D

import Objects
import Dynamics.DynamicsControl
import Dynamics.BodyData

limit : Double
limit = 1.1

offsetAngle : Double -> Double
offsetAngle x = if x > limit then pi/2 else (x/limit) * pi/2

angleCorrection : List (ObjectId, Double) -> Double
angleCorrection [] = 1
angleCorrection ((id, x)::xs) = abs $ cos $ offsetAngle x

jumpAngleCorrection : List (ObjectId, Double) -> Double
jumpAngleCorrection [] = 0
jumpAngleCorrection ((id, x)::xs) = abs $ cos $ offsetAngle x

sign : Double -> Double
sign x = if x < 0 then -1
                  else if x == 0 then 0 else 1

impulseVector : Double -> Vector2D -> Maybe Vector2D
impulseVector mass = Just . scale mass

airMovement : List (ObjectId, Double) ->
              Double ->
              Vector2D ->
              ObjectControl ->
              Maybe Vector2D
airMovement grounding
            mass
            (x, y)
            object_control@(MkObjectControl controlState controlParameters)
  = let airspeed_abs = airspeed object_control
        moveSign' = moveSign controlState
        airspeed' = moveSign' * airspeed_abs
        in if moveSign' == 0 || (moveSign' == sign x && airspeed_abs < abs x)
          then Nothing
          else impulseVector mass (airspeed', 0)

groundMovement : List (ObjectId, Double) ->
                 Double ->
                 Vector2D ->
                 ObjectControl ->
                 Maybe Vector2D
groundMovement grounding
               mass
               (x, y)
               object_control@(MkObjectControl controlState controlParameters)
  = if jumping controlState && canJump controlState
      then let jumpSpeed = jump controlParameters -- * jumpAngleCorrection grounding
               in impulseVector mass $ (0, jumpSpeed)
      else let angle_correction = angleCorrection grounding
               x' = speed object_control * moveSign controlState -- * angle_correction
               x_correction = if abs (x' - x) < 0.01 then 0 else x' - x
               in impulseVector mass (x_correction, 0)

export
movementImpulse : BodyData -> Maybe Vector2D
movementImpulse bodyData = case controls bodyData of
  Nothing => Nothing
  Just object_control@(MkObjectControl controlState controlParameters) =>
    let move_f = if onGround bodyData then groundMovement else airMovement
        in move_f (grounding bodyData) (mass bodyData) (velocity bodyData) object_control
