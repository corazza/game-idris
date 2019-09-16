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

lowCutoff : Double
lowCutoff = 0.01

highCutoff : Double
highCutoff = 5

impulseVector : Double -> Vector2D -> Maybe Vector2D
impulseVector mass = Just . scale mass

impulseVector' : Double -> Vector2D -> Maybe Vector2D
impulseVector' mass vec
  = let norm' = norm vec
        in if norm' < lowCutoff
              then Nothing
              else if norm' > highCutoff
                then let vec' = (highCutoff / norm') `scale` vec
                         in impulseVector mass vec'
                else impulseVector mass vec

airMovement : Double ->
              Vector2D ->
              ObjectControl ->
              Maybe Vector2D
airMovement mass
            (x, y)
            object_control@(MkObjectControl controlState controlParameters)
  = let airspeed_abs = airspeed object_control
        moveSign' = moveSign controlState
        airspeed' = moveSign' * airspeed_abs
        in if moveSign' == 0 || (moveSign' == sign x && airspeed_abs < abs x)
          then Nothing
          else impulseVector mass (airspeed', 0)

applySigns : Double -> Double -> Vector2D -> Vector2D
applySigns actual wanted (x, y) = actual * wanted `scale` (x, y)

continueMovement : Double -> Vector2D -> ObjectControl -> Maybe Vector2D
continueMovement mass
                 velocity@(x, y)
                 object_control@(MkObjectControl controlState controlParameters)
  = let moveSign' = moveSign controlState
        speed' = speed object_control
        in if velocity == nullVector
            then impulseVector' mass (speed' * moveSign', 0)
            else if moveSign' /= 0.0
              then impulseVector' mass (speed' * moveSign' - x, 0)
              -- then let velocity'@(x', y') = speed' `scale` normed velocity
              --          diff = velocity' - velocity
              --          in impulseVector' mass $ applySigns (sign x) moveSign' diff
              else impulseVector' mass $ 0.2 `scale` negate (fst velocity, 0)

groundMovement : Double ->
                 Vector2D ->
                 ObjectControl ->
                 Maybe Vector2D
groundMovement mass
               velocity@(x, y)
               object_control@(MkObjectControl controlState controlParameters)
  = if jumping controlState && canJump controlState
      then impulseVector' mass (0, jump controlParameters)
      else continueMovement mass velocity object_control


export
movementImpulse : BodyData -> Maybe Vector2D
movementImpulse bodyData = case controls bodyData of
  Nothing => Nothing
  Just object_control => (if onGround bodyData then groundMovement else airMovement)
      (mass bodyData) (velocity bodyData) object_control
