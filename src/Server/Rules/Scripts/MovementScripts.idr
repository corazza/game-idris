module Server.Rules.Scripts.MovementScripts

import Physics.Box2D

import Server.Rules.RuleScript
import Server.Rules.Behavior
import Server.Rules.RulesOutput
import Dynamics.MoveDirection
import Dynamics.BodyData
import Objects
import Commands

export
stopMovementScript : ObjectId -> RuleScript ()
stopMovementScript id = with RuleScript do
  Output $ RuleCommand $ Stop (Movement Left) id
  Output $ RuleCommand $ Stop (Movement Right) id
  Output $ RuleCommand $ Stop (Movement Up) id
  Output $ RuleCommand $ Stop (Movement Down) id

export
startMovementScript : ObjectId -> Direction -> RuleScript ()
startMovementScript id direction = with RuleScript do
  Output $ RuleCommand $ Start (Movement direction) id

export
changeDirectionScript : ObjectId -> RuleScript ()
changeDirectionScript id = with RuleScript do
  Just direction <- GetDirection id | pure ()
  case direction of
    Leftward => startMovementScript id Right
    Rightward => startMovementScript id Left

export
beginWalkScript : ObjectId -> RuleScript ()
beginWalkScript id
  = Output $ RuleCommand $ Start Walk id

export
endWalkScript : ObjectId -> RuleScript ()
endWalkScript id
  = Output $ RuleCommand $ Stop Walk id

export
correctFlip : (facing : MoveDirection) -> (from : Vector2D) -> (at : Vector2D) -> Vector2D
correctFlip facing from@(fx, fy) at@(ax, ay) = faceCorrection $ normed (at - from) where
  faceCorrection : Vector2D -> Vector2D
  faceCorrection (x, y) = ((case facing of
    Leftward => if ax - fx > 0 then -x else x
    Rightward => if ax - fx < 0 then -x else x), y)

export
stopAndFace : (id : ObjectId) -> MoveDirection -> UnitRuleScript
stopAndFace id direction = with RuleScript do
  stopMovementScript id
  Output $ SetFacing id direction

export
correctFacing : (id : ObjectId) -> (at : Vector2D) -> UnitRuleScript
correctFacing id (at_x, at_y) = with RuleScript do
  Just (attacker_x, attacker_y) <- GetPosition id | pure ()
  Just facing <- QueryBody id forceDirection | pure ()
  case facing of
    Leftward => case at_x > attacker_x of
      False => pure ()
      True => stopAndFace id Rightward
    Rightward => case at_x < attacker_x of
      False => pure ()
      True => stopAndFace id Leftward

export
correctFacingMove : (id : ObjectId) -> (at : Vector2D) -> UnitRuleScript
correctFacingMove id at = with RuleScript do
  Just (attacker_x, attacker_y) <- GetPosition id | pure ()
  Just facing <- QueryBody id facingFromMove | pure ()
  case facing of
    Nothing => correctFacing id at
    Just x => Output $ SetFacing id x
