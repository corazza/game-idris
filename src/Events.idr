module Events

import Control.ST

import Common
import Objects
import Input
import Physics.Vector2D

-- TODO similar hierarchy as in Inputs
-- (DurationStart / DurationStop) Duration (Movement | Jump | Attack | etc.)
public export
data Event = MovementStart MoveDirection ObjectId
           | MovementStop ObjectId
           | AttackStart Vector2D ObjectId
           | AttackStop Vector2D ObjectId
           | JumpStart ObjectId
           | JumpStop ObjectId
           | CollisionStart CollisionForObject CollisionForObject
           | CollisionStop CollisionForObject CollisionForObject

%name Events.Event event

export
Show Event where
  show (MovementStart direction x) = "MovementStart " ++ show direction ++ " " ++ x
  show (MovementStop x) = "MovementStop " ++ x
  show (AttackStart pos id) = "AttackStart " ++ id ++ " at " ++ show pos
  show (AttackStop pos id) = "AttackStop " ++ id ++ " at " ++ show pos
  show (JumpStart x) = "JumpStart " ++ x
  show (JumpStop x) = "JumpStop " ++ x
  show (CollisionStart for_one for_two) = "CollisionStart " ++ (id for_one) ++ " " ++ (id for_two)
  show (CollisionStop for_one for_two) = "CollisionStop " ++ (id for_one) ++ " " ++ (id for_two)


export
inputToEvent : (id : String) -> Vector2D -> (event : InputEvent) -> Maybe Event
inputToEvent id _ (CommandStart (Movement Left)) = Just $ MovementStart Leftward id
inputToEvent id _ (CommandStart (Movement Right)) = Just $ MovementStart Rightward id
inputToEvent id _ (CommandStart (Movement Up)) = Just $ JumpStart id
inputToEvent id _ (CommandStart (Movement Down)) = Nothing
inputToEvent id camera (CommandStart (Attack x y)) = let scenePos = screenToPosition camera (x, y) in
 Just $ AttackStart scenePos id

inputToEvent id _ (CommandStop (Movement Left)) = Just $ MovementStop id
inputToEvent id _ (CommandStop (Movement Right)) = Just $ MovementStop id
inputToEvent id _ (CommandStop (Movement Up)) = Just $ JumpStop id
inputToEvent id _ (CommandStop (Movement Down)) = Nothing
inputToEvent id camera (CommandStop (Attack x y)) = let scenePos = screenToPosition camera (x, y) in
  Just $ AttackStop scenePos id

export
reportEvents : ConsoleIO m => (List Events.Event) -> STrans m () xs (const xs)
reportEvents [] = pure ()
reportEvents xs = printLn xs
