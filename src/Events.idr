module Events

import Control.ST

import Common
import Objects
import Input

-- TODO similar hierarchy as in Inputs
-- (DurationStart / DurationStop) Duration (Movement | Jump | Attack | etc.)
public export
data Event = MovementStart MoveDirection ObjectId
           | MovementStop ObjectId
           | AttackStart ObjectId
           | AttackStop ObjectId
           | JumpStart ObjectId
           | JumpStop ObjectId
           | CollisionStart ObjectId ObjectId
           | CollisionStop ObjectId ObjectId

%name Events.Event event

export
Show Event where
  show (MovementStart direction x) = "MovementStart " ++ show direction ++ " " ++ x
  show (MovementStop x) = "MovementStop " ++ x
  show (AttackStart x) = "AttackStart " ++ x
  show (AttackStop x) = "AttackStop " ++ x
  show (JumpStart x) = "JumpStart " ++ x
  show (JumpStop x) = "JumpStop " ++ x
  show (CollisionStart id_one id_two) = "CollisionStart " ++ id_one ++ " " ++ id_two
  show (CollisionStop id_one id_two) = "CollisionStop " ++ id_one ++ " " ++ id_two


export
inputToEvent : (id : String) -> (event : InputEvent) -> Maybe Event
inputToEvent id (CommandStart (Movement Left)) = Just $ MovementStart Leftward id
inputToEvent id (CommandStart (Movement Right)) = Just $ MovementStart Rightward id
inputToEvent id (CommandStart (Movement Up)) = Just $ JumpStart id
inputToEvent id (CommandStart (Movement Down)) = Nothing
inputToEvent id (CommandStart Attack) = Just $ AttackStart id

inputToEvent id (CommandStop (Movement Left)) = Just $ MovementStop id
inputToEvent id (CommandStop (Movement Right)) = Just $ MovementStop id
inputToEvent id (CommandStop (Movement Up)) = Just $ JumpStop id
inputToEvent id (CommandStop (Movement Down)) = Nothing
inputToEvent id (CommandStop Attack) = Just $ AttackStop id

export
reportEvents : ConsoleIO m => (List Events.Event) -> STrans m () xs (const xs)
reportEvents [] = pure ()
reportEvents xs = printLn xs
