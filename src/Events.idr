module Events

import Objects
import Input

%access public export

data MoveDirection = MoveLeft | MoveRight

%name MoveDirection direction

data Event = MovementStart MoveDirection String
           | MovementStop String
           | Attack String
           | Jump String

%name Events.Event event

inputToEvent : (id : String) -> (event : InputEvent) -> Maybe Event
inputToEvent id (CommandStart (Movement Left)) = Just $ MovementStart MoveLeft id
inputToEvent id (CommandStart (Movement Right)) = Just $ MovementStart MoveRight id
inputToEvent id (CommandStart (Movement Up)) = Just $ Jump id
inputToEvent id (CommandStart (Movement Down)) = Nothing
inputToEvent id (CommandStart Attack) = Just $ Attack id

inputToEvent id (CommandStop (Movement Left)) = Just $ MovementStop id
inputToEvent id (CommandStop (Movement Right)) = Just $ MovementStop id
inputToEvent id (CommandStop (Movement Up)) = Nothing
inputToEvent id (CommandStop (Movement Down)) = Nothing
inputToEvent id (CommandStop Attack) = Nothing
