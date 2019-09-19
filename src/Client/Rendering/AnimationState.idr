module Client.Rendering.AnimationState

import Objects
import GameIO

public export
record AnimationState where
  constructor MkAnimationState
  name : String
  started : Int
  attackShowing : Maybe ContentReference

export
setStarted : Int -> AnimationState -> AnimationState
setStarted ticks = record { started = ticks }

export
setAttackShowing : ContentReference -> AnimationState -> AnimationState
setAttackShowing ref = record { attackShowing = Just ref }

export
unsetAttackShowing : AnimationState -> AnimationState
unsetAttackShowing = record { attackShowing = Nothing }

export
setName : String -> AnimationState -> AnimationState
setName name' = record { name = name' }

export
dummyAnimationState : Int -> AnimationState
dummyAnimationState started = MkAnimationState "resting" started Nothing

export
dummyAnimationStates : Int -> ObjectId -> AnimationState
dummyAnimationStates started _ = dummyAnimationState started

export
dummyAnimationStates' : Int -> ObjectId -> Maybe AnimationState
dummyAnimationStates' started _ = Just $ dummyAnimationState started
