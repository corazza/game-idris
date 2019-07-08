module AI.AIScript

import Events
import Descriptors
import AI.Controller
import Common
import Objects

public export
data AIScript : Type -> Type where
  AICommand : ObjectId -> Command -> AIScript ()
  Transition : (id : ObjectId) -> AIState -> Maybe AIAction -> AIScript ()
  UpdateData : (id : ObjectId) -> (f : AIData -> AIData) -> AIScript ()
  GetStartTime : (id : ObjectId) -> AIScript (Maybe Int) -- time since in this state
  GetDirection : (id : ObjectId) -> AIScript (Maybe AIDirection)

  GetTime : AIScript Int -- global time

  Log : String -> AIScript ()

  Pure : (res : a) -> AIScript a
  (>>=) : AIScript a -> (a -> AIScript b) -> AIScript b

export
Functor AIScript where
  map f x = do res <- x
               Pure (f res)

export
Applicative AIScript where
  pure = Pure
  sf <*> sa = do f <- sf
                 a <- sa
                 pure (f a)

export
Monad AIScript where
  (>>=) = AIScript.(>>=)

public export
UnitAIScript : Type
UnitAIScript = AIScript ()

timeForState : (time : Int) -> (id : ObjectId) -> Handlers -> UnitAIScript
timeForState time id (MkHandlers _ (Just (duration, MkTransition state action))) = with AIScript do
  Just transitioned <- GetStartTime id | pure ()
  let passed = (cast time) / 1000.0 - (cast transitioned) / 1000.0
  when (passed > duration) $
    Transition id state action
timeForState time id _ = pure ()

-- TODO improve matching, decrease repetition
export
timeScripts : (time : Int) -> (id : ObjectId) -> AIController -> UnitAIScript
timeScripts time id controller = case state controller of
  Initial => timeForState time id (initial controller)
  Roam => case roam controller of
    Nothing => pure ()
    Just handlers => timeForState time id handlers
  Chase => case chase controller of
    Nothing => pure ()
    Just handlers => timeForState time id handlers

export
actionToScript : (id : ObjectId) -> AIAction -> UnitAIScript
actionToScript id MoveRight = AICommand id (Start (Movement Right))
actionToScript id MoveLeft = AICommand id (Start (Movement Left))
actionToScript id ChangeDirection = with AIScript do
  Just direction <- GetDirection id | pure ()
  case direction of
    Leftward => AICommand id (Start (Movement Right))
    Rightward => AICommand id (Start (Movement Left))
actionToScript id Attack = ?sdklfsmhm_4
