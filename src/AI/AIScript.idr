module AI.AIScript

import Events
import Descriptors
import AI.Controller
import Common
import Objects

public export
data AIScript : Type -> Type where
  Event : Events.Event -> AIScript ()

  UpdateData : (id : ObjectId) -> (f : AIData -> AIData) -> AIScript ()
  GetTime : (id : ObjectId) -> AIScript Int

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

export
leftrightScript : (id : String) -> (duration : Double) -> UnitAIScript
leftrightScript id duration = Event (MovementStart Leftward id)

export
produceScript : (id : ObjectId) -> AIController -> UnitAIScript
produceScript id controller = case state controller of
  Leftright => case leftright controller of
    Nothing => Pure ()
    Just (duration, handlers) => leftrightScript id duration
  Chase => ?dfkkdee_2
  Attack => ?dfkkdee_3
