module AI.AIScript

import Events
import Descriptors
import AI.Controller
import Common
import Objects
import Physics.Vector2D

public export
data AIScript : Type -> Type where
  AICommand : (id : ObjectId) -> Command -> AIScript ()
  Transition : (id : ObjectId) -> AIState -> List AIAction -> AIScript ()
  UpdateData : (id : ObjectId) -> (f : AIData -> AIData) -> AIScript ()
  GetStartTime : (id : ObjectId) -> AIScript (Maybe Int) -- time since in this state
  GetDirection : (id : ObjectId) -> AIScript (Maybe AIDirection)
  GetController : (id : ObjectId) -> AIScript (Maybe AIController)

  SetLastHit : (id : ObjectId) -> (target_id : ObjectId) -> AIScript ()
  GetLastHit : (id : ObjectId) -> AIScript (Maybe ObjectId)
  GetPosition : (id : ObjectId) -> AIScript (Maybe Vector2D)

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

runGenericHandler : (id : ObjectId) ->
                    (controller : AIController) ->
                    (getHandler : AIController -> Maybe Transition) ->
                    UnitAIScript
runGenericHandler id controller getHandler = case getHandler controller of
  Nothing => pure ()
  Just (MkTransition state actions) => Transition id state actions

collisionScript : CollisionData -> AIController -> UnitAIScript
collisionScript (MkCollisionData self other) controller
  = runGenericHandler (id self) controller collisionHandler

export
eventScript : (event : Events.Event) -> UnitAIScript
eventScript (CollisionStart one two) = with AIScript do
  Just controller_one <- GetController (id one) | pure ()
  Just controller_two <- GetController (id two) | pure ()
  let cdata = buildCollisionData one two
  collisionScript (cdata First) controller_one
  collisionScript (cdata Second) controller_two
eventScript (CollisionStop x y) = pure ()
eventScript (Hit attacker target damage) = with AIScript do
  Just controller <- GetController target | pure ()
  SetLastHit target attacker -- implicit action: target last hit
  runGenericHandler target controller hitHandler

export
timeScript : (time : Int) -> (id : ObjectId) -> AIController -> UnitAIScript
timeScript time id controller = case currentHandlers controller of
  Nothing => pure ()
  Just handlers => case onTime handlers of
    Nothing => pure ()
    Just (duration, time_parameter, MkTransition state actions) => with AIScript do
      let duration' = getDoubleParameterOrDefault duration time_parameter controller
      Just transitioned <- GetStartTime id | pure ()
      let passed = (cast time) / 1000.0 - (cast transitioned) / 1000.0
      when (passed > duration') $
        Transition id state actions

export -- get chase_id as second argument
chaseScript : (id : ObjectId) -> AIController -> UnitAIScript
chaseScript id controller = case chasing $ aidata controller of
  Nothing => pure ()
  Just chase_id => with AIScript do
    Just chase_position <- GetPosition chase_id | pure ()
    Just my_position <- GetPosition id | pure ()
    if (fst my_position > fst chase_position)
      then AICommand id $ Start $ Movement Left
      else AICommand id $ Start $ Movement Right

export
mainScript : (time : Int) -> (id : ObjectId) -> AIController -> UnitAIScript
mainScript time id controller = with AIScript do
  timeScript time id controller
  chaseScript id controller

export
actionToScript : (id : ObjectId) -> AIAction -> UnitAIScript
actionToScript id MoveRight = AICommand id (Start (Movement Right))
actionToScript id MoveLeft = AICommand id (Start (Movement Left))
actionToScript id ChangeDirection = with AIScript do
  Just direction <- GetDirection id | pure ()
  case direction of
    Leftward => AICommand id (Start (Movement Right))
    Rightward => AICommand id (Start (Movement Left))
actionToScript id Attack = pure ()
actionToScript id Stop = with AIScript do -- TODO we have direction
  AICommand id (Stop (Movement Right))
  AICommand id (Stop (Movement Left))
  AICommand id (Stop (Movement Up))
  AICommand id (Stop (Movement Down))
actionToScript id BeginChase = with AIScript do
  Just hitter <- GetLastHit id | pure ()
  UpdateData id (beginChase hitter)
actionToScript id EndChase = UpdateData id endChase
actionToScript id BeginWalk = AICommand id (Start Walk)
actionToScript id EndWalk = AICommand id (Stop Walk)

export
actionsToScript : (id : ObjectId) -> List AIAction -> UnitAIScript
actionsToScript id xs = sequence_ (map (actionToScript id) xs)
