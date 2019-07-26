module Server.Rules.RuleScript

import Physics.Box2D

import Server.Rules.Behavior
import Server.Rules.RulesOutput
import Server.Rules.RuleEvent
import Server.Rules.NumericProperties
import Descriptions.ObjectDescription.RulesDescription
import Descriptions.ObjectDescription.RulesDescription.BehaviorDescription
import Dynamics.DynamicsEvent
import Commands
import Objects

public export
data RuleScript : Type -> Type where
  Output : RulesOutput -> RuleScript ()
  Transition : (id : ObjectId) -> BehaviorState -> List (RuleScript ()) -> RuleScript ()
  UpdateData : (id : ObjectId) -> (f : BehaviorData -> BehaviorData) -> RuleScript ()

  GetStartTime : (id : ObjectId) -> RuleScript (Maybe Int) -- time since in this state
  GetDirection : (id : ObjectId) -> RuleScript (Maybe BehaviorDirection)
  GetController : (id : ObjectId) -> RuleScript (Maybe BehaviorController)
  GetPosition : (id : ObjectId) -> RuleScript (Maybe Vector2D)

  UpdateNumericProperty : ObjectId ->
                          NumericPropertyId ->
                          (f : NumericProperty -> NumericProperty) ->
                          RuleScript ()
  QueryNumericProperty : ObjectId ->
                         NumericPropertyId ->
                         (f : NumericProperty -> a) ->
                         RuleScript (Maybe a)

  GetStat : (for : ObjectId) -> StatId -> RuleScript (Maybe Double)
  GetCreator : ObjectId -> RuleScript (Maybe ObjectId)

  GetTime : RuleScript Int -- global time

  Log : String -> RuleScript ()

  Pure : (res : a) -> RuleScript a
  (>>=) : RuleScript a -> (a -> RuleScript b) -> RuleScript b

export
Functor RuleScript where
  map f x = do res <- x
               Pure (f res)

export
Applicative RuleScript where
  pure = Pure
  sf <*> sa = do f <- sf
                 a <- sa
                 pure (f a)

export
Monad RuleScript where
  (>>=) = RuleScript.(>>=)

public export
UnitRuleScript : Type
UnitRuleScript = RuleScript ()

stopMovementScript : ObjectId -> RuleScript ()
stopMovementScript id = with RuleScript do
  Output $ RuleCommand $ Stop (Movement Left) id
  Output $ RuleCommand $ Stop (Movement Right) id
  Output $ RuleCommand $ Stop (Movement Up) id
  Output $ RuleCommand $ Stop (Movement Down) id

startMovementScript : ObjectId -> Direction -> RuleScript ()
startMovementScript id direction = with RuleScript do
  Output $ RuleCommand $ Start (Movement direction) id

changeDirectionScript : ObjectId -> RuleScript ()
changeDirectionScript id = with RuleScript do
  Just direction <- GetDirection id | pure ()
  case direction of
    Leftward => startMovementScript id Right
    Rightward => startMovementScript id Left

beginWalkScript : ObjectId -> RuleScript ()
beginWalkScript id
  = Output $ RuleCommand $ Start Walk id

endWalkScript : ObjectId -> RuleScript ()
endWalkScript id
  = Output $ RuleCommand $ Stop Walk id

beginChaseScript : (target : ObjectId) -> (self : ObjectId) -> UnitRuleScript
beginChaseScript target self = UpdateData self (beginChase target)

endChaseScript : ObjectId -> RuleScript ()
endChaseScript id = UpdateData id endChase

  -- Damage : (target : ObjectId) -> (for : Double) -> RuleScript ()
  -- Hit : (attacker : ObjectId) -> (target : ObjectId) -> (for : Double) -> RuleScript ()

runHitAction : (target : ObjectId) ->
               (attacker : ObjectId) ->
               BehaviorAction ->
               UnitRuleScript
runHitAction target attacker MoveLeft = startMovementScript target Left
runHitAction target attacker MoveRight = startMovementScript target Right
runHitAction target attacker Stop = stopMovementScript target
runHitAction target attacker ChangeDirection = changeDirectionScript target
runHitAction target attacker ProjectileDamage = pure ()
runHitAction target attacker Attack = pure ()
runHitAction target attacker BeginChase = beginChaseScript attacker target
runHitAction target attacker EndChase = endChaseScript target
runHitAction target attacker BeginWalk = beginWalkScript target
runHitAction target attacker EndWalk = endWalkScript target

export
hitScript : (target : ObjectId) ->
            (attacker : ObjectId) ->
            (for : Double) ->
            UnitRuleScript
hitScript target attacker controller = with RuleScript do
  Just controller <- GetController target | pure ()
  case currentHandlers controller of
    Nothing => pure () -- TODO FIX UGLY REPETITION
    Just handlers => case onHit handlers of
      Nothing => pure ()
      Just (MkTransition state actions) =>
        let actions = map (runHitAction target attacker) actions
            in Transition target state actions

doDamage : (target : ObjectId) -> (for : Double) -> UnitRuleScript
doDamage target for = with RuleScript do
  UpdateNumericProperty target "health" $ waste for
  Just health <- QueryNumericProperty target "health" $ current | pure ()
  Output $ NumericPropertyCurrent target "health" health
  case health <= 0 of
    False => pure ()
    True => Output $ Death target

doHit : (attacker : ObjectId) ->
        (target : ObjectId) ->
        (for : Double) ->
        UnitRuleScript
doHit attacker target for = with RuleScript do
  doDamage target for
  hitScript target attacker for -- handler

projectileDamage : (projectile_id : ObjectId) -> (target : ObjectId) -> UnitRuleScript
projectileDamage projectile_id target = with RuleScript do
  Just damage <- GetStat projectile_id "damage"
            | Nothing => pure () -- TODO warning
  Just creator <- GetCreator projectile_id
            | Nothing => pure () -- TODO warning
  doHit creator target damage

runCollisionAction : CollisionData -> BehaviorAction -> RuleScript ()
runCollisionAction collision_data MoveLeft
  = startMovementScript (self_id collision_data) Left
runCollisionAction collision_data MoveRight
  = startMovementScript (self_id collision_data) Right
runCollisionAction collision_data Stop
  = stopMovementScript (self_id collision_data)
runCollisionAction collision_data ChangeDirection
  = changeDirectionScript (self_id collision_data)
runCollisionAction collision_data ProjectileDamage
  = projectileDamage (self_id collision_data) (other_id collision_data)
runCollisionAction collision_data Attack = pure ()
runCollisionAction collision_data BeginChase
  = beginChaseScript (other_id collision_data) (self_id collision_data)
runCollisionAction collision_data EndChase = endChaseScript (self_id collision_data)
runCollisionAction collision_data BeginWalk = beginWalkScript (self_id collision_data)
runCollisionAction collision_data EndWalk = endWalkScript (self_id collision_data)

runTimeAction : ObjectId -> BehaviorAction -> UnitRuleScript
runTimeAction id MoveLeft = startMovementScript id Left
runTimeAction id MoveRight = startMovementScript id Right
runTimeAction id Stop = stopMovementScript id
runTimeAction id ChangeDirection = changeDirectionScript id
runTimeAction id ProjectileDamage = pure ()
runTimeAction id Attack = pure ()
runTimeAction id BeginChase = pure ()
runTimeAction id EndChase = endChaseScript id
runTimeAction id BeginWalk = beginWalkScript id
runTimeAction id EndWalk = endWalkScript id

collisionScript : CollisionData -> BehaviorController -> UnitRuleScript
collisionScript collision_data controller
  = case collisionHandler controller of
      Nothing => pure ()
      Just (MkTransition state actions) =>
        let id = self_id collision_data
            actions = map (runCollisionAction collision_data) actions
            in Transition id state actions

collisionScript' : CollisionData -> ObjectId -> UnitRuleScript
collisionScript' collision_data id = with RuleScript do
  Just controller <- GetController id | pure ()
  collisionScript collision_data controller

export
dynamicsEventScript : (event : DynamicsEvent) -> UnitRuleScript
dynamicsEventScript (CollisionStart one two) = with RuleScript do
  let cdata = buildCollisionData one two
  collisionScript' (cdata First) (id one)
  collisionScript' (cdata Second) (id two)
dynamicsEventScript (CollisionStop x y) = pure ()

export
timeScript : (time : Int) -> (id : ObjectId) -> UnitRuleScript
timeScript time id = with RuleScript do
  Just controller <- GetController id | pure ()
  case currentHandlers controller of
    Nothing => pure ()
    Just handlers => case onTime handlers of
      Nothing => pure ()
      Just (duration, time_parameter, MkTransition state actions) => with RuleScript do
        let duration' = getDoubleParameterOrDefault duration time_parameter controller
        Just transitioned <- GetStartTime id | pure ()
        let passed = (cast time) / 1000.0 - (cast transitioned) / 1000.0
        when (passed > duration') $
          let actions = map (runTimeAction id) actions
              in Transition id state actions

export -- get chase_id as second argument
chaseScript : (id : ObjectId) -> UnitRuleScript
chaseScript id = with RuleScript do
  Just controller <- GetController id | pure ()
  case chasing $ behavior_data controller of
    Nothing => pure ()
    Just chase_id => with RuleScript do
      Just chase_position <- GetPosition chase_id | pure ()
      Just my_position <- GetPosition id | pure ()
      if (fst my_position > fst chase_position)
        then startMovementScript id Left
        else startMovementScript id Right

export
mainScript : (time : Int) -> (id : ObjectId) -> UnitRuleScript
mainScript time id = with AIScript do
  timeScript time id
  chaseScript id
