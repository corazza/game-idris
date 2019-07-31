module Server.Rules.RuleScript

import Physics.Box2D

import Server.Rules.Behavior
import Server.Rules.RulesOutput
import Server.Rules.RuleEvent
import Server.Rules.NumericProperties
import Descriptions.ObjectDescription.RulesDescription
import Descriptions.ItemDescription
import Descriptions.AbilityDescription
import Descriptions.ObjectDescription.RulesDescription.BehaviorDescription
import Dynamics.DynamicsEvent
import Commands
import GameIO
import Objects
import Exception
import Timeline

public export
data RuleScript : Type -> Type where
  Output : RulesOutput -> RuleScript ()
  Transition : (id : ObjectId) -> BehaviorState -> List (RuleScript ()) -> RuleScript ()
  UpdateData : (id : ObjectId) -> (f : BehaviorData -> BehaviorData) -> RuleScript ()
  GetItemDescription : (ref : ContentReference) -> RuleScript (Checked ItemDescription)
  GetAttack : (id : ObjectId) -> RuleScript (Maybe AbilityDescription)
  Ability : (id : ObjectId) -> (at : Vector2D) -> (desc : AbilityDescription) -> RuleScript ()
  GetCharacter : (id : ObjectId) -> RuleScript (Maybe Character)
  UpdateCharacter : (id : ObjectId) -> (f : Character -> Character) -> RuleScript ()

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
runCollisionAction collision_data Door = pure ()
runCollisionAction collision_data Loot = pure ()

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
runTimeAction id Door = pure ()
runTimeAction id Loot = pure ()

collisionScript : CollisionData -> ObjectId -> UnitRuleScript
collisionScript collision_data id = with RuleScript do
  Just controller <- GetController id | pure ()
  case collisionHandler controller of
      Nothing => pure ()
      Just (MkTransition state actions) =>
        let id = self_id collision_data
            actions = map (runCollisionAction collision_data) actions
            in Transition id state actions

lootScript : (looter : ObjectId) ->
             (drop : ObjectId) ->
             (item : ContentReference) ->
             UnitRuleScript
lootScript looter drop item = with RuleScript do
  Just character <- GetCharacter looter | pure ()
  Just drop_controller <- GetController drop | pure ()
  Right item_desc <- GetItemDescription item
        | Left e => Log ("couldn't get item " ++ item ++ ", error: " ++ e)
  UpdateCharacter looter $ loot item

-- TODO chaining (moves are universal, so they go first)
runInteractAction : (interact_string : String) ->
                    (initiator : ObjectId) ->
                    (target : ObjectId) ->
                    BehaviorAction ->
                    UnitRuleScript
runInteractAction interact_string initiator target MoveLeft = startMovementScript target Left
runInteractAction interact_string initiator target MoveRight = startMovementScript target Right
runInteractAction interact_string initiator target Stop = stopMovementScript target
runInteractAction interact_string initiator target ChangeDirection = changeDirectionScript target
runInteractAction interact_string initiator target ProjectileDamage = pure ()
runInteractAction interact_string initiator target Attack = pure ()
runInteractAction interact_string initiator target BeginChase = pure ()
runInteractAction interact_string initiator target EndChase = endChaseScript target
runInteractAction interact_string initiator target BeginWalk = beginWalkScript target
runInteractAction interact_string initiator target EndWalk = endWalkScript target
runInteractAction interact_string initiator target Door
  = Output $ ExitTo initiator interact_string
runInteractAction interact_string initiator target Loot = lootScript initiator target interact_string

interactScript : (initiator : ObjectId) -> (target : ObjectId) -> UnitRuleScript
interactScript initiator target = with RuleScript do
  Just target_controller <- GetController target | pure ()
  case interactHandler target_controller of
    Nothing => pure ()
    Just (interact_string, MkTransition state actions) => with RuleScript do
      case getStringParameter interact_string target_controller of
        Nothing => pure ()
        Just x => let actions = map (runInteractAction x initiator target) actions
                      in Transition target state actions

export
dynamicsEventScript : (event : DynamicsEvent) -> UnitRuleScript
dynamicsEventScript (CollisionStart one two) = with RuleScript do
  let cdata = buildCollisionData one two
  collisionScript (cdata First) (id one)
  collisionScript (cdata Second) (id two)
dynamicsEventScript (CollisionStop x y) = pure ()
dynamicsEventScript (QueryResult query_id object_id)
  = Log $ "query with id " ++ show query_id ++ " positive for " ++ object_id
dynamicsEventScript (Interact initiator target)
  = interactScript initiator target

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
garbageScript : (id : ObjectId) -> UnitRuleScript
garbageScript id = with RuleScript do
  Just controller <- GetController id | pure ()
  case halted controller of
    False => pure ()
    True => Output $ Death id

export
mainScript : (time : Int) -> (id : ObjectId) -> UnitRuleScript
mainScript time id = with AIScript do
  timeScript time id
  chaseScript id
  garbageScript id

export
attackScript : (id : ObjectId) -> (at : Vector2D) -> UnitRuleScript
attackScript id at = case !(GetCharacter id) of
  Just character => case attackItem character of
    Nothing => pure ()
    Just ref => with RuleScript do
      Right attack_item <- GetItemDescription ref | Left e =>
            Log ("couldn't get item description " ++ show (attackItem character) ++ " (attackScript)")
      case equip attack_item of
        Nothing => pure ()
        Just (MkEquipDescription slot ability) => Ability id at ability
  Nothing => with RuleScript do
    Just attack <- GetAttack id | pure ()
    Ability id at attack
