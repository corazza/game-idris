module Server.Rules.RuleScript

import Physics.Box2D

import Server.Rules.Behavior
import Server.Rules.RulesOutput
import Server.Rules.RuleEvent
import Server.Rules.NumericProperties
import Server.Rules.RulesData
import Client.ClientCommands
import Descriptions.ObjectDescription.RulesDescription
import Descriptions.ObjectDescription.RenderDescription
import Descriptions.ObjectDescription.BodyFlags
import Descriptions.ObjectDescription.RulesDescription.BehaviorDescription
import Descriptions.ItemDescription
import Descriptions.MapDescription
import Descriptions.AbilityDescription
import Dynamics.DynamicsEvent
import Dynamics.BodyData
import Dynamics.MoveDirection
import Commands
import GameIO
import Objects
import Exception
import Timeline
import Timeline.Items

public export
data RuleScript : Type -> Type where
  Output : RulesOutput -> RuleScript ()
  Transition : (id : ObjectId) -> BehaviorState -> List (RuleScript ()) -> RuleScript ()
  UpdateBehaviorData : (id : ObjectId) -> (f : BehaviorData -> BehaviorData) -> RuleScript ()
  QueryBehaviorData : (id : ObjectId) -> (q : BehaviorData -> a) -> RuleScript (Maybe a)
  RulesClientCommand : (id : ObjectId) -> ClientCommand -> RuleScript ()

  GetItemDescription : (ref : ContentReference) -> RuleScript (Checked ItemDescription)
  GetAttack : (id : ObjectId) -> RuleScript (Maybe ContentReference)
  GetBody : (id : ObjectId) -> RuleScript (Maybe BodyData)
  QueryBody : (id : ObjectId) -> (q : BodyData -> a) -> RuleScript (Maybe a)

  GetStartTime : (id : ObjectId) -> RuleScript (Maybe Int) -- time since in this state
  GetDirection : (id : ObjectId) -> RuleScript (Maybe BehaviorDirection)
  GetController : (id : ObjectId) -> RuleScript (Maybe BehaviorController)
  GetPosition : (id : ObjectId) -> RuleScript (Maybe Vector2D)

  QueryData : (id : ObjectId) -> (q : RulesData -> a) -> RuleScript (Maybe a)
  UpdateData : (id : ObjectId) -> (f : RulesData -> RulesData) -> RuleScript ()

  QueryItems : (id : ObjectId) -> (q : Items -> a) -> RuleScript (Maybe a)
  UpdateItems : (id : ObjectId) -> (f : Items -> Items) -> RuleScript ()

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
beginChaseScript target self = UpdateBehaviorData self (beginChase target)

endChaseScript : ObjectId -> RuleScript ()
endChaseScript id = UpdateBehaviorData id endChase

runHitAction : (target : ObjectId) ->
               (attacker : ObjectId) ->
               BehaviorEffect ->
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
runHitAction target attacker (SetMaskBits xs) = Output $ SetMaskBits target xs
runHitAction target attacker (UnsetMaskBits xs) = Output $ UnsetMaskBits target xs
runHitAction target attacker (PlaySound ref) = Output $ PlaySound ref
runHitAction target attacker (PlaySoundParameter param) = with RuleScript do
  Just (Just ref) <- QueryData target $ dataGetStringParameter param | pure ()
  Output $ PlaySound ref

scriptFilter : (f : a -> RuleScript Bool) -> List a -> RuleScript (List a)
scriptFilter f [] = pure []
scriptFilter f (x :: xs) = case !(f x) of
  False => scriptFilter f xs
  True => pure $ x :: !(scriptFilter f xs)

isAnimate : ObjectId -> RuleScript Bool
isAnimate id = case !(QueryData id rulesType) of
  Nothing => pure False
  Just Animate => pure True
  Just Inanimate => pure False

isInanimate : ObjectId -> RuleScript Bool
isInanimate id = case !(QueryData id rulesType) of
  Nothing => pure False
  Just Animate => pure False
  Just Inanimate => pure True

hitScriptFilter : (target : ObjectId) ->
                  (attacker : ObjectId) ->
                  (for : Double) ->
                  (action : BehaviorAction) ->
                  RuleScript Bool
hitScriptFilter target attacker for action = case condition action of
  Nothing => pure True
  Just Animate => isAnimate target
  Just Inanimate => isInanimate target

export
hitScript : (target : ObjectId) ->
            (attacker : ObjectId) ->
            (for : Double) ->
            UnitRuleScript
hitScript target attacker for = with RuleScript do
  Just controller <- GetController target | pure ()
  case currentHandlers controller of
    Nothing => pure () -- TODO FIX UGLY REPETITION
    Just handlers => case onHit handlers of
      Nothing => pure ()
      Just (MkTransition state actions) => with RuleScript do
        filtered <- scriptFilter (hitScriptFilter target attacker for) actions
        let scripts = map (runHitAction target attacker) (map effect filtered)
        Transition target state scripts

playConditional : (id : ObjectId) -> (sound : Maybe ContentReference) -> UnitRuleScript
playConditional id Nothing = pure ()
playConditional id ref = UpdateData id $ setMeleeSound ref

doDamage : (attacker : ObjectId) ->
           (target : ObjectId) ->
           (for : Double) ->
           (sound : Maybe ContentReference) ->
           UnitRuleScript
doDamage attacker target for sound = with RuleScript do
  UpdateNumericProperty target "health" $ waste for
  Just health <- QueryNumericProperty target "health" $ current | pure ()
  Output $ NumericPropertyCurrent target "health" health
  playConditional attacker sound
  case health <= 0 of
    False => pure ()
    True => Output $ Death target

doHit : (attacker : ObjectId) ->
        (target : ObjectId) ->
        (for : Double) ->
        (sound : Maybe ContentReference) ->
        UnitRuleScript
doHit attacker target for sound = with RuleScript do
  doDamage attacker target for sound
  UpdateBehaviorData target $ setLastHit attacker
  hitScript target attacker for -- handler

projectileDamage : CollisionData -> UnitRuleScript
projectileDamage collision_data
  = case self_fixture collision_data == "projectile head" of
      True => let projectile_id = self_id collision_data
                  target = other_id collision_data
                  in with RuleScript do
                    Just damage <- GetStat projectile_id "damage"
                                | Nothing => pure () -- TODO warning
                    Just creator <- GetCreator projectile_id
                                 | Nothing => pure () -- TODO warning
                    doHit creator target damage Nothing
      False => pure ()

runCollisionAction : CollisionData -> BehaviorEffect -> RuleScript ()
runCollisionAction collision_data MoveLeft
  = startMovementScript (self_id collision_data) Left
runCollisionAction collision_data MoveRight
  = startMovementScript (self_id collision_data) Right
runCollisionAction collision_data Stop
  = stopMovementScript (self_id collision_data)
runCollisionAction collision_data ChangeDirection
  = changeDirectionScript (self_id collision_data)
runCollisionAction collision_data ProjectileDamage = projectileDamage collision_data
runCollisionAction collision_data Attack = pure ()
runCollisionAction collision_data BeginChase
  = beginChaseScript (other_id collision_data) (self_id collision_data)
runCollisionAction collision_data EndChase = endChaseScript (self_id collision_data)
runCollisionAction collision_data BeginWalk = beginWalkScript (self_id collision_data)
runCollisionAction collision_data EndWalk = endWalkScript (self_id collision_data)
runCollisionAction collision_data Door = pure ()
runCollisionAction collision_data Loot = pure ()
runCollisionAction collision_data (SetMaskBits xs)
  = Output $ SetMaskBits (self_id collision_data) xs
runCollisionAction collision_data (UnsetMaskBits xs)
  = Output $ UnsetMaskBits (self_id collision_data) xs
runCollisionAction collision_data (PlaySound ref) = Output $ PlaySound ref
runCollisionAction collision_data (PlaySoundParameter param) = with RuleScript do
  Just (Just ref) <- QueryData (self_id collision_data) $
      dataGetStringParameter param | pure ()
  Output $ PlaySound ref

runTimeAction : ObjectId -> BehaviorEffect -> UnitRuleScript
runTimeAction id MoveLeft = startMovementScript id Left
runTimeAction id MoveRight = startMovementScript id Right
runTimeAction id Stop = stopMovementScript id
runTimeAction id ChangeDirection = changeDirectionScript id
runTimeAction id ProjectileDamage = pure ()
runTimeAction id Attack = pure ()
runTimeAction id BeginChase = with RuleScript do
  Just (Just last_hit) <- QueryBehaviorData id lastHit | pure ()
  beginChaseScript last_hit id
runTimeAction id EndChase = endChaseScript id
runTimeAction id BeginWalk = beginWalkScript id
runTimeAction id EndWalk = endWalkScript id
runTimeAction id Door = pure ()
runTimeAction id Loot = pure ()
runTimeAction id (SetMaskBits xs) = Output $ SetMaskBits id xs
runTimeAction id (UnsetMaskBits xs) = Output $ UnsetMaskBits id xs
runTimeAction id (PlaySound ref) = Output $ PlaySound ref
runTimeAction id (PlaySoundParameter param) = with RuleScript do
  Just (Just ref) <- QueryData id $ dataGetStringParameter param | pure ()
  Output $ PlaySound ref

collisionScriptFilter : CollisionData ->
                        (action : BehaviorAction) ->
                        RuleScript Bool
collisionScriptFilter collision_data action
  = let other = other_id collision_data
        in case condition action of
          Nothing => pure True
          Just Animate => isAnimate other
          Just Inanimate => isInanimate other

collisionScript : CollisionData -> ObjectId -> UnitRuleScript
collisionScript collision_data id = with RuleScript do
  Just controller <- GetController id | pure ()
  case collisionHandler controller of
      Nothing => pure ()
      Just (MkTransition state actions) => with RuleScript do
        filtered <- scriptFilter (collisionScriptFilter collision_data) actions
        let scripts = map (runCollisionAction collision_data) (map effect filtered)
        let id = self_id collision_data
        Transition id state scripts

lootScript : (looter : ObjectId) ->
             (item : ContentReference) ->
             UnitRuleScript
lootScript looter item = with RuleScript do
  UpdateItems looter $ loot item
  RulesClientCommand looter RefreshInventory

-- TODO chaining (moves are universal, so they go first)
runInteractAction : (interact_string : String) ->
                    (initiator : ObjectId) ->
                    (target : ObjectId) ->
                    BehaviorEffect ->
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
runInteractAction interact_string initiator target Loot = lootScript initiator interact_string
runInteractAction interact_string initiator target (SetMaskBits xs)
  = Output $ SetMaskBits target xs
runInteractAction interact_string initiator target (UnsetMaskBits xs)
  = Output $ UnsetMaskBits target xs
runInteractAction interact_string initiator target (PlaySound ref)
  = Output $ PlaySound ref
runInteractAction interact_string initiator target (PlaySoundParameter param)
  = with RuleScript do
      Just (Just ref) <- QueryData target $ dataGetStringParameter param | pure ()
      Output $ PlaySound ref

interactScriptFilter : (initiator : ObjectId) ->
                       (target : ObjectId) ->
                       (action : BehaviorAction) ->
                       RuleScript Bool
interactScriptFilter initiator target action
  = case condition action of
          Nothing => pure True
          Just Animate => isAnimate target
          Just Inanimate => isInanimate target

interactScript : (initiator : ObjectId) -> (target : ObjectId) -> UnitRuleScript
interactScript initiator target = with RuleScript do
  Just target_controller <- GetController target | pure ()
  case interactHandler target_controller of
    Nothing => pure ()
    Just (interact_string, MkTransition state actions) => with RuleScript do
      case getStringParameter interact_string target_controller of
        Nothing => pure ()
        Just x => with RuleScript do
          filtered <- scriptFilter (interactScriptFilter initiator target) actions
          let scripts = map (runInteractAction x initiator target) (map effect filtered)
          Transition target state scripts

chooseMeleeHitSound : (target : ObjectId) -> RuleScript ContentReference
chooseMeleeHitSound target = case !(QueryData target rulesType) of
  Just Animate => pure "main/sounds/sword/gash.wav"
  Just Inanimate => pure "main/sounds/melee/hit_inanimate.wav"
  _ => pure "main/sounds/melee/hit_inanimate.wav"

meleeScript : (initiator : ObjectId) -> (target : ObjectId) -> UnitRuleScript
meleeScript initiator target = case !(GetAttack initiator) of
  Nothing => pure ()
  Just ref => with RuleScript do
    Right attack_item <- GetItemDescription ref | Left e =>
      Log ("couldn't get item description (attackScript), error:\n" ++ e)
    case !(QueryBody target (drop . flags)) of
      Just False => case equip attack_item of
        Just (MkEquipDescription Hands (Just ability) offset) =>
          case effect ability of
            Melee range damage => with RuleScript do
              Just initiator_position <- GetPosition initiator | pure ()
              Just target_position <- GetPosition target | pure ()
              let impulse_direction = normed (target_position - initiator_position)
              case !(QueryBody initiator (sameDirection impulse_direction)) of
                Just True => with RuleScript do
                  doHit initiator target damage $ Just !(chooseMeleeHitSound target)
                  Output $ ApplyImpulse target (damage `scale` impulse_direction)
                _ => pure ()
            _ => pure ()
        _ => pure ()
      _ => pure ()

export
dynamicsEventScript : (event : DynamicsEvent) -> UnitRuleScript
dynamicsEventScript (CollisionStart one two) = with RuleScript do
  let cdata = buildCollisionData one two
  collisionScript (cdata First) (id one)
  collisionScript (cdata Second) (id two)
dynamicsEventScript (CollisionStop x y) = pure ()
dynamicsEventScript (QueryResult initiator target "interact")
  = interactScript initiator target
dynamicsEventScript (QueryResult initiator target "melee")
  = if initiator /= target then meleeScript initiator target else pure ()
dynamicsEventScript (QueryResult initiator target name)
  = Log $ initiator ++ " queried " ++ target ++ " (" ++ name ++ ")"

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
          let actions = map (runTimeAction id) (map effect actions)
              in Transition id state actions

chaseOffsetLength : Double
chaseOffsetLength = 2.5

export -- get chase_id as second argument
chaseScript : (id : ObjectId) -> UnitRuleScript
chaseScript id = with RuleScript do
  Just controller <- GetController id | pure ()
  case chasing $ behavior_data controller of
    Nothing => pure ()
    Just chase_id => with RuleScript do
      Just target_position <- GetPosition chase_id | pure ()
      Just my_position <- GetPosition id | pure ()
      let diff = fst target_position - fst my_position
      if abs diff < chaseOffsetLength
        then stopMovementScript id -- stop chase
        else if diff > 0
          then startMovementScript id Right
          else startMovementScript id Left

export
garbageScript : (id : ObjectId) -> UnitRuleScript
garbageScript id = with RuleScript do
  Just controller <- GetController id | pure ()
  case halted controller of
    False => pure ()
    True => Output $ Death id

meleeSoundScript : (id : ObjectId) -> UnitRuleScript
meleeSoundScript id = case !(QueryData id meleeSound) of
  Just (Just ref) => with RuleScript do
    Output $ PlaySound ref
    UpdateData id $ setMeleeSound Nothing
  _ => pure ()

export
mainScript : (time : Int) -> (id : ObjectId) -> UnitRuleScript
mainScript time id = with RuleScript do
  timeScript time id
  chaseScript id
  garbageScript id
  meleeSoundScript id

correctFlip : (facing : MoveDirection) -> (from : Vector2D) -> (at : Vector2D) -> Vector2D
correctFlip facing from@(fx, fy) at@(ax, ay) = faceCorrection $ normed (at - from) where
  faceCorrection : Vector2D -> Vector2D
  faceCorrection (x, y) = ((case facing of
    Leftward => if ax - fx > 0 then -x else x
    Rightward => if ax - fx < 0 then -x else x), y)

throwImpulseAngleFrom : (thrower : BodyData) ->
                        (at : Vector2D) ->
                        (strength : Double) ->
                        (Vector2D, Double, Vector2D)
throwImpulseAngleFrom thrower at strength =
  let thrower_position = position thrower
      direction = correctFlip (forceDirection thrower) thrower_position at
      from = thrower_position + (2 `scale` direction)
      impulse = strength `scale` direction
      angle' = angle direction - pi/2.0
      in (impulse, angle', from)

abilityEffectScript : (id : ObjectId) ->
                      (at : Vector2D) ->
                      (desc : AbilityEffect) ->
                      UnitRuleScript
abilityEffectScript id at (Throw ref impulse) = with RuleScript do
  Just body <- GetBody id | pure ()
  let (impulse', angle, from) = throwImpulseAngleFrom body at impulse
  let creation = MkCreation ref from (Just impulse') (Just id) (Just angle)
                            Nothing Nothing Nothing
  Output $ Create creation
abilityEffectScript id at (Melee range damage) = Output $ RunQuery id "melee" range

stopAndFace : (id : ObjectId) -> MoveDirection -> UnitRuleScript
stopAndFace id direction = with RuleScript do
  stopMovementScript id
  Output $ SetFacing id direction

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

correctFacingMove : (id : ObjectId) -> (at : Vector2D) -> UnitRuleScript
correctFacingMove id at = with RuleScript do
  Just (attacker_x, attacker_y) <- GetPosition id | pure ()
  Just facing <- QueryBody id facingFromMove | pure ()
  case facing of
    Nothing => correctFacing id at
    Just x => Output $ SetFacing id x

getAttack : (id : ObjectId) -> RuleScript (Maybe ContentReference, Maybe AbilityDescription)
getAttack id =  case !(GetAttack id) of
  Nothing => pure (Nothing, Nothing)
  Just ref => with RuleScript do
    Right attack_item <- GetItemDescription ref | Left e => with RuleScript do
        Log ("couldn't get item description (getAttackAbility), error:\n" ++ e)
        pure (Nothing, Nothing)
    case equip attack_item of
      Just (MkEquipDescription slot ability offset) => pure (Just ref, ability)
      _ => pure (Just ref, Nothing)

playConditional' : Maybe ContentReference -> UnitRuleScript
playConditional' Nothing = pure ()
playConditional' (Just ref) = Output $ PlaySound ref

export
beginAttackScript : (id : ObjectId) -> (at : Vector2D) -> UnitRuleScript
beginAttackScript id at = with RuleScript do
  correctFacing id at
  case !(getAttack id) of
    (Just ref, Just ability) => with RuleScript do
      Output $ SetAttackShowing id ref
      playConditional' $ preSound ability
    _ => pure ()

export
endAttackScript : (id : ObjectId) -> (at : Vector2D) -> UnitRuleScript
endAttackScript id at = with RuleScript do
  correctFacingMove id at
  case !(getAttack id) of
    (Just ref, Just ability) => with RuleScript do
      Output $ UnsetAttackShowing id
      abilityEffectScript id at $ effect ability
      playConditional' $ postSound ability
    _ => pure ()

clearSlot : (equipper : ObjectId) -> EquipSlot -> UnitRuleScript
clearSlot equipper slot = with RuleScript do
  Just (Just item_ref) <- QueryItems equipper (getAtSlot slot) | pure ()
  UpdateItems equipper $ resetSlot slot
  lootScript equipper item_ref

export
equipScript : (equipper : ObjectId) ->
              (item : ContentReference) ->
              UnitRuleScript
equipScript equipper item = case !(QueryItems equipper (hasItem item)) of
  Just True => with RuleScript do
    Right item_desc <- GetItemDescription item
          | Left e => Log ("couldn't get item " ++ item ++ ", error: " ++ e)
    case equip item_desc of
      Nothing => pure ()
      Just equip_desc => with RuleScript do
        let slot' = slot equip_desc
        clearSlot equipper slot'
        UpdateItems equipper $ equip item slot'
        UpdateItems equipper $ removeItem item
        RulesClientCommand equipper RefreshInventory
  _ => pure ()

export
unequipScript : (equipper : ObjectId) ->
                (item : ContentReference) ->
                UnitRuleScript
unequipScript equipper item = case !(QueryItems equipper (hasEquipped item)) of
  Just (Just slot) => clearSlot equipper slot
  _ => pure ()
