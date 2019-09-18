module Server.Rules.Scripts.MovementScripts

import Server.Rules.RuleScript
import Server.Rules.Behavior
import Server.Rules.RulesOutput
import Server.Rules.RulesData
import Server.Rules.Scripts.Basics
import Server.Rules.Scripts.Damage
import Server.Rules.Scripts.MovementScripts
import Client.ClientCommands
import Descriptions.ItemDescription
import Descriptions.AbilityDescription
import Descriptions.ObjectDescription.RulesDescription.BehaviorDescription
import Descriptions.ObjectDescription.BodyFlags
import Dynamics.DynamicsEvent
import Dynamics.BodyData
import Timeline.Items
import Objects
import Commands
import Exception

export
beginChaseScript : (target : ObjectId) -> (self : ObjectId) -> UnitRuleScript
beginChaseScript target self = UpdateBehaviorData self (beginChase target)

export
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

export
doHit : (attacker : ObjectId) ->
        (target : ObjectId) ->
        (for : Double) ->
        (sound : Maybe ContentReference) ->
        UnitRuleScript
doHit attacker target for sound = with RuleScript do
  doDamage attacker target for sound
  UpdateBehaviorData target $ setLastHit attacker
  hitScript target attacker for -- handler

export
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

collisionScriptFilter : CollisionData ->
                        (action : BehaviorAction) ->
                        RuleScript Bool
collisionScriptFilter collision_data action
  = let other = other_id collision_data
        in case condition action of
          Nothing => pure True
          Just Animate => isAnimate other
          Just Inanimate => isInanimate other

export
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

export
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

export
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

export
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
