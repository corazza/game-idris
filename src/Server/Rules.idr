module Server.Rules

import Control.ST
import Control.ST.ImplicitCall
import Physics.Box2D

import Server.Rules.PRules
import Server.Rules.NumericProperties
import Server.Rules.RulesOutput
import Server.Rules.RuleScript
import Server.Rules.Behavior
import Dynamics.PDynamics
import Dynamics.DynamicsEvent
import Descriptions.MapDescription
import Descriptions.ItemDescription
import Descriptions.AbilityDescription
import Descriptions.ObjectDescription.RulesDescription
import Descriptions.ObjectDescription.RulesDescription.BehaviorDescription
import Objects
import GameIO
import Commands
import JSONCache
import Exception
import Timeline

public export
interface Rules (m : Type -> Type) where
  SRules : Type

  startRules : (preload : PreloadResults) -> ST m Var [add SRules]
  endRules : (rules : Var) -> ST m () [remove rules SRules]

  queryPRules : (rules : Var) -> (q : PRules -> a) -> ST m a [rules ::: SRules]
  updatePRules : (rules : Var) -> (f : PRules -> PRules) -> ST m () [rules ::: SRules]

  addObject : (rules : Var) ->
              (id : ObjectId) ->
              (desc : RulesDescription) ->
              (creator : Maybe ObjectId) ->
              ST m () [rules ::: SRules]

  addCharacter : (rules : Var) ->
                 (id : ObjectId) ->
                 (character_id : CharacterId) ->
                 (character : Character) ->
                 ST m () [rules ::: SRules]

  updateCharacter : (rules : Var) ->
                    (id : ObjectId) ->
                    (f : Character -> Character) ->
                    ST m () [rules ::: SRules]

  removeObject : (rules : Var) ->
                 (id : ObjectId) ->
                 ST m () [rules ::: SRules]

  processDynamicsEvent : (rules : Var) -> DynamicsEvent -> ST m () [rules ::: SRules]
  processDynamicsEvents : (rules : Var) -> List DynamicsEvent -> ST m () [rules ::: SRules]

  getRulesOutputs : (rules : Var) -> ST m (List RulesOutput) [rules ::: SRules]

  runAbility : (rules : Var) ->
               ObjectId ->
               (at : Vector2D) ->
               AbilityDescription ->
               BodyData ->
               ST m () [rules ::: SRules]
  runCommand : (rules : Var) -> Command -> ST m () [rules ::: SRules]
  runCommands : (rules : Var) -> List Command -> ST m () [rules ::: SRules]

  -- private
  -- dataUpdate : (rules : Var) -> Command -> ST m () [rules ::: SRules]
  -- private
  -- dataUpdates : (rules : Var) -> List Command -> ST m () [rules ::: SRules]

  private
  runScript : (rules : Var) -> RuleScript a -> ST m a [rules ::: SRules]
  private
  runUnitScripts : (rules : Var) -> List UnitRuleScript -> ST m () [rules ::: SRules]

export
GameIO m => Rules m where
  SRules = State PRules

  startRules preload = new $ emptyPRules preload
  endRules rules = delete rules

  queryPRules rules q = read rules >>= pure . q
  updatePRules rules f = update rules f

  addObject rules id desc creator = with ST do
    case behavior desc of
      Nothing => updatePRules rules $ prulesAddObject id desc Nothing creator
      Just behavior_params => with ST do
        preload' <- queryPRules rules preload
        case getBehaviorDescription (ref behavior_params) preload' of
          Left e => lift $ log $ "couldn't get behavior description " ++ ref behavior_params
          Right behavior_desc =>
            let for_controller = Just (behavior_desc, behavior_params)
                in updatePRules rules $ prulesAddObject id desc for_controller creator

  addCharacter rules id character_id character =
    updatePRules rules $ prulesAddCharacter id character_id character

  updateCharacter rules id f = with ST do
    Just (character_id, character) <- queryPRules rules $ prulesGetCharacter id
    updatePRules rules $ prulesUpdateCharacter id f
    updatePRules rules $ output (UpdateCharacter character_id f)

  removeObject rules id = updatePRules rules $ prulesRemoveObject id

  runAbility rules id at (Throw ref impulse) body
    = let thrower_position = position body
          direction = normed (at - thrower_position)
          from = thrower_position + (2 `scale` direction)
          impulse' = impulse `scale` direction
          angle' = angle direction - pi/2.0
          creation = MkCreation
            ref from (Just impulse') (Just id) (Just angle') Nothing Nothing
          in updatePRules rules $ output (Create creation)

  runCommand rules (Stop (Attack at) id) = runScript rules $ attackScript id at
  runCommand rules _ = pure ()

  runCommands rules [] = pure ()
  runCommands rules (cmd::xs) = runCommand rules cmd >>= const (runCommands rules xs)

  runScript rules (Output rules_output)
    = updatePRules rules $ output rules_output
  runScript rules (Transition id state scripts) = with ST do
    updatePRules rules $ prulesUpdateController id $ transition !ticks state
    runUnitScripts rules scripts
  runScript rules (UpdateData id f)
    = updatePRules rules $ prulesUpdateController id $ updateData f
  runScript rules (GetItemDescription ref) = with ST do
    preload' <- queryPRules rules preload
    pure $ getItemDescription ref preload'
  runScript rules (GetAttack id) =  queryPRules rules $ getAttack id
  runScript rules (Ability id at desc) = with ST do
    Just body <- queryPRules rules $ getBody id | pure ()
    runAbility rules id at desc body
  runScript rules (GetCharacter id) = queryPRules rules $ prulesGetCharacter' id
  runScript rules (UpdateCharacter id f) = updateCharacter rules id f
  runScript rules (GetStartTime id) = queryPRules rules $ getStartTime id
  runScript rules (GetDirection id) = queryPRules rules $ getDirection id
  runScript rules (GetController id) = queryPRules rules $ getController id
  runScript rules (GetPosition id) = queryPRules rules $ getPosition id
  runScript rules (UpdateNumericProperty for id f)
    = updatePRules rules $ prulesUpdateNumProp for id f
  runScript rules (QueryNumericProperty for id q)
    = queryPRules rules $ prulesQueryNumProp for id q
  runScript rules (GetStat for name) = queryPRules rules $ getStat for name
  runScript rules (GetCreator for) = queryPRules rules $ getCreator for
  runScript rules GetTime = ticks
  runScript rules (Log x) = lift $ log x
  runScript rules (Pure res) = pure res
  runScript rules (x >>= f) = runScript rules x >>= runScript rules . f

  runUnitScripts rules [] = pure ()
  runUnitScripts rules (script::xs)
    = runScript rules script >>= const (runUnitScripts rules xs)

  processDynamicsEvent rules event = runScript rules (dynamicsEventScript event)

  processDynamicsEvents rules [] = pure ()
  processDynamicsEvents rules (event::xs)
    = processDynamicsEvent rules event >>= const (processDynamicsEvents rules xs)

  getRulesOutputs rules = with ST do
    let mainScript' = mainScript !ticks
    controllers <- queryPRules rules controllerIds
    runUnitScripts rules $ map mainScript' $ controllers
    output <- queryPRules rules rulesOutput
    update rules flushRulesOutput
    pure output
