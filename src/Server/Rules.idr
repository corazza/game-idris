module Server.Rules

import Control.ST
import Control.ST.ImplicitCall
import Physics.Box2D
import Data.AVL.Set

import Server.Rules.PRules
import Server.Rules.NumericProperties
import Server.Rules.RulesOutput
import Server.Rules.RuleScript
import Server.Rules.Scripts
import Server.Rules.Scripts.BehaviorScripts
import Server.Rules.Scripts.AttackScripts
import Server.Rules.Scripts.ItemsScripts
import Server.Rules.RulesData
import Server.Rules.Behavior
import Client.ClientCommands
import Dynamics.BodyData
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
import Timeline.Items

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

  removeObject : (rules : Var) ->
                 (id : ObjectId) ->
                 ST m () [rules ::: SRules]

  processDynamicsEvent : (rules : Var) -> DynamicsEvent -> ST m () [rules ::: SRules]
  processDynamicsEvents : (rules : Var) -> List DynamicsEvent -> ST m () [rules ::: SRules]

  getRulesOutputs : (rules : Var) -> ST m (List RulesOutput) [rules ::: SRules]

  runCommand : (rules : Var) -> Command -> ST m () [rules ::: SRules]
  runCommands : (rules : Var) -> List Command -> ST m () [rules ::: SRules]

  setLogTransitions : (rules : Var) -> ObjectId -> ST m () [rules ::: SRules]
  unsetLogTransitions : (rules : Var) -> ObjectId -> ST m () [rules ::: SRules]

  private
  queryCharacter : (rules : Var) ->
                   (id : ObjectId) ->
                   (q : Character -> a) ->
                   ST m a [rules ::: SRules]

  private
  updateCharacter : (rules : Var) ->
                    (id : ObjectId) ->
                    (f : Character -> Character) ->
                    ST m () [rules ::: SRules]

  private
  clientCommand : (rules : Var) ->
                  (id : ObjectId) ->
                  (cmd : ClientCommand) ->
                  ST m () [rules ::: SRules]

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
                in with ST do
                  updatePRules rules $ prulesAddObject id desc for_controller creator
                  case logTransitions behavior_params of
                    False => pure ()
                    True => setLogTransitions rules id

  addCharacter rules id character_id character =
    updatePRules rules $ prulesAddCharacter id character_id character

  queryCharacter rules id q = with ST do
    Just (character_id, character) <- queryPRules rules $ prulesGetCharacter id
    pure $ q character

  updateCharacter rules id f = with ST do
    Just (character_id, character) <- queryPRules rules $ prulesGetCharacter id
    updatePRules rules $ prulesUpdateCharacter id f
    updatePRules rules $ output (UpdateCharacter character_id f)

  clientCommand rules id cmd = with ST do
    Just (character_id, character) <- queryPRules rules $ prulesGetCharacter id
    updatePRules rules $ output (RulesClientCommand character_id cmd)

  removeObject rules id = updatePRules rules $ prulesRemoveObject id

  runCommand rules (Start (Attack at) id) = runScript rules $ beginAttackScript id at
  runCommand rules (Stop (Attack at) id) = runScript rules $ endAttackScript id at
  runCommand rules (Equip ref id) = runScript rules $ equipScript id ref
  runCommand rules (Unequip ref id) = runScript rules $ unequipScript id ref
  runCommand rules _ = pure ()

  runCommands rules [] = pure ()
  runCommands rules (cmd::xs) = runCommand rules cmd >>= const (runCommands rules xs)

  setLogTransitions rules id = updatePRules rules $ prulesSetLogTransitions id
  unsetLogTransitions rules id = updatePRules rules $ prulesUnsetLogTransitions id

  runScript rules (Output rules_output)
    = updatePRules rules $ output rules_output
  runScript rules t@(Transition id state scripts) = with ST do
    updatePRules rules $ prulesUpdateController id $ transition !ticks state
    runUnitScripts rules scripts
    logging <- queryPRules rules loggingTransitions
    case contains id logging of
      False => pure ()
      True => lift $ log $ id ++ " transitioning to " ++ state
  runScript rules (UpdateBehaviorData id f)
    = updatePRules rules $ prulesUpdateController id $ updateData f
  runScript rules (QueryBehaviorData id q)
    = queryPRules rules $ prulesQueryController id $ queryData q
  runScript rules (GetItemDescription ref) = with ST do
    preload' <- queryPRules rules preload
    pure $ getItemDescription ref preload'
  runScript rules (GetAttack id) = queryPRules rules $ getAttack id
  runScript rules (GetBody id) = queryPRules rules $ getBody id
  runScript rules (QueryBody id q) = queryPRules rules $ prulesQueryBody id q
  runScript rules (RulesClientCommand id cmd) = clientCommand rules id cmd
  runScript rules (GetStartTime id) = queryPRules rules $ getStartTime id
  runScript rules (GetDirection id) = queryPRules rules $ getDirection id
  runScript rules (GetController id) = queryPRules rules $ getController id
  runScript rules (GetPosition id) = queryPRules rules $ getPosition id
  runScript rules (QueryData id q) = queryPRules rules $ prulesQueryObject id q
  runScript rules (UpdateData id f) = updatePRules rules $ prulesUpdateObject id f
  runScript rules (QueryItems id q) = queryPRules rules $ prulesQueryItems id q
  runScript rules (UpdateItems id f) = with ST do
    updatePRules rules $ prulesUpdateItems id f
    characters' <- queryPRules rules characters
    case hasKey id characters' of
      False => pure ()
      True => updateCharacter rules id (itemsToCharacter f)
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
    object_ids <- queryPRules rules objectIds
    runUnitScripts rules $ map mainScript' $ object_ids
    output <- queryPRules rules rulesOutput
    update rules flushRulesOutput
    pure output
