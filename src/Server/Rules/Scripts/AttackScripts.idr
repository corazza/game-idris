module Server.Rules.Scripts.AttackScripts

import Physics.Box2D

import Server.Rules.RuleScript
import Server.Rules.RulesOutput
import Server.Rules.Scripts.Basics
import Server.Rules.Scripts.MovementScripts
import Server.Rules.Scripts.AbilityScripts
import Descriptions.AbilityDescription
import Descriptions.ItemDescription
import Objects
import Exception

export
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
