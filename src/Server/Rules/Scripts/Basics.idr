module Server.Rules.Scripts.Basics

import Server.Rules.RuleScript
import Server.Rules.RulesData
import Server.Rules.Behavior
import Server.Rules.RulesOutput
import Descriptions.ObjectDescription.RulesDescription
import Objects

export
isAnimate : ObjectId -> RuleScript Bool
isAnimate id = case !(QueryData id rulesType) of
  Nothing => pure False
  Just Animate => pure True
  Just Inanimate => pure False

export
isInanimate : ObjectId -> RuleScript Bool
isInanimate id = case !(QueryData id rulesType) of
  Nothing => pure False
  Just Animate => pure False
  Just Inanimate => pure True

export
playConditional : (id : ObjectId) -> (sound : Maybe ContentReference) -> UnitRuleScript
playConditional id Nothing = pure ()
playConditional id ref = UpdateData id $ setMeleeSound ref

export
playConditional' : Maybe ContentReference -> UnitRuleScript
playConditional' Nothing = pure ()
playConditional' (Just ref) = Output $ PlaySound ref

export
chooseMeleeHitSound : (target : ObjectId) -> RuleScript ContentReference
chooseMeleeHitSound target = case !(QueryData target rulesType) of
  Just Animate => pure "main/sounds/sword/gash.wav"
  Just Inanimate => pure "main/sounds/melee/hit_inanimate.wav"
  _ => pure "main/sounds/melee/hit_inanimate.wav"

export
meleeSoundScript : (id : ObjectId) -> UnitRuleScript
meleeSoundScript id = case !(QueryData id meleeSound) of
  Just (Just ref) => with RuleScript do
    Output $ PlaySound ref
    UpdateData id $ setMeleeSound Nothing
  _ => pure ()

export
garbageScript : (id : ObjectId) -> UnitRuleScript
garbageScript id = with RuleScript do
  Just controller <- GetController id | pure ()
  case halted controller of
    False => pure ()
    True => Output $ Death id
