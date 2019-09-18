module Server.Rules.Scripts.Damage

import Server.Rules.RuleScript
import Server.Rules.Behavior
import Server.Rules.RulesOutput
import Server.Rules.RulesData
import Server.Rules.NumericProperties
import Server.Rules.Scripts.Basics
import Objects
import Commands

export
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
