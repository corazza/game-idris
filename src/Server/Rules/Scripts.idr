module Server.Rules.Scripts

import Dynamics.DynamicsEvent
import Server.Rules.RuleScript
import Server.Rules.Behavior
import Server.Rules.Scripts.BehaviorScripts
import Server.Rules.Scripts.Damage
import Server.Rules.Scripts.Basics
import Objects

export
mainScript : (time : Int) -> (id : ObjectId) -> UnitRuleScript
mainScript time id = with RuleScript do
  timeScript time id
  chaseScript id
  garbageScript id
  meleeSoundScript id
