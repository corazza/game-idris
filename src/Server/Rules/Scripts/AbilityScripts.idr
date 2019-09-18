module Server.Rules.Scripts.AbilityScripts

import Physics.Box2D

import Server.Rules.RuleScript
import Server.Rules.RulesOutput
import Server.Rules.Scripts.MovementScripts
import Dynamics.BodyData
import Dynamics.MoveDirection
import Descriptions.AbilityDescription
import Descriptions.ObjectDescription.RulesDescription
import Descriptions.ObjectDescription.RenderDescription
import Descriptions.MapDescription
import Objects

export
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

export
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
