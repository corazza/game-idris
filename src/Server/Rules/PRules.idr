module Server.Rules.PRules

import Server.Rules.NumericProperties
import Server.Rules.Behavior
import Server.Rules.RulesOutput
import Objects
import GameIO
import Commands
import JSONCache
import Descriptions.ObjectDescription.RulesDescription
import Descriptions.ObjectDescription.RulesDescription.BehaviorDescription
import Dynamics.PDynamics

public export
record RulesData where
  constructor MkRulesData
  numericProperties : Maybe NumericPropertyDict
  stats : Maybe StatsDict
  attack : Maybe AbilityDescription
  creator : Maybe ObjectId
  controller : Maybe BehaviorController

makeData : (desc : RulesDescription) ->
           (controller : Maybe BehaviorController) ->
           (creator : Maybe ObjectId) ->
           RulesData
makeData desc controller creator = MkRulesData
  (numPropDictFromDescription desc) (stats desc) (attack desc) creator controller

updateNumPropInData : NumericPropertyId ->
                      (f : NumericProperty -> NumericProperty) ->
                      RulesData -> RulesData
updateNumPropInData id f
  = record { numericProperties $= map (updateNumericProperty id f) }

queryNumPropInData : NumericPropertyId ->
                     (q : NumericProperty -> a) ->
                     RulesData -> Maybe a
queryNumPropInData id q rules_data
  = numericProperties rules_data >>= (queryNumericProperty id q)

updateControllerInData : (f : BehaviorController -> BehaviorController) ->
                         RulesData -> RulesData
updateControllerInData f = record { controller $= map f }

hasController : RulesData -> Bool
hasController = isJust . controller

public export
record PRules where
  constructor MkPRules
  preload : PreloadResults
  objects : Objects RulesData
  bodyData : Objects BodyData
  rulesOutput : List RulesOutput

export
emptyPRules : PreloadResults -> PRules
emptyPRules preload = MkPRules preload empty empty empty

export
prulesSetBodyData : Objects BodyData -> PRules -> PRules
prulesSetBodyData bodyData' = record { bodyData = bodyData' }

export
prulesAddObject : ObjectId ->
                  (rules_desc : RulesDescription) ->
                  Maybe (BehaviorDescription, BehaviorParameters) ->
                  (creator : Maybe ObjectId) ->
                  PRules -> PRules
prulesAddObject id rules_desc for_controller creator
  = let object = makeData rules_desc controller creator
        in record { objects $= addObject id object } where
          controller : Maybe BehaviorController
          controller = map (uncurry fromDescriptorParameters) for_controller

export
prulesRemoveObject : ObjectId -> PRules -> PRules
prulesRemoveObject id = record { objects $= removeObject id }

export
prulesUpdateObject : (id : ObjectId) ->
                     (f : RulesData -> RulesData) ->
                     PRules -> PRules
prulesUpdateObject id f
  = record { objects $= updateObject id f }

export
prulesQueryObject : (id : ObjectId) ->
                    (q : RulesData -> a) ->
                    PRules -> Maybe a
prulesQueryObject id q prules = lookup id (objects prules) >>= pure . q

export
getController : ObjectId -> PRules -> Maybe BehaviorController
getController id prules = lookup id (objects prules) >>= controller

export
controllerIds : PRules -> List ObjectId
controllerIds prules = map fst $ filter (hasController . snd) $ toList $ objects prules

export
getDirection : ObjectId -> PRules -> Maybe BehaviorDirection
getDirection id prules = getController id prules >>= direction . behavior_data

export
getStartTime : ObjectId -> PRules -> Maybe Int
getStartTime id prules = getController id prules >>= pure . transitioned

export
getBody : ObjectId -> PRules -> Maybe BodyData
getBody id prules = lookup id (bodyData prules)

export
getPosition : ObjectId -> PRules -> Maybe Vector2D
getPosition id prules = getBody id prules >>= pure . position

export
getCreator : (for : ObjectId) -> PRules -> Maybe ObjectId
getCreator for = join . prulesQueryObject for creator

export
getAttack : (for : ObjectId) -> PRules -> Maybe AbilityDescription
getAttack for = join . prulesQueryObject for attack

export
getStats : (for : ObjectId) -> PRules -> Maybe StatsDict
getStats for = join . prulesQueryObject for stats

export
getStat : (for : ObjectId) -> (name : StatId) -> PRules -> Maybe Double
getStat for name prules = getStats for prules >>= lookup name

export
prulesUpdateNumProp : ObjectId ->
                      NumericPropertyId ->
                      (f : NumericProperty -> NumericProperty) ->
                      PRules -> PRules
prulesUpdateNumProp object_id prop_id f
  = prulesUpdateObject object_id (updateNumPropInData prop_id f)

export
prulesQueryNumProp : ObjectId ->
                     NumericPropertyId ->
                     (q : NumericProperty -> a) ->
                     PRules -> Maybe a
prulesQueryNumProp object_id prop_id q
  = join . prulesQueryObject object_id (queryNumPropInData prop_id q)

export
prulesUpdateController : ObjectId ->
                         (f : BehaviorController -> BehaviorController) ->
                         PRules -> PRules
prulesUpdateController id f = prulesUpdateObject id $ updateControllerInData f


commandToDataUpdate : Command -> BehaviorData -> BehaviorData
commandToDataUpdate (Start (Movement Left) _) = record { direction = Just Leftward }
commandToDataUpdate (Start (Movement Right) _) = record { direction = Just Rightward }
commandToDataUpdate _ = id

commandInternal : RulesOutput -> PRules -> PRules
commandInternal (RuleCommand command)
  = let id = getId command
        f = updateData (commandToDataUpdate command)
        in prulesUpdateController id f
commandInternal _ = id
-- internalOutput (Death x) = id
-- internalOutput (NumericPropertyCurrent x y z) = id

export
output : RulesOutput -> PRules -> PRules
output rules_output
  = record { rulesOutput $= append rules_output}
  . commandInternal rules_output

export
flushRulesOutput : PRules -> PRules
flushRulesOutput = record { rulesOutput = empty }
