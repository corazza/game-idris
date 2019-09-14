module Server.Rules.PRules

import Server.Rules.NumericProperties
import Server.Rules.Behavior
import Server.Rules.RulesData
import Server.Rules.RulesOutput
import Objects
import GameIO
import Commands
import JSONCache
import Descriptions.AbilityDescription
import Descriptions.ObjectDescription.RulesDescription
import Descriptions.ObjectDescription.RulesDescription.BehaviorDescription
import Dynamics.BodyData
import Timeline
import Timeline.Items

public export
record PRules where
  constructor MkPRules
  preload : PreloadResults
  objects : Objects RulesData
  characters : Objects (CharacterId, Character)
  bodyData : Objects BodyData
  rulesOutput : List RulesOutput

export
emptyPRules : PreloadResults -> PRules
emptyPRules preload = MkPRules preload empty empty empty empty

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
prulesAddCharacter : ObjectId -> CharacterId -> Character -> PRules -> PRules
prulesAddCharacter id character_id character
  = record { characters $= addObject id (character_id, character),
             objects $= updateObject id (setItems (items character)) }

export
prulesGetCharacter : ObjectId -> PRules -> Maybe (CharacterId, Character)
prulesGetCharacter id = lookup id . characters

export
prulesGetCharacter' : ObjectId -> PRules -> Maybe Character
prulesGetCharacter' id = map snd . lookup id . characters

export
prulesUpdateCharacter : ObjectId -> (f : Character -> Character) -> PRules -> PRules
prulesUpdateCharacter id f
  = record { characters $= updateObject id (\(character_id, character) => (character_id, f character)) }

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
prulesUpdateItems : (id : ObjectId) ->
                    (f : Items -> Items) ->
                    PRules -> PRules
prulesUpdateItems id f = prulesUpdateObject id (itemsToData f) where
  itemsToData : (f : Items -> Items) -> (RulesData -> RulesData)
  itemsToData f = record { items $= map f }

export
prulesQueryObject : (id : ObjectId) ->
                    (q : RulesData -> a) ->
                    PRules -> Maybe a
prulesQueryObject id q prules = lookup id (objects prules) >>= pure . q

export
prulesQueryItems : (id : ObjectId) ->
                   (q : Items -> a) ->
                   PRules -> Maybe a
prulesQueryItems id q = join . prulesQueryObject id (itemsToData q) where
  itemsToData : (q : Items -> a) -> (RulesData -> Maybe a)
  itemsToData q = map q . items

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
getAttack : (for : ObjectId) -> PRules -> Maybe ContentReference
getAttack for prules = prulesQueryObject for items prules >>= join . map attackItem

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
