module Server.Rules.RulesData

import Server.Rules.NumericProperties
import Server.Rules.Behavior
import Timeline
import Timeline.Items
import Descriptions.ObjectDescription.RulesDescription
import Descriptions.ItemDescription
import Objects
import GameIO

public export
record RulesData where
  constructor MkRulesData
  numericProperties : Maybe NumericPropertyDict
  stats : Maybe StatsDict
  creator : Maybe ObjectId
  controller : Maybe BehaviorController
  items : Maybe Items
  meleeSound : Maybe ContentReference
  rulesType : RulesType

export
setItems : Items -> RulesData -> RulesData
setItems items' = record { items = Just items' }

export
setMeleeSound : Maybe ContentReference -> RulesData -> RulesData
setMeleeSound ref = record { meleeSound = ref }

export
makeData : (desc : RulesDescription) ->
           (controller : Maybe BehaviorController) ->
           (creator : Maybe ObjectId) ->
           RulesData
makeData desc controller creator = MkRulesData
  (numPropDictFromDescription desc) (stats desc) creator controller
  (items desc) Nothing rulesType' where
    rulesType' : RulesType
    rulesType' = case map rulesType (behavior desc) of
      Just (Just x) => x
      _ => rulesType desc

export
updateNumPropInData : NumericPropertyId ->
                      (f : NumericProperty -> NumericProperty) ->
                      RulesData -> RulesData
updateNumPropInData id f
  = record { numericProperties $= map (updateNumericProperty id f) }

export
queryNumPropInData : NumericPropertyId ->
                     (q : NumericProperty -> a) ->
                     RulesData -> Maybe a
queryNumPropInData id q rules_data
  = numericProperties rules_data >>= (queryNumericProperty id q)

export
updateControllerInData : (f : BehaviorController -> BehaviorController) ->
                         RulesData -> RulesData
updateControllerInData f = record { controller $= map f }

export
hasController : RulesData -> Bool
hasController = isJust . controller

export
toData : (q : BehaviorController -> a) -> (RulesData -> Maybe a)
toData q = map q . controller

export
dataGetStringParameter : (name : String) -> RulesData -> Maybe String
dataGetStringParameter name = join . map (getStringParameter name) . controller
