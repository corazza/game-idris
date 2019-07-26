module Descriptions.ObjectDescription.RulesDescription

import Data.AVL.Dict

import GameIO
import Exception
import Objects

public export
record NumericPropertyDescription where
  constructor MkNumericPropertyDescription
  full : Double

ObjectCaster NumericPropertyDescription where
  objectCast dict = with Checked do
    full <- getDouble "full" dict
    pure $ MkNumericPropertyDescription full

public export
NumPropDescDict : Type
NumPropDescDict = Dict NumericPropertyId NumericPropertyDescription

public export
StatsDict : Type
StatsDict = Dict StatId Double

public export
data AbilityDescription
  = Throw ContentReference Double -- ref, impulse

ObjectCaster AbilityDescription where
  objectCast dict = with Checked do
    type <- getString "type" dict
    case type of
      "throw" => with Checked do
        ref <- getString "ref" dict
        impulse <- getDouble "impulse" dict
        pure $ Throw ref impulse
      _ => fail "ability type must be of \"throw\""

public export
record BehaviorParameters where
  constructor MkBehaviorParameters
  ref : ContentReference
  intParameters : Dict String Int
  doubleParameters : Dict String Double
  stringlistParameters : Dict String (List String)
%name BehaviorParameters behavior_params

export
Show BehaviorParameters where
  show bp
    =  "{ ref: " ++ ref bp
    ++ ", intParameters: " ++ show (intParameters bp)
    ++ ", doubleParameters: " ++ show (doubleParameters bp)
    ++ ", stringlistParameters: " ++ show (stringlistParameters bp)
    ++ " }"

allToInt : (String, JSON) -> Checked (String, Int)
allToInt (name, JNumber x) = pure (name, cast x)
allToInt (name, _) = fail $ name ++ " not a number"

allToDouble : (String, JSON) -> Checked (String, Double)
allToDouble (name, JNumber x) = pure (name, x)
allToDouble (name, _) = fail $ name ++ " not a number"

allToStringlist : (String, JSON) -> Checked (String, List String)
allToStringlist (name, JArray xs)
  = catResults (map jsonToString xs) >>= pure . MkPair name
allToStringlist (name, _) = fail $ name ++ " not an array"

getParameterType : (name : String) ->
                   (f : (String, JSON) -> Checked (String, a)) ->
                   (dict : JSONDict) ->
                   Checked (Dict String a)
getParameterType name f dict = case lookup name dict of
  Nothing => pure empty
  Just (JObject xs) => catResults (map f xs) >>= pure . fromList
  _ => fail $ name ++ " must be JObject"

ObjectCaster BehaviorParameters where
  objectCast dict = with Checked do
    ref <- getString "ref" dict
    intParameters <- getParameterType "int_parameters" allToInt dict
    doubleParameters <- getParameterType "double_parameters" allToDouble dict
    stringlistParameters <- getParameterType "stringlist_parameters" allToStringlist dict
    pure $ MkBehaviorParameters ref intParameters doubleParameters stringlistParameters

public export
record RulesDescription where
  constructor MkRulesDescription
  numericProperties : Maybe NumPropDescDict
  stats : Maybe StatsDict
  attack : Maybe AbilityDescription
  behavior : Maybe BehaviorParameters

toProperty : (String, JSON) -> Checked (String, NumericPropertyDescription)
toProperty (name, json) = with Checked do
  propDesc <- the (Checked NumericPropertyDescription) $ cast json
  pure $ (name, propDesc)

getNumericProperties : JSONDict -> Checked (Maybe NumPropDescDict)
getNumericProperties dict = case lookup "numericProperties" dict of
  Nothing => pure Nothing
  Just (JObject x) => with Checked do
    propList <- catResults $ map toProperty x
    pure $ Just $ fromList propList
  Just _ => fail "numericProperties must be JObject"

toStat : (String, JSON) -> Checked (String, Double)
toStat (name, json) = case json of
  (JNumber x) => pure (name, x)
  _ => fail "stat must be double"

getStats : JSONDict -> Checked (Maybe StatsDict)
getStats dict = case lookup "stats" dict of
  Nothing => pure Nothing
  Just (JObject x) => with Checked do
    statList <- catResults $ map toStat x
    pure $ Just $ fromList statList
  Just _ => fail "stats must be JObject"

export
ObjectCaster RulesDescription where
  objectCast dict  = with Checked do
    numericProperties <- getNumericProperties dict
    stats <- getStats dict
    attack <- the (Checked (Maybe AbilityDescription)) $ getCastableMaybe "attack" dict
    behavior <- the (Checked (Maybe BehaviorParameters)) $ getCastableMaybe "behavior" dict
    pure $ MkRulesDescription numericProperties stats attack behavior
