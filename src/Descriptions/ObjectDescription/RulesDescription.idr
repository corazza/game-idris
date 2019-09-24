module Descriptions.ObjectDescription.RulesDescription

import Data.AVL.Dict

import GameIO
import Exception
import Objects
import Descriptions.AbilityDescription
import Timeline.Items
import Serializer

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
data RulesType = Animate | Inanimate

rulesTypePicker : Dict String RulesType
rulesTypePicker = fromList [
  ("animate", Animate),
  ("inanimate", Inanimate)
]

rulesTypeToString : RulesType -> String
rulesTypeToString Animate = "animate"
rulesTypeToString Inanimate = "inanimate"

getRulesType : JSONDict -> Checked RulesType
getRulesType dict = case hasKey "rulesType" dict of
  False => pure Inanimate
  True => getString "rulesType" dict >>= flip (pick "rulesType") rulesTypePicker

getRulesTypeMaybe : JSONDict -> Checked (Maybe RulesType)
getRulesTypeMaybe dict = with Checked do
  type <- getStringMaybe "rulesType" dict
  case type of
    Nothing => pure Nothing
    Just str => pick "rulesType" str rulesTypePicker >>= pure . Just

public export
record BehaviorParameters where
  constructor MkBehaviorParameters
  ref : ContentReference
  intParameters : Dict String Int
  doubleParameters : Dict String Double
  stringlistParameters : Dict String (List String)
  stringParameters : Dict String String
  logTransitions : Bool
  rulesType : Maybe RulesType
%name BehaviorParameters behavior_params

export
Show BehaviorParameters where
  show bp
    =  "{ ref: " ++ ref bp
    ++ ", intParameters: " ++ show (intParameters bp)
    ++ ", doubleParameters: " ++ show (doubleParameters bp)
    ++ ", stringlistParameters: " ++ show (stringlistParameters bp)
    ++ ", stringParameters: " ++ show (stringParameters bp)
    ++ ", logTransitions: " ++ show (logTransitions bp)
    ++ " }"

allToInt : (String, JSON) -> Checked (String, Int)
allToInt (name, JNumber x) = pure (name, cast x)
allToInt (name, _) = fail $ name ++ " not a number"

allToDouble : (String, JSON) -> Checked (String, Double)
allToDouble (name, JNumber x) = pure (name, x)
allToDouble (name, _) = fail $ name ++ " not a number"

allToString : (String, JSON) -> Checked (String, String)
allToString (name, JString x) = pure (name, x)
allToString (name, _) = fail $ name ++ " not a string"

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

export
ObjectCaster BehaviorParameters where
  objectCast dict = with Checked do
    ref <- getString "ref" dict
    intParameters <- getParameterType "int_parameters" allToInt dict
    doubleParameters <- getParameterType "double_parameters" allToDouble dict
    stringlistParameters <- getParameterType "stringlist_parameters" allToStringlist dict
    stringParameters <- getParameterType "string_parameters" allToString dict
    logTransitions <- getBoolOrDefault False "logTransitions" dict
    rulesType <- getRulesTypeMaybe dict
    pure $ MkBehaviorParameters ref intParameters doubleParameters
                stringlistParameters stringParameters logTransitions rulesType

export
Serialize BehaviorParameters where
  toDict bp = with ST do
    bpObject <- makeObject
    addString bpObject "ref" $ ref bp
    addIntDict bpObject "int_parameters" $ intParameters bp
    addDoubleDict bpObject "double_parameters" $ doubleParameters bp
    addStringlistDict bpObject "stringlist_parameters" $ stringlistParameters bp
    addStringDict bpObject "string_parameters" $ stringParameters bp
    addBool bpObject "logTransitions" $ logTransitions bp
    addStringMaybe bpObject "rulesType" $ map rulesTypeToString $ rulesType bp
    getDict bpObject

public export
record RulesDescription where
  constructor MkRulesDescription
  numericProperties : Maybe NumPropDescDict
  stats : Maybe StatsDict
  attack : Maybe AbilityDescription
  behavior : Maybe BehaviorParameters
  items : Maybe Items
  rulesType : RulesType

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
    items <- the (Checked (Maybe Items)) $ getCastableMaybe "items" dict
    rulesType <- getRulesType dict
    pure $ MkRulesDescription numericProperties stats attack behavior items rulesType
