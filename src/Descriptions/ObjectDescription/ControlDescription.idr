module Descriptions.ObjectDescription.ControlDescription

import GameIO
import Exception

public export
record AIParameters where
  constructor MkAIParameters
  intParameters : Dict String Int
  doubleParameters : Dict String Double
  stringlistParameters : Dict String (List String)

export
Show AIParameters where
  show ai_parameters
    =  "{ int_parameters: " ++ show (intParameters ai_parameters) ++ ", "
    ++ "{ double_parameters: " ++ show (doubleParameters ai_parameters) ++ ", "
    ++ "{ stringlist_parameters:" ++ show (stringlistParameters ai_parameters) ++ " }"

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

export
ObjectCaster AIParameters where
  objectCast dict = with Checked do
    intParameters <- getParameterType "int_parameters" allToInt dict
    doubleParameters <- getParameterType "double_parameters" allToDouble dict
    stringlistParameters <- getParameterType "stringlist_parameters" allToStringlist dict
    pure $ MkAIParameters intParameters doubleParameters stringlistParameters

public export
record ControlDescription where
  constructor MkControlDescription
  speed : Double
  jump : Double
  ai : Maybe (ContentReference, Maybe AIParameters)

export
Show ControlDescription where
  show control_description
    =  "{ speed: " ++ show (speed control_description) ++ ", "
    ++ "{ jump: " ++ show (jump control_description) ++ ", "
    ++ "{ ai: " ++ show (ai control_description) ++ " }"

export
ObjectCaster ControlDescription where
  objectCast dict = with Checked do
    speed <- getDouble "speed" dict
    jump <- getDouble "jump" dict
    case hasKey "ai" dict of
      False => pure $ MkControlDescription speed jump Nothing
      True => with Checked do
        ai_ref <- getString "ai" dict
        let takesAI = MkControlDescription speed jump
        case hasKey "ai_parameters" dict of
          False => pure $ takesAI $ Just (ai_ref, Nothing)
          True => with ST do
            ai_parameters <- the (Checked AIParameters) $ getCastable "ai_parameters" dict
            pure $ takesAI $ Just (ai_ref, Just ai_parameters)
