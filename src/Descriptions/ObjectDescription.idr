module Descriptions.ObjectDescription

import Descriptions.ObjectDescription.BodyDescription
import Descriptions.ObjectDescription.RenderDescription
import Descriptions.ObjectDescription.ControlDescription
import Descriptions.ObjectDescription.RulesDescription
import GameIO
import Exception

public export
record ObjectDescription where
  constructor MkObjectDescription
  name : String
  body : BodyDescription
  render : RenderDescription
  control : Maybe ControlDescription
  rules : Maybe RulesDescription
  -- rules : Maybe RulesDescription
%name ObjectDescription object_description

export
Show ObjectDescription where
  show (MkObjectDescription name body render control rules)
    =  "{ name: " ++ name
    ++ ", body: " ++ show body
    ++ ", render: " ++ show render
    ++ ", control: " ++ show control
    -- TODO MISSING rules
    ++ " }"

export
ObjectCaster ObjectDescription where
  objectCast dict = with Checked do
    name <- getString "name" dict
    body <- the (Checked BodyDescription) $ getCastable "body" dict
    render <- the (Checked RenderDescription) $ getCastable "render" dict
    control <- the (Checked (Maybe ControlDescription)) $ getCastableMaybe "control" dict
    rules <- the (Checked (Maybe RulesDescription)) $ getCastableMaybe "rules" dict
    pure $ MkObjectDescription name body render control rules