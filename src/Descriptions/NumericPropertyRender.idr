module Descriptions.NumericPropertyRender

import Descriptions.Color
import GameIO
import Exception

public export
record NumPropRenderDescription where
  constructor MkNumPropRenderDescription
  name : String
  fullWidth : Double
  height : Double
  yd : Double
  color : Color

ObjectCaster NumPropRenderDescription where
  objectCast dict = with Checked do
    name <- getString "name" dict
    fullWidth <- getDouble "fullWidth" dict
    height <- getDouble "height" dict
    yd <- getDouble "yd" dict
    color <- getColor "color" dict
    pure $ MkNumPropRenderDescription name fullWidth height yd color

public export
NumPropRenderDescriptionDict : Type
NumPropRenderDescriptionDict = Dict String NumPropRenderDescription

getPropRender : (String, JSON) -> Checked (String, NumPropRenderDescription)
getPropRender (name, json) = with Checked do
  render <- the (Checked NumPropRenderDescription) $ cast json
  pure (name, render)

export
ObjectCaster NumPropRenderDescriptionDict where
  objectCast dict = (catResults $ map getPropRender (toList dict)) >>= pure . fromList
