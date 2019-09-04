module Descriptions.FontDescription

import GameIO
import Exception
import Descriptions.Color

public export
record FontDescription where
  constructor MkFontDescription
  ref : ContentReference -- ref to the actual font file TTF has to load
  size : Int
  color : Color

export
ObjectCaster FontDescription where
  objectCast dict = with Checked do
    ref <- getString "ref" dict
    size <- getInt "size" dict
    color <- getColor "color" dict
    pure $ MkFontDescription ref size color
