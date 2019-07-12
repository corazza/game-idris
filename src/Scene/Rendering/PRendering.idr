module Scene.Rendering.PRendering

import Control.ST
import Control.ST.ImplicitCall

import Scene.Rendering.Camera
import Scene.Dynamics
import SDL
import GameIO
import Objects

public export
data RenderMethod = Invisible
                  | Tiled ResourceReference
                  | Single ResourceReference
                  | Animated ResourceReference Int -- started

public export
record PRendering where
  constructor MkPRendering
  background : ResourceReference
  layers : List (Objects RenderMethod)
  camera : Camera
