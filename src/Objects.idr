module Objects

import Graphics.SDL2

%access public export

record Object where
  constructor MkObject
  x, y : Int
  dx, dy : Int
  texture : Texture
