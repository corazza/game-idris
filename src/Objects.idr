module Objects

import Graphics.SDL2
import Data.AVL.Dict

import Physics.Vector2D
import Physics

%access public export

record Object where
  constructor MkObject
  id : String
  position : Vector2D
  boxDescription : BoxDescription
  texture : Texture

%name Object object

export
dim : Object -> Vector2D
dim = dim . boxDescription

export
w : Object -> Double
w = fst . dim

export
h : Object -> Double
h = snd . dim

export
x : Object -> Double
x = fst . position

export
y : Object -> Double
y = snd . position
