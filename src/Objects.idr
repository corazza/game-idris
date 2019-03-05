module Objects

import Graphics.SDL2
import Data.AVL.Dict

import Physics.Vector2D

%access public export


data BoxType = Static | Dynamic

record BoxDescription where
  constructor MkBoxDescription
  mass : Double
  type : BoxType
  dim : Vector2D

record Object where
  constructor MkObject
  id : String
  position : Vector2D
  angle : Double
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
