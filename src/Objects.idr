module Objects

import Graphics.SDL2
import Data.AVL.Dict

import Vector2D
import Physics

%access public export

record Object where
  constructor MkObject
  id : String
  position : Vector2D
  boxDescription : BoxDescription
  texture : Texture

%name Object object

ObjectDict : Type
ObjectDict = Dict String Object
%name ObjectDict objectDict

export
w : Object -> Double
w = fst . dim . boxDescription

export
h : Object -> Double
h = snd . dim . boxDescription

export
x : Object -> Double
x = fst . position

export
y : Object -> Double
y = snd . position
