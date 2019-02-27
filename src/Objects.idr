module Objects

import Graphics.SDL2
import Data.AVL.Dict

%access public export

record Object where
  constructor MkObject
  id : String
  x, y : Int
  dx, dy : Int
  w, h : Int
  texture : Texture

%name Object object

ObjectDict : Type
ObjectDict = Dict String Object
%name ObjectDict objectDict
