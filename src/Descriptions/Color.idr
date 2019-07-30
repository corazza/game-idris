module Descriptions.Color

import GameIO
import Exception

public export
data Color = MkColor Int Int Int Int
%name Color color

export
Show Color where
  show (MkColor r g b a)
    = "rgba(" ++ show r ++ ", " ++ show g ++ ", " ++ show b ++ ", " ++ show a ++ ")"

export
getColor : (name : String) -> (dict : JSONDict) -> Checked Color
getColor name dict = with Checked do
  JArray [JNumber r, JNumber g, JNumber b, JNumber a] <-
    maybeToEither ("color \"" ++ name ++ "\" inexistent") (lookup name dict)
                  | fail "color format fail (must be [r, g, b])"
  pure $ MkColor (cast r) (cast g) (cast b) (cast a)

export
addColor : (object : Var) ->
           (key : String) ->
           (value : Color) ->
           ST Identity () [object ::: SKind JSONOBject {m=Identity}]
addColor object key (MkColor r g b a) = with ST do
  colorArray <- makeArray
  appendDouble colorArray $ cast r
  appendDouble colorArray $ cast g
  appendDouble colorArray $ cast b
  appendDouble colorArray $ cast a
  colorArray' <- getArray colorArray
  addArray object key colorArray'
